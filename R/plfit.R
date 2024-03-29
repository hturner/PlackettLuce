#' PlackettLuce Wrapper for Model-based Recursive Partitioning
#'
#' This is a wrapper around \code{PlackettLuce} as required by
#' \code{\link[partykit]{mob}} for model-based recursive partitioning. It is
#' not intended for general use.
#'
#' @param y a \code{"\link{grouped_rankings}"} object giving the rankings to
#' model.
#' @param x unused.
#' @inheritParams PlackettLuce
#' @inheritParams summaries
#' @param offset unused.
#' @param ... additional arguments passed to \code{PlackettLuce}.
#' @param estfun logical. If \code{TRUE} the empirical estimating functions
#' (score/gradient contributions) are returned.
#' @param object logical. If \code{TRUE} the fitted model is returned.
#' @return a list with elements
#' \item{coefficients}{ model coefficients. }
#' \item{objfun}{ the negative log-likelihood. }
#' \item{estfun}{ if \code{estfun} the empirical estimating functions. }
#' \item{object}{ if \code{object} the fitted model. }
#' @keywords internal
#' @examples
#' # rankings
#' R <- matrix(c(1, 2, 0, 0,
#'               4, 1, 2, 3,
#'               2, 1, 1, 1,
#'               1, 2, 3, 0,
#'               2, 1, 1, 0,
#'               1, 0, 3, 2), nrow = 6, byrow = TRUE)
#' colnames(R) <- c("apple", "banana", "orange", "pear")
#' R <- as.rankings(R)
#'
#' # group rankings into two groups
#' G <- group(R, rep(1:2, 3))
#'
#' # plfit() gives the same results as PlackettLuce()
#' pl <- plfit(G)
#' pl$coefficients
#' -pl$objfun
#'
#' mod <- PlackettLuce(R)
#' coef(mod)
#' logLik(mod)
#' @export
plfit <- function (y, x = NULL, ref = 1L, start = NULL, weights = NULL,
                   offset = NULL, ..., estfun = FALSE, object = FALSE) {
    x <- !(is.null(x) || NCOL(x) == 0L)
    offset <- !is.null(offset)
    if (x || offset)
        warning("unused argument(s): ",
                paste(c("x"[x], "offset"[offset]), collapse = ","))
    dots <- list(...)
    if ("adherence" %in% names(dots) & !"gamma" %in% names(dots)){
        stop("adherence can not be fixed in a Plackett-Luce tree")
    }
    res <- PlackettLuce(y, start = start, weights = weights,
                        na.action = NULL, ...)
    if (estfun) {
        percomp <- estfun.PlackettLuce(res, ref = ref)
        estfun <- rowsum(as.matrix(percomp), attr(y, "index"))
    } else estfun <- NULL
    if (object) {
        # returning in order to compute final vcov etc, so can
        # aggregate equal rankings now, across rankers
        if (is.null(res$gamma)) {
            uniq <- !duplicated(attr(y, "id"))
            g <- match(attr(y, "id"), attr(y, "id")[uniq])
            res$rankings <- structure(res$rankings[uniq,],
                                      class = "rankings")
            res$weights <- c(tapply(res$weights, g, sum))
        }
    }
    list(coefficients = coef(res, ref = ref),
         ties = res$ties,
         objfun = -res$loglik,
         estfun = estfun,
         object = if (object) res else NULL)
}

# log-likelihood derivatives (score function) per ranking
#' @method estfun PlackettLuce
#' @importFrom sandwich estfun
#' @export
estfun.PlackettLuce <- function(x, ref = 1L, ...) {
    d <- x$ties
    D <- length(d)
    # get coefficients (unconstrained)
    coef <- x$coefficients
    N <- length(coef) - D + 1L
    alpha <- coef[1L:N]
    delta <- c(1.0, coef[-c(1L:N)])
    # get free choices and alternatives for each ranking
    choices <- choices(x$rankings, names = FALSE)
    free <- lengths(choices$alternatives) != 1L
    choices <- lapply(choices, `[`,  free)
    # derivative wrt to log alpha part 1: 1/(size of selected set)
    nr <- nrow(x$rankings)
    A <- matrix(0.0, nrow = nr, ncol = N,
                dimnames = list(NULL, names(alpha)))
    choices$choices <- split(choices$choices, choices$ranking)
    for (i in seq_len(nr)){
        r <- choices$choices[[i]]
        len <- lengths(r)
        if (!is.null(x$adherence)){
            A[i, unlist(r)] <- rep(x$adherence[x$ranker[i]]/len, len)
        } else A[i, unlist(r)] <- rep(1L/len, len)
    }
    # derivative wrt delta part 1: row TRUE where selected set has cardinality d
    if (D > 1L){
        B <- matrix(nrow = nr, ncol = D - 1L,
                    dimnames = list(NULL, names(delta[-1L])))
        for (i in seq_along(d[-1L])){
            if (!is.null(x$adherence)) {
                B[, i] <- apply(A == x$adherence[x$ranker]/d[i + 1L], 1L, any)
            } else B[, i] <- apply(A == 1L/d[i + 1L], 1L, any)
        }
    }
    # derivatives part 2: expectation of alpha | delta per set to choose from
    size <- lengths(choices$alternatives)
    ord <- order(size)
    if (!is.null(x$adherence)){
        # don't group
        a <- x$adherence[x$ranker][choices$ranking]
        unique_alternatives <- choices$alternatives
    } else {
        a <- NULL
        unique_alternatives <- unique(choices$alternatives[ord])
    }
    na <- lengths(unique_alternatives)
    R <- matrix(0L, nrow = length(na), ncol = max(na))
    R[cbind(rep(seq_along(unique_alternatives), na), sequence(na))] <-
        unlist(unique_alternatives)
    G <- seq_along(unique_alternatives)
    G <- lapply(seq_len(max(na)), function(i) G[na == i])
    P <- unique(na)
    res <- expectation(c("alpha", "delta"), alpha, delta,
                       a, N, d, P, R, G, W = NULL)

    if (!is.null(x$adherence)){
        h <- seq_len(nrow(R))
    } else {
        h <- match(choices$alternatives, unique_alternatives)
    }
    A <- (A - rowsum(res$expA[h,], choices$ranking)) * x$weights
    if (!is.null(ref) && ref %in% names(alpha)) {
        ref <- which(names(alpha) == ref)
    }
    if (D == 1L) {
        if (!is.null(ref)) {
            return(A[, -ref, drop = FALSE])
        } else return(A)
    }
    # N.B. expectation of delta should include delta*, but cancelled out in
    # in iterative scaling so omitted!
    res$expB <- sweep(res$expB, 2L, delta[-1L], "*")
    B <- (B - rowsum(res$expB[h,], choices$ranking)) * x$weights
    if (!is.null(ref)) {
        return(cbind(A[, -ref, drop = FALSE], B))
    } else return(cbind(A[, drop = FALSE], B))
}

