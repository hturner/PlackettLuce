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
#'
#' # group rankings into two groups
#' G <- grouped_rankings(R, rep(1:2, 3))
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
plfit <- function (y, x = NULL, ref = 1, start = NULL, weights = NULL,
                   offset = NULL, ..., estfun = FALSE, object = FALSE) {
    x <- !(is.null(x) || NCOL(x) == 0L)
    offset <- !is.null(offset)
    if (x || offset)
        warning("unused argument(s): ",
                paste(c("x"[x], "offset"[offset]), collapse = ","))
    res <- PlackettLuce(y, start = start, weights = weights, ...)
    if (object) {
        # returning in order to compute final vcov etc, so can aggregate now
        uniq <- !duplicated(attr(y, "id"))
        g <- match(attr(y, "id"), attr(y, "id")[uniq])
        res$rankings <- structure(res$rankings[uniq,],
                                 class = "rankings")
        res$weights <- tapply(res$weights, g, sum)
    }
    if (estfun) {
        percomp <- estfun.PlackettLuce(res, ref = ref)
        if (object) percomp <- percomp[g, , drop = FALSE]
        estfun <- rowsum(as.matrix(percomp), attr(y, "index"))
    } else estfun <- NULL
    list(coefficients = coef(res, ref = ref),
         maxTied = res$maxTied,
         objfun = -res$loglik,
         estfun = estfun,
         object = if (object) res else NULL)
}

# log-likelihood derivatives (score function)
#' @method estfun PlackettLuce
#' @importFrom sandwich estfun
#' @export
estfun.PlackettLuce <- function(x, ref = 1, ...) {
    D <- x$maxTied
    # get coefficients (unconstrained)
    coef <- x$coefficients
    N <- length(coef) - D + 1
    alpha <- coef[1:N]
    delta <- c(1, coef[-c(1:N)])
    # get choices and alternatives for each ranking
    choices <- choices(x$rankings, names = FALSE)
    # derivative wrt to log alpha part 1: 1/(size of selected set)
    nr <- nrow(x$rankings)
    A <- matrix(0, nrow = nr, ncol = N,
                dimnames = list(NULL, names(alpha)))
    choices$choices <- split(choices$choices, choices$ranking)
    for (i in seq_len(nr)){
        r <- choices$choices[[i]]
        len <- lengths(r)
        if (!is.null(x$adherence)){
            A[i, unlist(r)] <- rep(x$adherence[i]/len, len)
        } else A[i, unlist(r)] <- rep(1/len, len)
    }
    # derivative wrt delta part 1: row TRUE where selected set has cardinality d
    if (D > 1){
        B <- matrix(nrow = nr, ncol = D - 1,
                    dimnames = list(NULL, names(delta[-1])))
        for (d in 2:D){
            B[, d - 1] <- apply(A == 1/d, 1, any)
        }
    }
    # derivatives part 2: expectation of alpha | delta per set to choose from
    size <- lengths(choices$alternatives)
    ord <- order(size)
    if (!is.null(x$adherence)){
        # don't group
        a <- rep(x$adherence, tabulate(choices$ranking))
        unique_alternatives <- choices$alternatives
    } else {
        a <- NULL
        unique_alternatives <- unique(choices$alternatives[ord])
    }
    na <- lengths(unique_alternatives)
    R <- matrix(0, nrow = length(na), ncol = max(na))
    R[cbind(rep(seq_along(unique_alternatives), na), sequence(na))] <-
        unlist(unique_alternatives)
    G <- seq_along(unique_alternatives)
    G <- lapply(seq_len(max(na)), function(i) G[na == i])
    S <- unique(na)
    res <- expectation(c("alpha", "delta"), alpha, delta,
                       N, D, S, R, G, a)

    if (!is.null(x$adherence)){
        h <- order(unlist(G[S]))
    } else {
        h <- match(choices$alternatives, unique_alternatives)
    }
    A <- A - rowsum(res$expA[h,], choices$ranking)
    if (!is.null(ref) && ref %in% names(alpha)) {
        ref <- which(names(alpha) == ref)
    }
    if (D == 1) {
        if (!is.null(ref)) {
            return(A[, -ref, drop = FALSE])
        } else return(A)
    }
    # N.B. expectation of delta should include delta*, but cancelled out in
    # in iterative scaling so omitted!
    res$expB <- sweep(res$expB, 2, delta[-1], "*")
    B <- B - rowsum(res$expB[h,], choices$ranking)
    if (!is.null(ref)) {
        return(cbind(A[, -ref, drop = FALSE], B))
    } else return(cbind(A[, drop = FALSE], B))
}

