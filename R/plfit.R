#' PlackettLuce Wrapper for Model-based Recursive Partitioning
#'
#' This is a wrapper around \code{PlackettLuce} as required by
#' \code{\link[partykit]{mob}} for model-based recursive partitioning. It is
#' not intended for general use.
#'
#' @param y a \code{"\link{grouped_rankings}"} object giving the rankings to
#' model.
#' @param x unused.
#' @param start unused.
#' @param weights unused (generates warnings).
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
#' # group by subject
#' G <- grouped_rankings(R, rep(1:2, 3))
#'
#' # plfit() gives the same results as PlackettLuce()
#' plfit(G)
#'
#' PlackettLuce(R)
#' @export
plfit <- function (y, x = NULL, start = NULL, weights = NULL, offset = NULL,
                   ..., estfun = FALSE, object = FALSE) {
    x <- !(is.null(x) || NCOL(x) == 0L)
    offset <- !is.null(offset)
    start <- !is.null(start)
    if (x || offset)
        warning("unused argument(s): ",
                paste(c("x"[x], "start"[start], "offset"[offset]),
                      collapse = ","))
    res <- PlackettLuce(y, weights = weights, ...)
    if (estfun) {
        percomp <- estfun.PlackettLuce(res)
        estfun <- rowsum(as.matrix(percomp), attr(y, "index"))
    } else estfun <- NULL
    list(coefficients = coef(res), objfun = -res$loglik,
         estfun = estfun,
         object = if (object) res else NULL)
}

# log-likelihood derivatives (score function)
#' @method estfun PlackettLuce
#' @importFrom sandwich estfun
#' @export
estfun.PlackettLuce <- function(x) {
    D <- x$maxTied
    # get coefficients (unconstrained)
    coef <- x$coefficients
    N <- length(coef) - D + 1
    alpha <- coef[1:N]
    delta <- c(1, coef[-c(1:N)])
    # get choices and alternatives for each ranking
    choices <- as.choices(x$rankings, names = FALSE)
    # derivative wrt to alpha part 1: 1/(size of selected set)
    nr <- nrow(x$rankings)
    A <- matrix(0, nrow = nr, ncol = N,
                dimnames = list(NULL, names(alpha)))
    for (i in seq_len(nr)){
        r <- choices$choices[choices$ranking == i]
        len <- lengths(r)
        A[i, unlist(r)] <- rep(1/len, len)
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
    # id unique alternatives - need to see how weights would fit in
    size <- lengths(choices$alternatives)
    ord <- order(size)
    unique_alternatives <- unique(choices$alternatives[ord])
    na <- lengths(unique_alternatives)
    R <- matrix(0, nrow = length(na), ncol = max(na))
    R[cbind(rep(seq_along(unique_alternatives), na), sequence(na))] <-
        unlist(unique_alternatives)
    G <- seq_along(unique_alternatives)
    G <- lapply(seq_len(max(na)), function(i) G[na == i])
    S <- setdiff(unique(na), 1)
    res <- expectation(c("alpha", "delta"), alpha, delta,
                       N = N, D = D, S = unique(na), R, G)
    h <- match(choices$alternatives, unique_alternatives)
    A <- A - rowsum(res$expA[h,], choices$ranking)
    # ignore column corresponding to fixed ref
    ref <- x$ref
    if (ref %in% names(alpha)) ref <- which(names(alpha) == ref)
    if (D == 1) return(A[, -ref, drop = FALSE])
    # N.B. expectation of delta should include delta*, but cancelled out in
    # in iterative scaling so omitted!
    res$expB <- sweep(res$expB, 2, delta[-1], "*")
    B <- B - rowsum(res$expB[h,], choices$ranking)
    cbind(A[, -ref, drop = FALSE], B)
}

