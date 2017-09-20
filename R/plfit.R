#' PlackettLuce Wrapper for Model-based Recursive Partitioning
#'
#' This is a wrapper arround \code{PlackettLuce} as required by
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
    weights <- !is.null(weights)
    offset <- !is.null(offset)
    start <- !is.null(start)
    if (x || weights || offset)
        warning("unused argument(s): ",
                paste(c("x"[x], "start"[start], "weights"[weights],
                        "offset"[offset]), collapse = ","))
    stopifnot(inherits(y, "grouped_rankings"))
    # y is grouped_rankings object
    R <- attr(y, "rankings")
    # return null result if network not strongly connected
    if (attr(R, "no") > 1){
        return(list(coefficients = NA, objfun = Inf,
                    estfun = NULL, object = NULL))
    }
    res <- PlackettLuce(R, ...)
    # returns with rownames - possible to avoid?
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
    N <- length(coef(x)) - D + 1
    # coefs on log-scale here
    lambda <- coef(x)[1:N]
    gamma <- c(0, coef(x)[-c(1:N)])
    # get sizes of selected sets for each observation (as in main function)
    M <- t(Matrix(unclass(x$rankings), sparse = TRUE))
    J <- apply(M, 2, max) # max nsets
    J <- J - as.numeric(rowSums(t(M) == J) == 1) # nontrivial nsets
    # derivative wrt to lambda, first 1/(size of selected sets)
    A <- M
    A[t(t(M) > J)] <- 0
    A@x <- 1/as.double(unlist(apply(A, 2, function(x) tabulate(x)[x])))
    # for derivative wrt gamma, first 1 where select set of corresponding size
    if (D > 1){
        B <- matrix(nrow = D - 1, ncol = ncol(M))
        for (d in 2:D){
            B[d - 1, ] <- apply(A == 1/d, 2, any)
        }
    }
    # subtract expectation of alpha per set to choose from
    for (j in seq_len(max(J))){
        A <- A - sapply(seq_len(ncol(M)), function(i){
            expectation("alpha", exp(lambda), exp(gamma), M[,i, drop = FALSE] >= j,
                        1, N, D, 1)
        })
    }
    # ignore column corresponding to fixed ref
    ref <- x$ref
    if (ref %in% names(lambda)) ref <- which(names(lambda) == ref)
    res <- t(A[-ref, , drop = FALSE])
    if (D > 1){
        for (j in seq_len(max(J))){
            B <- B - sapply(seq_len(ncol(M)), function(i){
                # N.B. expectation should include delta*, but cancelled out in
                # in iterative scaling so omitted!
                exp(gamma)[-1]*expectation("delta", exp(lambda), exp(gamma),
                                           M[,i, drop = FALSE] >= j,
                                           1, N, D, 1)[-1]
            })
        }
      cbind(res, t(B))
    } else res
}
