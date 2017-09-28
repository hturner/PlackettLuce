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
    # get coefficients (unconstrained)
    coef <- x$coefficients
    N <- length(coef) - D + 1
    alpha <- coef[1:N]
    delta <- c(1, coef[-c(1:N)])
    # derivative wrt to alpha part 1: 1/(size of selected set)
    R <- unclass(x$rankings)
    S <- apply(R, 1, function(x){
        last <- which(x == max(x))
        ind <- which(x > 0)
        # exclude untied last place
        x[ind] <- 1/(tabulate(x[ind])[x[ind]])
        if (length(last) == 1) x[last] <- 0
        x
    })
    A <- t(S)
    # derivative wrt delta part 1: row TRUE where selected set has cardinality d
    if (D > 1){
        B <- matrix(nrow = nrow(R), ncol = D - 1,
                    dimnames = list(NULL, names(delta[-1])))
        for (d in 2:D){
            B[, d - 1] <- apply(A == 1/d, 1, any)
        }
    }
    # derivatives part 2: expectation of alpha | delta per set to choose from
    nr <- nrow(R)
    nc <- ncol(R)
    R <- t(apply(R, 1, order, decreasing = TRUE))
    S <- t(vapply(seq_len(nr), function(i) {
        x <- unclass(x$rankings)[i, R[i,]]
        rev(diff(c(0, rev(x))))
    }, numeric(nc), USE.NAMES = FALSE))
    P <- setdiff(which(as.logical(colSums(S) > 0)), 1)
    for (i in seq_len(nr)) {
        res <- expectation("all", alpha, delta, R[r, , drop = FALSE],
                           structure(as.list(S[r,]),
                                     ind = as.list(rep.int(1, nc))),
                           N, D, P)[c("expA", "expB")]
        A[i,] <- A[i,] - res$expA
        # N.B. expectation of delta should include delta*, but cancelled out in
        # in iterative scaling so omitted!
        B[i,] <- B[i,] - delta[-1]*res$expB
    }
    # ignore column corresponding to fixed ref
    ref <- x$ref
    if (ref %in% names(alpha)) ref <- which(names(alpha) == ref)
    cbind(A[, -ref, drop = FALSE], B)
}
