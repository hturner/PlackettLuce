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
