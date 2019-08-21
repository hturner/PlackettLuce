#' Fitted Probabilities for PlackettLuce Objects
#'
#' Fitted probabilities for all choice/alternative combinations in the data.
#'
#' @param object an object as returned by
#' \code{\link{PlackettLuce}} or \code{\link{pltree}}.
#' @param aggregate logical; if \code{TRUE} observations of the same choice from
#' the same set of alternatives are aggregated.
#' @param free logical; if \code{TRUE} only free choices are included, i.e.
#' choices of one item from a set of one item are excluded.
#' @param ... further arguments passed to method (ignored).
#' @return A list with the following components
#' \item{choices}{The selected item(s).}
#' \item{alternatives}{The set of item(s) that the choice was made from.}
#' \item{ranking}{The ranking(s) including this choice.}
#' \item{n}{The weighted count of rankings including this
#' choice (equal to the ranking weight if \code{aggregate = FALSE}.}
#' \item{fitted}{The fitted probability of making this choice.}
#' If \code{object} was a \code{"pltree"} object, the list has an
#' additional element \code{node}, specifying which node the ranking corresponds
#' to.
#' @seealso choices
#' @importFrom stats xtabs
#' @export
fitted.PlackettLuce <- function(object, aggregate = TRUE, free = TRUE, ...) {
    # get choices and alternatives for each ranking
    choices <- choices(object$rankings, names = FALSE)
    # get parameters
    id <- seq(length(object$coefficients) - object$maxTied + 1L)
    alpha <- object$coefficients[id]
    delta <- c(1.0, unname(object$coefficients[-id]))
    # if free = TRUE, ignore forced choice (choice of 1)
    if (free) {
        free <- lengths(choices$alternatives) != 1L
        choices <- lapply(choices, `[`,  free)
    }
    # id unique choices
    unique_choices <- unique(choices$choices)
    g <- match(choices$choices, unique_choices)
    # compute numerators
    if (!is.null(object$adherence)){
        n <- lengths(choices$choices)
        a <- rep(object$adherence[object$ranker], tabulate(choices$ranking))
        numerator <- delta[n] *
            (vapply(unique_choices, function(x) prod(alpha[x]), 1.0))[g]^a/n
    } else {
        n <- lengths(unique_choices)
        a <- NULL
        numerator <- (delta[n] *
            vapply(unique_choices, function(x) prod(alpha[x]), 1.0)^(1L/n))[g]
    }
    # id unique alternatives
    size <- lengths(choices$alternatives)
    ord <- order(size)
    if (!is.null(object$adherence)){
        # don't group
        unique_alternatives <- choices$alternatives
    } else unique_alternatives <- unique(choices$alternatives[ord])
    # for now work theta out - could perhaps save in object
    na <- lengths(unique_alternatives)
    R <- matrix(0L, nrow = length(na), ncol = max(na))
    R[cbind(rep(seq_along(unique_alternatives), na),
            sequence(na))] <- unlist(unique_alternatives)
    G <- seq_along(unique_alternatives)
    G <- lapply(seq_len(max(na)), function(i) G[na == i])
    S <- setdiff(unique(na), 1L)
    D <- object$maxTied
    N <- ncol(object$rankings)
    theta <- expectation("theta", alpha, delta, a, N, D, S, R, G)$theta
    if (!is.null(object$adherence)){
        denominator <- theta[order(unlist(G[S]))]
    } else {
        denominator <- numeric(length(numerator))
        h <- match(choices$alternatives, unique_alternatives)
        denominator <- theta[h]
    }
    choices$fitted <- numerator/denominator
    choices$n <- as.integer(object$weights[unlist(choices$ranking)])
    if (inherits(object$na.action, "exclude")){
        n_miss <- length(object$na.action)
        ranking <- seq_len(max(choices$ranking) + n_miss)[-object$na.action]
        choices$ranking <- c(ranking[choices$ranking], object$na.action)
        id <- order(choices$ranking)
        pad <- rep(NA_integer_, n_miss)
        choices$ranking <- choices$ranking[id]
        choices$choices <- c(choices$choices, pad)[id]
        choices$alternatives <- c(choices$alternatives, pad)[id]
        choices$fitted <- c(choices$fitted, pad)[id]
        choices$n <- c(choices$n, pad)[id]
        g <- c(g, pad)[id]
        h <- c(h, pad)[id]
    }
    if (aggregate){
        if (!is.null(object$adherence)) {
            warning("`aggregate` ignored when `object$adherence` is not `NULL`")
            return(choices)
        }
        g <- paste(g, h, sep = ":")
        g <- match(g, unique(g))
        choices$ranking <- split(choices$ranking, g)
        keep <- !duplicated(g)
        agg <- c("choices", "alternatives", "fitted")
        choices[agg] <- lapply(choices[agg], function(x) x[keep])
        choices$n <- as.integer(table(g)*choices$n)
        class(choices) <- c("aggregated_choices", "list")
    }
    choices
}
