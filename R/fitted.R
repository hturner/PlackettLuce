#' Fitted probabilities for PlackettLuce objects
#'
#' Fitted probabilities for all choice/alternative combinations in the data.
#'
#' @param object a \code{"PlackettLuce"} object as returned by
#' \code{\link{PlackettLuce}}.
#' @param aggregate logical; if \code{TRUE} observations of the same choice from
#' the same set of alternatives are aggregated.
#' @param free logical; if \code{TRUE} only free choices are included, i.e.
#' choices of one item from a set of one item are excluded.
#' @param ... further arguments passed to method (ignored).
#' @return a list with the following components
#' \item{choices}{The selected item(s).}
#' \item{alternatives}{The set of item(s) that the choice was made from.}
#' \item{ranking}{The ranking(s) including this choice.}
#' \item{n}{If \code{aggregate = TRUE}, the number of rankings including this
#' choice.}
#' \item{fitted}{The fitted probability of making this choice.}
#' @seealso as.choices
#' @export
fitted.PlackettLuce <- function(object, aggregate = TRUE, free = TRUE, ...) {
    # get choices and alternatives for each ranking
    choices <- as.choices(object$rankings, names = FALSE)
    # get parameters
    id <- seq(length(object$coefficients) - object$maxTied + 1)
    alpha <- object$coefficients[id]
    delta <- c(1, unname(object$coefficients[-id]))
    # if free = TRUE, ignore forced choice (choice of 1)
    if (free) {
        free <- lengths(choices$alternatives) != 1
        choices <- lapply(choices, `[`,  free)
    }
    # id unique choices
    unique_choices <- unique(choices$choices)
    g <- match(choices$choices, unique_choices)
    # compute numerators
    n <- lengths(unique_choices)
    numerator <- (delta[n] *
        vapply(unique_choices, function(x) prod(alpha[x]), 1)^(1/n))[g]
    # id unique alternatives
    size <- lengths(choices$alternatives)
    ord <- order(size)
    unique_alternatives <- unique(choices$alternatives[ord])
    # for now work theta out - could perhaps save in object
    na <- lengths(unique_alternatives)
    R <- matrix(0, nrow = length(na), ncol = max(na))
    R[cbind(rep(seq_along(unique_alternatives), na),
            sequence(na))] <- unlist(unique_alternatives)
    G <- seq_along(unique_alternatives)
    G <- lapply(seq_len(max(na)), function(i) G[na == i])
    S <- setdiff(unique(na), 1)
    D <- object$maxTied
    N <- ncol(object$rankings)
    theta <- expectation("theta", alpha, delta, N, D, S, R, G)$theta
    denominator <- numeric(length(numerator))
    h <- match(choices$alternatives, unique_alternatives)
    denominator <- theta[h]
    choices$fitted <- numerator/denominator
    if (aggregate){
        g <- paste(g, h, sep = ":")
        g <- match(g, unique(g))
        choices$ranking <- split(choices$ranking, g)
        keep <- !duplicated(g)
        agg <- c("choices", "alternatives", "fitted")
        choices[agg] <- lapply(choices[agg], function(x) x[keep])
        choices$n <- as.integer(table(g))
        class(choices) <- c("aggregated_choices", "list")
    }
    choices
}
