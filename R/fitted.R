#' Fitted probabilities for PlackettLuce objects
#'
#' Fitted probabilities for all choice/alternative combinations in the data
#'
#' @param object a \code{"PlackettLuce"} object as returned by
#' \code{\link{PlackettLuce}}.
#' @param ... further arguments passed to method (ignored).
#'
#' @seealso as.choices
#' @importFrom tibble as.tibble
#' @export
fitted.PlackettLuce <- function(object, aggregate = TRUE, ...) {
    # get choices and alternatives for each ranking
    choices <- as.choices(object$rankings, names = FALSE)
    # get parameters
    id <- seq(length(object$coefficients) - object$maxTied + 1)
    alpha <- object$coefficients[id]
    delta <- c(1, unname(mod$coefficients[-id]))
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
    R <- sparseMatrix(i = rep(seq_along(unique_alternatives), na),
                      j = sequence(na),
                      x = unlist(unique_alternatives))
    S <- sparseMatrix(i = seq_along(unique_alternatives),
                      j = na, x = 1)
    theta <- expectation2("theta", alpha, delta, R, S,
                          N = 4, D = object$maxTied, P = unique(na))$theta
    denominator <- numeric(length(numerator))
    h <- match(choices$alternatives, unique_alternatives)
    denominator <- theta[h]
    choices$fitted <- numerator/denominator
    if (aggregate){
        g <- paste(g, h, sep = ":")
        choices$ranking <- split(choices$ranking, g)
        keep <- !duplicated(g)
        agg <- c("choices", "alternatives", "fitted")
        choices[agg] <- lapply(choices[agg], function(x) x[keep])
        choices$n <- table(g)
        class(choices) <- c("aggregated_choices", "list")
    }
    choices
}
