## Todo:
## Drop dependence on tibble
## Avoid re-evaluation for repeated choices-alternatives combinations
## Optimize subsetting in prob

#' Fitted probabilities for PlackettLuce objects
#'
#' Fitted probabilities for all choice/alternative combinations in the data
#'
#' @param object a \code{"PlackettLuce"} object as returned by
#' \code{\link{PlackettLuce}}.
#' @param ... further arguments passed to method (ignored).
#'
#' @seealso choices
#' @importFrom tibble as.tibble
#' @export
fitted.PlackettLuce0 <- function(object, ...) {
    patterns <- object$patterns
    choices <- choices(object$rankings, names = FALSE)
    choices <- tibble::as.tibble(choices)
    objects <- attr(choices, "objects")
    choices$D <- unlist(lapply(choices[["choices"]], length))
    choices$T <- unlist(lapply(choices[["alternatives"]], length))
    npatterns <- ncol(patterns)
    nobjects <- nrow(patterns)
    alpha <- object$coefficients[seq_len(nobjects)]
    delta <- c(1, object$coefficients[-seq_len(nobjects)])
    nconst <- object$constants
    prob <- function(obs) {
        D <- obs$D
        alt <- obs$alternatives
        ch <- obs$choices
        if (obs$T == 1) 1 else {
            delta[D] * prod(alpha[ch])^(1/D) /
                nconst[colSums(patterns == (seq.int(nobjects) %in% alt)) ==
                           nobjects] }
    }
    choices$fitted <- apply(choices, 1, prob)
    choices$D <- choices$T <- NULL
    attr(choices, "objects") <- objects
    choices
}
