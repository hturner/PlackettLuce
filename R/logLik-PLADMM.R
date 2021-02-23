#' @importFrom utils tail
#' @export
logLik.PLADMM <- function(object, ...){
    structure(tail(object$loglik, 1),
              df = object$rank,
              class = "logLik")
}

#' @export
deviance.PLADMM <- function(object, ...){
    -2L*tail(object$loglik, 1)
}
