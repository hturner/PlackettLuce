#' @export
logLik.PlackettLuce <- function(object, ...){
    structure(object$loglik,
              df = object$rank,
              class = "logLik")
}

#' @export
deviance.PlackettLuce <- function(object, ...){
    -2L*object$loglik
}
