logLik.PlackettLuce0 <- function(object, ...){
    structure(object$loglik,
              df = object$rank,
              class = "logLik")
}

deviance.PlackettLuce0 <- function(object, ...){
    -2*object$loglik
}
