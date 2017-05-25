#' Extract Coefficient of Plackett Luce Model
#'
#' @inheritParams PlackettLuce
#' @param object an object of class "PlackettLuce" as returned by \code{PlackettLuce}.
#' @param type the type of coefficients to return: one of \code{"ties"}, \code{"abilities"} or \code{"all"}.
#' @param ... additional arguments, currently ignored.
#'
#' @export
coef.PlackettLuce <- function(object, ref = NULL, type = "all", ...){
  type <-  match.arg(type, c("ties", "abilities", "all"))
  coefs <- log(object$coefficients)
  ncoefs <- length(coefs)
  id <- seq_len(ncoefs - object$maxTied + 1)
  object_names <- names(coefs)[id]
  if (is.null(ref)) ref <- object$ref
  if (ref %in% object_names) ref <- which(object_names == ref)
  if (ref %in% id) {
      coefs[id] <- coefs[id] - coefs[ref]
  } else stop("Invalid value for the 'ref' argument")

  switch(type,
         "ties" = return(coefs[-id]),
         "abilities" = return(structure(coefs[id], ref = ref)),
         "all" = return(structure(coefs, ref = ref)))
}
