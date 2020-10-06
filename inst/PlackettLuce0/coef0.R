#' Extract Coefficient of Plackett Luce Model
#'
#' @inheritParams PlackettLuce
#' @param object an object of class "PlackettLuce" as returned by
#' \code{PlackettLuce}.
#' @param type the type of coefficients to return: one of \code{"ties"},
#' \code{"abilities"} or \code{"all"}.
#' @param ... additional arguments, currently ignored.
#'
coef.PlackettLuce0 <- function(object, ref = NULL, type = "all", ...){
  type <-  match.arg(type, c("ties", "abilities", "all"))
  coefs <- log(object$coefficients)
  ncoefs <- length(coefs)
  id <- seq_len(ncoefs - object$maxTied + 1)
  object_names <- names(coefs)[id]
  # allow ref = TRUE to enable use of psychotools::node_btplot
  if (is.null(ref) || isTRUE(ref)) ref <- object$ref
  if (ref %in% object_names) ref <- which(object_names == ref)
  if (ref %in% id) {
      if (is.na(coefs[ref])) stop("Reference ability inestimable")
      coefs[id] <- coefs[id] - coefs[ref]
  } else stop("Invalid value for the 'ref' argument")

  cls <- c("coef.PlackettLuce0", "numeric")
  switch(type,
         "ties" = return(coefs[-id]),
         "abilities" = return(structure(coefs[id], ref = ref, class = cls)),
         "all" = return(structure(coefs, ref = ref, class = cls)))
}

#' @method print coef.PlackettLuce0
#' @export
print.coef.PlackettLuce0 <- function (x, ...) {
    print.default(c(x))
}
