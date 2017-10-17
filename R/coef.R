#' Extract Coefficients of Plackett-Luce Model
#'
#' Extract the coefficients of a model fitted by \code{PlackettLuce}. By
#' default, parameters are returned on the log scale, as most suited for
#' inference. If \code{log = FALSE}, the worth parameters are returned,
#' constrained to sum to one so that they represent the probability that
#' the corresponding item comes first in a ranking of all items, given that
#' first place is not tied.
#'
#' @param object an object of class "PlackettLuce" as returned by
#'  \code{PlackettLuce}.
#' @param ref an integer or character string specifying the reference item (for
#' which log worth will be set to zero). If \code{NULL} the sum of the log worth
#' parameters is set to zero.
#' @param log a logical indicating whether to return parameters on the log scale
#' with the item specified by \code{ref} set to zero.
#' @param type the type of coefficients to return: one of \code{"ties"},
#' \code{"worth"} or \code{"all"}.
#' @param ... additional arguments, currently ignored.
#'
#' @export
coef.PlackettLuce <- function(object, ref = 1, log = TRUE,
                              type = "all", ...){
  type <-  match.arg(type, c("ties", "worth", "all"))
  if (!log) {
      coefs <- object$coefficients
  } else {
      item <- itempar(object, ref = ref, log = log, vcov = FALSE)
      ref <- attr(item, "ref")
      nitem <- length(item)
      coefs <- c(item, log(object$coefficients[-seq_len(nitem)]))
  }
  ncoefs <- length(coefs)
  id <- seq_len(ncoefs - object$maxTied + 1)
  cls <- c("coef.PlackettLuce", "numeric")
  switch(type,
         "ties" = return(coefs[-id]),
         "worth" = return(structure(coefs[id], ref = ref, class = cls)),
         "all" = return(structure(coefs, ref = ref, class = cls)))
}

#' @method print coef.PlackettLuce
#' @export
print.coef.PlackettLuce <- function (x, ...) {
    print.default(c(x))
}
