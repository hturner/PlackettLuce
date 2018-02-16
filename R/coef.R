#' Plackett-Luce Model Summaries
#'
#' Obtain the coefficients, model summary or coefficient variance-covariance
#' matrix for a model fitted by \code{PlackettLuce}.
#'
#' By default, parameters are returned on the log scale, as most suited for
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
#' @name summaries
#' @export
coef.PlackettLuce <- function(object, ref = 1, log = TRUE,
                              type = "all", ...){
  type <-  match.arg(type, c("ties", "worth", "all"))
  ncoefs <- length(object$coefficients)
  id <- seq_len(ncoefs - object$maxTied + 1)
  if (!log) {
      # ignore ref here, always return probabilities
      const <- sum(object$coefficients[id])
      coefs <- c(object$coefficients[id]/const, object$coefficients[-id])
  } else {
      const <- mean(log(object$coefficients[id])[ref])
      item <- itempar.PlackettLuce(object, ref = ref, log = log, vcov = FALSE)
      ref <- attr(item, "ref")
      coefs <- c(item, log(object$coefficients[-id]))
  }
  cls <- c("coef.PlackettLuce", "numeric")
  switch(type,
         "ties" = return(coefs[-id]),
         "worth" = return(structure(coefs[id], ref = ref, log = log,
                                    const = const, class = cls)),
         "all" = return(structure(coefs, ref = ref, log = log,
                                  const = const, class = cls)))
}

#' @method print coef.PlackettLuce
#' @export
print.coef.PlackettLuce <- function (x, ...) {
    print.default(c(x))
}
