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
#' The variance-covariance matrix is returned for the worth and tie parameters
#' on the log scale, with the reference as specified by \code{ref}. For models
#' estimated by maximum likelihood, the variance-covariance is the inverse of
#' the Fisher information of the log-likelihood.
#'
#' For models with a normal or gamma prior, the variance-covariance is based on
#' the Fisher information of the log-posterior. When adherence parameters have
#' been estimated, the log-posterior is not linear in the parameters. In this
#' case there is a difference between the expected and observed Fisher
#' information. By default, \code{vcov} will return the variance-covariance
#' based on the expected information, but \code{type} gives to option to use
#' the observed information instead. For large samples, the difference between
#' these options should be small. Note that the estimation of the adherence
#' parameters is accounted for in the computation of the variance-covariance
#' matrix, but only the sub-matrix corresponding to the worth and tie
#' parameters is estimated.
#' @param object An object of class "PlackettLuce" as returned by
#'  \code{PlackettLuce}.
#' @param ref An integer or character string specifying the reference item (for
#' which log worth will be set to zero). If \code{NULL} the sum of the log worth
#' parameters is set to zero.
#' @param log A logical indicating whether to return parameters on the log scale
#' with the item specified by \code{ref} set to zero.
#' @param type For \code{coef}, the type of coefficients to return: one of
#' \code{"ties"}, \code{"worth"} or \code{"all"}. For \code{vcov}, the type of
#' Fisher information to base the estimation on: either \code{"expected"} or
#' \code{"observed"}.
#' @param ... additional arguments, passed to \code{vcov} by \code{summary}.
#' @name summaries
#' @export
coef.PlackettLuce <- function(object, ref = 1L, log = TRUE,
                              type = "all", ...){
  type <-  match.arg(type, c("ties", "worth", "all"))
  ncoefs <- length(object$coefficients)
  id <- seq_len(ncoefs - length(object$ties) + 1L)
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
