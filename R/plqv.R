# create qvcalc generic (should be done in qvcalc)
#' Quasi Variances for Model Coefficients
#'
#' Computes a set of quasi variances (and corresponding quasi standard errors)
#' for estimated model coefficients relating to the levels of a categorical
#' (i.e., factor) explanatory variable. For details of the method see Firth
#' (2000), Firth (2003) or Firth and de Menezes (2004). Quasi variances
#' generalize and improve the accuracy of “floating absolute risk”
#' (Easton et al., 1991).
#'
#' @param object An object acceopted by \code{\link[qvcalc]{qvcalc}} or a
#' \code{"PlackettLuce"} object.
#' @param factorname See \code{\link[qvcalc]{qvcalc}}.
#' @param coef.indices See \code{\link[qvcalc]{qvcalc}}.
#' @param labels See \code{\link[qvcalc]{qvcalc}}.
#' @param dispersion See \code{\link[qvcalc]{qvcalc}}.
#' @param estimates See \code{\link[qvcalc]{qvcalc}}.
#' @param modelcall See \code{\link[qvcalc]{qvcalc}}.
#' @param ... Further arguments passed to or from other methods.
#' @inheritParams PlackettLuce
#'
#' @export
qvcalc <- function(object, ...){
    UseMethod("qvcalc")
}


#' @rdname qvcalc
#' @export
qvcalc.default <- function(object, factorname = NULL, coef.indices = NULL,
                           labels = NULL, dispersion = NULL, estimates = NULL,
                           modelcall = NULL, ...){
    call <- match.call()
    call[[1]] <- as.name(qvcalc::qvcalc)
    eval(call)
}

#' @rdname qvcalc
qvcalc.PlackettLuce <- function(object, ref = NULL, ...) {
  qv <- requireNamespace("qvcalc")
  if (!qv) stop("qvcalc package required to compute quasivariances")
  coefs <- coef(object, ref = ref)
  vc <- vcov.PlackettLuce(object, ref = ref)
  nobj <- length(coefs) - object$maxTied + 1
  # qvcalc does not accept Matrix objects
  qvcalc::qvcalc(as.matrix(vc[1:nobj, 1:nobj]), estimates = coefs[1:nobj])
}
