#' Quasi Variances for Model Coefficients
#'
#' A method for \code{\link{qvcalc}} to compute a set of quasi variances (and
#' corresponding quasi standard errors) for estimated item parameters from a
#' Plackett-Luce model.
#'
#' For details of the method see Firth (2000), Firth (2003) or Firth and de
#' Menezes (2004). Quasi variances generalize and improve the accuracy of
#' \dQuote{floating absolute risk} (Easton et al., 1991). This device for
#' economical model summary was first suggested by Ridout (1989).
#'
#' Ordinarily the quasi variances are positive and so their square roots
#' (the quasi standard errors) exist and can be used in plots, etc.
#'
#' @param object a \code{"PlackettLuce"} object as returned by
#' \code{PlackettLuce}.
#' @param ... additional arguments, currently ignored..
#' @inheritParams summaries
#' @return A list of class \code{"qv"}, with components
#' \item{covmat}{The full variance-covariance matrix for the item
#' parameters.}
#' \item{qvframe}{A data frame with variables `estimate`, `SE`, `quasiSE` and
#' `quasiVar`, the last two being a quasi standard error and quasi-variance
#' for each parameter.}
#' \item{dispersion}{\code{NULL} (dispersion is fixed to 1).}
#' \item{relerrs}{Relative errors for approximating the standard errors of all
#' simple contrasts.}
#' \item{factorname}{\code{NULL} (not required for this method).}
#' \item{coef.indices}{\code{NULL} (not required for this method).}
#' \item{modelcall}{The call to \code{PlackettLuce} to fit the model from which
#' the item parameters were estimated.}
#' @seealso \code{\link{worstErrors}}, \code{\link{plot.qv}}.
#' @references
#' Easton, D. F, Peto, J. and Babiker, A. G. A. G. (1991) Floating absolute
#' risk: an alternative to relative risk in survival and case-control analysis
#' avoiding an arbitrary reference group. *Statistics in Medicine* **10**,
#' 1025--1035.
#'
#' Firth, D. (2000)  Quasi-variances in Xlisp-Stat and on the web.
#' *Journal of Statistical Software* **5.4**, 1--13.
#' At http://www.jstatsoft.org
#'
#' Firth, D. (2003) Overcoming the reference category problem in the
#' presentation of statistical models. *Sociological Methodology*
#' **33**, 1--18.
#'
#' Firth, D. and de Menezes, R. X. (2004)  Quasi-variances.
#' *Biometrika* **91**, 65--80.
#'
#' Menezes, R. X. de (1999)  More useful standard errors for group and factor
#' effects in generalized linear models.  *D.Phil. Thesis*,
#' Department of Statistics, University of Oxford.
#'
#' Ridout, M.S. (1989). Summarizing the results of fitting generalized
#' linear models to data from designed experiments. In: *Statistical
#'     Modelling: Proceedings of GLIM89 and the 4th International
#'     Workshop on Statistical Modelling held in Trento, Italy, July 17--21,
#'     1989* (A. Decarli et al., eds.), pp 262--269. New York: Springer.
#' @examples
#' # Six partial rankings of four objects, 1 is top rank, e.g
#' # first ranking: item 1, item 2
#' # second ranking: item 2, item 3, item 4, item 1
#' # third ranking: items 2, 3, 4 tie for first place, item 1 second
#' R <- matrix(c(1, 2, 0, 0,
#'               4, 1, 2, 3,
#'               2, 1, 1, 1,
#'               1, 2, 3, 0,
#'               2, 1, 1, 0,
#'               1, 0, 3, 2), nrow = 6, byrow = TRUE)
#' colnames(R) <- c("apple", "banana", "orange", "pear")
#'
#' mod <- PlackettLuce(R)
#' qv <- qvcalc(mod)
#' qv
#' plot(qv)
#' @importFrom stats vcov
#' @export
qvcalc.PlackettLuce <- function(object, ref = 1L, ...) {
  coefs <- coef(object, ref = ref)
  vc <- vcov(object, ref = ref)
  nobj <- length(coefs) - object$maxTied + 1L
  qvcalc(vc[1L:nobj, 1L:nobj], estimates = coefs[1L:nobj],
         modelcall = object$call)
}

#' @importFrom qvcalc qvcalc
#' @export
qvcalc::qvcalc
