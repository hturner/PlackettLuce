#' @export
#' @importFrom stats aggregate
#' @importFrom MASS ginv
#' @importFrom Matrix crossprod Diagonal
vcov.PlackettLuce <- function(object, ref = NULL, ...) {
  ##  A temporary version until we can do it properly
  ##
  theLongData <- poisson_rankings(object$rankings, object$weights)
  coefs <- coef(object, ref = ref)
  na <- is.na(coefs)
  coefnames <- names(coefs[!na])
  ncoefs <- sum(!na)
  X <- theLongData$X
  z <- theLongData$z
  y <- theLongData$y
  w <- theLongData$w
  ##  Compute the fitted values:
  fit <- as.vector(exp(X %*% coefs[!na]))
  totals <- as.vector(tapply(w * y, z, sum))
  fit <- fit  *  as.vector(totals/tapply(fit, z, sum))[z]
  ##  Compute the vcov matrix
  WX <- fit * X
  XtWX <- as.matrix(crossprod(X, WX))
  ZtWX <- rowsum(as.matrix(WX), z)
  ZtWZinverse <- 1/totals
  ## Should we try to avoid ginv() ?
  result <- ginv(XtWX - crossprod(sqrt(ZtWZinverse) * ZtWX))
  ##
  ##  That's the basic computation all done, ie to get Moore-Penrose inverse of
  ##  the information matrix.
  ##
  ##  The rest is all about presenting the result as the /actual/ vcov matrix
  ##  for a specified set of contrasts (or equivalently a specified constraint
  ##  on the parameters).
  nobj <- ncoefs - object$maxTied + 1
  # ref already checked in coef method (with error if invalid)
  ref <- attr(coefs, "ref")
  # Can be done more economically?
  theContrasts <- Diagonal(ncoefs)
  theContrasts[ref, seq_len(nobj)] <-
      theContrasts[ref,  seq_len(nobj)] - 1/length(ref)
  result <- crossprod(theContrasts, result) %*% theContrasts
  rownames(result) <- colnames(result) <- coefnames
  return(as.matrix(result))
}
