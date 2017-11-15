#' @rdname  summaries
#' @export
#' @importFrom stats aggregate
#' @importFrom MASS ginv
#' @importFrom Matrix crossprod Diagonal
vcov.PlackettLuce <- function(object, ref = 1, ...) {
  ##  A temporary version until we can do it properly
  ##
  ## only worth parameters on log scale here
  coefs <- coef(object, ref = ref)
  ncoefs <- length(coefs)
  nobj <- ncoefs - object$maxTied + 1
  ## also put tie parameters on log scale to compute vcov
  if (ncoefs > nobj){
      delta <- coefs[-seq_len(nobj)]
      coefs[-seq_len(nobj)] <- log(delta)
  }
  ## design matrix as for log-linear model
  theLongData <- poisson_rankings(object$rankings, object$weights)
  X <- theLongData$X
  z <- theLongData$z
  y <- theLongData$y
  w <- theLongData$w
  ##  Compute the fitted values:
  fit <- as.vector(exp(X %*% as.vector(coefs)))
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
  ## ref already checked in coef method (with error if invalid)
  ref <- attr(coefs, "ref")
  ## Can be done more economically?
  theContrasts <- Diagonal(ncoefs)
  theContrasts[ref, seq_len(nobj)] <-
      theContrasts[ref,  seq_len(nobj)] - 1/length(ref)
  result <- crossprod(theContrasts, result) %*% theContrasts
  ## Adjust vcov for tie parameters on original scale
  if (ncoefs > nobj){
      D <- diag(c(rep(1, nobj), delta))
      result <- t(D) %*% result %*% D
  }
  rownames(result) <- colnames(result) <- names(coefs)
  return(as.matrix(result))
}
