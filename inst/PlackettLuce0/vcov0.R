vcov.PlackettLuce0 <- function(object, ref = NULL, ...) {
  ##  A temporary version until we can do it properly
  ##
  theLongData <- longdat2(unclass(object$rankings))
  coefs <- coef.PlackettLuce0(object, ref = ref)
  na <- is.na(coefs)
  coefnames <- names(coefs[!na])
  ncoefs <- sum(!na)
  X <- theLongData$X
  z <- theLongData$z
  y <- theLongData$y
  ##  Compute the fitted values:
  fit <- as.vector(exp(X %*% coefs[!na]))
  fit <- fit  *  as.vector(tapply(y, z, sum)[z] / tapply(fit, z, sum)[z])
  ##  Compute the vcov matrix
  WX <- fit * X
  XtWX <- crossprod(X, WX)
  ZtWX <- as.matrix(aggregate(WX, by = list(z), FUN = sum)[,-1])
  ZtWZinverse <- 1 / as.vector(tapply(fit, z, sum))
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
  ref <- which((seq_along(coefs) == ref)[!na])
  # Can be done more economically?
  theContrasts <- Diagonal(ncoefs)
  theContrasts[ref, 1:nobj] <- theContrasts[ref, 1:nobj] - 1
  result <- crossprod(theContrasts, result) %*% theContrasts
  rownames(result) <- colnames(result) <- coefnames
  return(as.matrix(result))
}
