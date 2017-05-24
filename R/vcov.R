vcov.PlackettLuce <- function(object, ref = 1, ...) {
  ##  A temporary version until we can do it properly
  ##
  ##  Not sure whether to use longdat() or longdat2()
  ##  -- so let's just try the second if the first one fails:
  theLongData <- try(PlackettLuce:::longdat(object$rankings), silent = TRUE)
  if (inherits(theLongData, "try-error")) theLongData <- PlackettLuce:::longdat2(object$rankings)
  coefs <- PlackettLuce:::coef.PlackettLuce(object, ref = ref)
  coefnames <- names(object$coefficients)
  ncoefs <- length(coefs)
  X <- theLongData $ X
  z <- theLongData $ z
  y <- theLongData $ y
  ##  Compute the fitted values:
  fit <- as.vector(exp(X %*% coefs))
  fit <- fit  *  as.vector(tapply(y, z, sum)[z] / tapply(fit, z, sum)[z])
  ##  Compute the vcov matrix
  WX <- fit * X
  XtWX <- crossprod(X, WX)
  ZtWX <- as.matrix(aggregate(WX, by = list(z), FUN = sum)[,-1])
  ZtWZinverse <- 1 / as.vector(tapply(fit, z, sum))
  result <- MASS:::ginv(XtWX - crossprod(sqrt(ZtWZinverse) * ZtWX))    ## Should we try to avoid ginv() ?
  ##
  ##  That's the basic computation all done, ie to get Moore-Penrose inverse of the information matrix.
  ##
  ##  The rest is all about presenting the result as the /actual/ vcov matrix for a specified
  ##  set of contrasts (or equivalently a specified constraint on the parameters).
  nobj <- ncoefs - object$maxTied + 1
  if (ref %in% coefnames) ref <- which(coefnames == ref)
  if (ref %in% 1:ncoefs) {
    theContrasts <- diag(ncoefs)  # Better to do this sparsely?
    theContrasts[ref, 1:nobj] <- theContrasts[ref, 1:nobj] - 1  # Can be done more economically?
    } else stop("Invalid value for the 'ref' argument")
  result <- crossprod(theContrasts, result) %*% theContrasts
  rownames(result) <- colnames(result) <- coefnames
  return(result)
}