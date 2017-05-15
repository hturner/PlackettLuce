coef.PlackettLuce <- function(object, ref = 1, ...){
  coefs <- log(object $ coefficients)
  ncoefs <- length(coefs)
  nobj <- ncoefs - object $ maxTied + 1
  object_names <- names(coefs)[1:nobj] 
  if (ref %in% object_names) ref <- which(object_names == ref)
  if (ref %in% 1:nobj) {
    coefs[1:nobj] <- coefs[1:nobj] - coefs[ref]
  } else stop("Invalid value for the 'ref' argument")
  return(coefs)
}