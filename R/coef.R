coef.PlackettLuce <- function(object, ref = 1, parameters = "all", ...){
  what <-  match.arg(what, c("ties", "abilities", "all"))
  coefs <- log(object $ coefficients)
  ncoefs <- length(coefs)
  nobj <- ncoefs - object $ maxTied + 1
  object_names <- names(coefs)[1:nobj]
  if (ref %in% object_names) ref <- which(object_names == ref)
  if (ref %in% 1:nobj) {
    coefs[1:nobj] <- coefs[1:nobj] - coefs[ref]
  } else stop("Invalid value for the 'ref' argument")
  switch(what,
         "ties" = return(coefs[-c(1:nobj)]),
         "abilities" = return(coefs[1:nobj]),
         "all" = return(coefs))
}
