plqv <- function(object, ref = 1) {
  require(qvcalc)
  coefs <- PlackettLuce:::coef.PlackettLuce(object, ref = ref)
  vc <- PlackettLuce:::vcov.PlackettLuce(object, ref = ref)
  nobj <- length(coefs) - object$maxTied + 1
  qvcalc(vc[1:nobj, 1:nobj], estimates = coefs[1:nobj])
}
