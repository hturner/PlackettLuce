#' @export
vcov.PLADMM <- function(object, ...) {
  if ("vcov" %in% names(object)) return(object$vcov)
  alpha <- object$tilde_pi # exp(X %*% beta) including intercept
  nrankings <- nrow(object$orderings)
  # ignore intercept as fixed by sum to 1 constraint
  beta <- coef(object)[-1]
  ncoef <- length(beta)
  # compute Hessian matrix
  H <- matrix(0, ncoef, ncoef, dimnames = list(names(beta), names(beta)))
  Xalpha <- object$x[, -1, drop = FALSE] * alpha
  for (r in seq(nrankings)){
    nitems <- sum(object$orderings[r,] != 0)
    nchoices <- nitems - 1
    for (i in seq(nchoices)){
      items <- object$orderings[r, i:nitems]
      a <- sum(alpha[items])
      xa <- colSums(Xalpha[items, , drop = FALSE])
      H <- H + tcrossprod(xa)/a^2 -
        crossprod(object$x[items, -1, drop = FALSE],
                  Xalpha[items, , drop = FALSE])/a
    }
  }
  # vcov = inverse of Fisher information = inverse of negative Hessian
  solve(-H)
}
