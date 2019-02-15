#' @rdname  summaries
#' @export
#' @importFrom stats aggregate
#' @importFrom Matrix crossprod
vcov.PlackettLuce <- function(object, ref = 1L,
                              type = c("expected", "observed"), ...) {
  type <- match.arg(type)
  # don't use coef() here, want unconstrained for now
  coefs  <- log(object$coefficients)
  object_names <- names(coefs)
  p <- length(coefs)
  nobj <- p - object$maxTied + 1L
  # so need to check reference
  if (any(ref %in% object_names)) {
      ref <- match(ref, object_names)
  } else if (!all(ref %in% seq(p))){
      stop("cannot match `ref` with items")
  }
  # get setup for equivalent poisson glm/gnm
  theLongData <- poisson_rankings(object$rankings, object$weights,
                                  if (!is.null(object$gamma)) coefs,
                                  object$adherence, object$ranker,
                                  object$gamma, aggregate = FALSE)
  X <- theLongData$X
  z <- theLongData$z
  y <- theLongData$y
  w <- theLongData$w
  #  Compute the fitted values:
  fit <- as.vector(exp(X[, seq_len(p)] %*% as.vector(coefs)))
  totals <- as.vector(tapply(w * y, z, sum))
  fit <- fit  *  as.vector(totals/tapply(fit, z, sum))[z]
  #  Compute the vcov matrix
  ## XtWX for non-eliminated
  WX <- fit * X
  XtWX <- as.matrix(crossprod(X, WX))
  ## covariance with eliminated (lower left of full XtWX)
  ZtWX <- rowsum(as.matrix(WX), z)
  ## diag of (XtWX)^-1 for eliminated
  ZtWZinverse <- 1L/totals
  ## (generalized) inverse of vcov for non-eliminated parameters
  ## this is minus the expectation of the second derivs of the multinomial log-lik
  ## i.e. (expected) Fisher info for the multinomial log-likelihood
  Info <- XtWX - crossprod(sqrt(ZtWZinverse) * ZtWX)
  ## for observed Fisher info, i.e. second derivs of the multinomial log-lik
  ## add component from (y - mu)deta/(dbeta1 dbeta2) - only affects adherence
  if (type == "observed" & !is.null(object$gamma)){
      ## adjust variance of adherence
      diag(Info[-(1L:p), -(1L:p)]) <- diag(Info[-(1L:p), -(1L:p)]) -
          (w*y - fit) %*% X[, -(1L:p)]
      ## adjust covariance with worth parameters
      adj <- rowsum(as.matrix((w*y - fit) * X[, 1L:nobj]), theLongData$a)
      Info[1L:nobj, -(1L:p)] <- Info[1L:nobj, -(1L:p)] - t(adj)
      Info[-(1L:p), 1L:nobj] <- Info[-(1L:p), 1L:nobj] - adj
  }
  ## Add in components from any priors
  if (!is.null(object$gamma)){
      ## minus expectation of second derivs of (independent) gamma priors
      ind <- (p + 1L):ncol(X)
      Info[cbind(ind, ind)] <- Info[cbind(ind, ind)] +
          object$gamma$rate * object$adherence *
          rowsum(object$weights, object$ranker)[,1]
  }
  # create vcov of parameters of interest (worth, tie)
  if (!is.null(object$normal)){
      ## minus expectation of second derivs of multivariate normal prior
      Info[seq_len(nobj), seq_len(nobj)] <- Info[seq_len(nobj), seq_len(nobj)] +
          object$normal$invSigma
      ## invert full matrix
      result <- chol2inv(chol(Info))[seq(p), seq(p)]
      dimnames(result) <- list(names(coef), names(coef))
  } else {
      ## drop a column and invert
      ## if reference is a single item this is equivalent to taking generalised
      ## inverse and then re-parameterising
      id <- ifelse(length(ref) == 1L, ref, 1L)
      result <- array(0.0, dim = c(p, p),
                      dimnames = list(names(coefs), names(coefs)))
      result[-id, -id] <-
          chol2inv(chol(Info[-id, -id]))[seq(p - 1L), seq(p - 1L)]
      ## so that's all the work done if the reference is a single item
      if (length(ref) == 1) return(result)
  }
  # compute vcov for specified set of contrasts if necessary
  ## equiv of D <- diag(p); D[ref, seq(nobj)] <- 1 - 1/n; t(D) %*% V %*% D
  n <- length(ref)
  r <- rowSums(result[, ref, drop = FALSE])
  result[, seq(nobj)] <- result[, seq(nobj)] - r/n
  r <- colSums(result[ref, , drop = FALSE])
  result[seq(nobj), ] <- result[seq(nobj), ] - rep(r/n, each = nobj)
  result
}
