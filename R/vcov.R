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
                                  object$adherence, object$ranker,
                                  object$gamma, aggregate = FALSE)
  X <- theLongData$X
  z <- theLongData$z
  y <- theLongData$y
  w <- theLongData$w
  if (!is.null(object$gamma)){
      a <- theLongData$a
      # non-zero values in X cols for log-adherence
      A <-  as.vector(X[, seq(nobj)] %*% as.vector(coefs)[seq(nobj)])
  }
  #  Compute the fitted values:
  fit <-  as.vector(exp(X %*% as.vector(coefs)))
  totals <- as.vector(tapply(w * y, z, sum))
  fit <- fit  *  as.vector(totals/tapply(fit, z, sum))[z]
  #  Compute the vcov matrix
  ## XtWX for worth and tie parameters (and adherence)
  WX <- fit * X
  XtWX <- as.matrix(crossprod(X, WX))
  if (!is.null(object$gamma))
      AtWX <- rowsum(A*as.matrix(WX[, 1:p]), theLongData$a)
  ## covariance of worth and tie with eliminated (and adherence) parameters
  ## crossprod(sparse.model.matrix(~as.factor(z) - 1), WX))
  ZtWX <- rowsum(as.matrix(WX), z)
  if (!is.null(object$gamma)) ZtWA <- rowsum(fit*A, z)[, 1]
  ## diag of (XtWX)^-1 for eliminated
  ZtWZinverse <- 1L/totals
  ## diag of XtWX for adherence
  if (!is.null(object$gamma)) AtWA <- rowsum(fit*A^2, theLongData$a)[,1]
  ## components of (generalized) inverse of vcov for worth, tie and adherence
  ## this is minus the expectation of the second derivs of the multinomial
  ## log-lik i.e. (expected) Fisher info for the multinomial log-likelihood
  ### top-left: rows/cols for worth and tie
  InfoTL <- XtWX - crossprod(sqrt(ZtWZinverse) * ZtWX)
  if (!is.null(object$gamma)){
      ### bottom-left: cross derivs adherence and worth/tie
      InfoBL <- AtWX - rowsum(ZtWX * ZtWA * ZtWZinverse, a[!duplicated(z)])
      ### diagonal of bottom-right: rows/cols for adherence
      diagInfoBR <- AtWA - rowsum(ZtWA^2 * ZtWZinverse, a[!duplicated(z)])[,1]
  }
  ## for observed Fisher info, i.e. second derivs of the multinomial log-lik
  ## add component from (y - mu)*deta/(dbeta1 dbeta2) - only affects adherence
  if (type == "observed" & !is.null(object$gamma)){
      ## adjust variance of adherence
      diagInfoBR <- diagInfoBR - rowsum((w*y - fit)*A, a)[,1]
      ## adjust covariance with worth parameters
      adj <- rowsum(as.matrix((w*y - fit) * X[, seq_len(nobj)]), a)
      InfoBL[, seq_len(nobj)] <- InfoBL[, seq_len(nobj)] - adj
  }
  ## Add in component from gamma prior
  if (!is.null(object$gamma)){
      ## minus expectation of second derivs of (independent) gamma priors
      diagInfoBR <- diagInfoBR + object$gamma$rate * object$adherence *
          rowsum(object$weights, object$ranker)[,1]
  }
  # create vcov of parameters of interest (worth, tie)
  if (!is.null(object$normal)){
      ## minus expectation of second derivs of multivariate normal prior
      InfoTL[seq_len(nobj), seq_len(nobj)] <-
          InfoTL[seq_len(nobj), seq_len(nobj)] + object$normal$invSigma
      ## get vcov for worth and tie only
      if (!is.null(object$gamma)){
          result <- chol2inv(chol(InfoTL - crossprod(InfoBL/sqrt(diagInfoBR))))
      } else result <- chol2inv(chol(InfoTL))
      dimnames(result) <- list(names(coef), names(coef))
  } else {
      ## drop a column and get vcov for worth and tie only
      ## if reference is a single item this is equivalent to taking generalised
      ## inverse and then re-parameterising
      id <- ifelse(length(ref) == 1L, ref, 1L)
      result <- array(0.0, dim = c(p, p),
                      dimnames = list(names(coefs), names(coefs)))
      if (!is.null(object$gamma)){
          result[-id, -id] <-
              chol2inv(chol(InfoTL[-id, -id] -
                                crossprod((InfoBL/sqrt(diagInfoBR))[, -id])))
      } else result[-id, -id] <- chol2inv(chol(InfoTL[-id, -id]))
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
