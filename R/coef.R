#' Extract Coefficient of Plackett Luce Model
#'
#' @inheritParams PlackettLuce
#' @param object an object of class "PlackettLuce" as returned by \code{PlackettLuce}.
#' @param type the type of coefficients to return: one of \code{"ties"}, \code{"abilities"} or \code{"all"}.
#' @param ... additional arguments, currently ignored.
#'
#' @export
coef.PlackettLuce <- function(object, ref = NULL, type = "all", ...){
  type <-  match.arg(type, c("ties", "abilities", "all"))
  coefs <- log(object$coefficients)
  ncoefs <- length(coefs)
  id <- seq_len(ncoefs - object$maxTied + 1)
  object_names <- names(coefs)[id]
  # allow ref = TRUE to enable use of psychotools::node_btplot
  if (is.null(ref) || isTRUE(ref)) ref <- object$ref
  if (ref %in% object_names) ref <- which(object_names == ref)
  if (ref %in% id) {
      if (is.na(coefs[ref])) stop("Reference ability inestimable")
      coefs[id] <- coefs[id] - coefs[ref]
  } else stop("Invalid value for the 'ref' argument")

  cls <- c("coef.PlackettLuce", "numeric")
  switch(type,
         "ties" = return(coefs[-id]),
         "abilities" = return(structure(coefs[id], ref = ref, class = cls)),
         "all" = return(structure(coefs, ref = ref, class = cls)))
}

#' @method print coef.PlackettLuce
#' @export
print.coef.PlackettLuce <- function (x, ...) {
    print.default(c(x))
}

#' @method itempar PlackettLuce
#' @export
itempar.PlackettLuce <- function(object, ref = NULL, alias = TRUE, vcov = TRUE,
                                 log = FALSE, ...){
    coefs <- coef(object, type = "abilities")
    object_names <- names(coefs)
    n <- length(coefs)
    id <- seq_len(n)[!is.na(coefs)]
    # set reference to one or more indices
    if (is.null(ref)) {
        ref <- id
    } else if (is.vector(ref)){
        if (any(ref %in% object_names)) ref <- match(ref, object_names)
        if (!all(ref %in% id))
            stop("Could not match 'ref' to estimable coefficients")
    } else if (is.matrix(ref)) {
        stop("Handling of contrast matrices in argument 'ref' currently not implemented for itempar.PlackettLuce().")
    } else stop("Argument 'ref' is misspecified (see ?itempar for possible values).")
    ref <- match(ref, id)
    # define parameters
    if (log){
        # based on contrasts
        D <- diag(length(id))
        D[, ref] <- D[, ref] - 1/length(ref)
        coefs[id] <- as.vector(D %*% coefs[id])
    } else {
        # constrained so sum of ref = 1
        alpha <- exp(coefs[id])
        denom <- sum(alpha[ref])
        coefs[id] <- alpha/denom
    }
    # define vcov
    if (vcov){
        if (log) {
            # vcov of contrasts
            V <- D %*% vcov(object) %*% t(D)
        } else {
            # vcov of exp(coefs)
            V <- diag(alpha) %*% vcov(object) %*% diag(alpha)
            # partial derivatives of scaled exp(coefs) wrt exp(coefs)
            D <- array(dim = dim(V))
            nonref <- setdiff(seq_along(id), ref)
            if (length(nonref)){
                D[, nonref] <- 0
                D[cbind(nonref, nonref)] <- 1/denom
            }
            D[, ref] <- -alpha/denom^2
            D[cbind(ref, ref)] <- 1/denom + D[cbind(ref, ref)]
            # vcov of scaled exp coefs
            V <- D %*% V %*% t(D)
        }
        dimnames(V) <- list(object_names[id], object_names[id])
    }
    # remove aliased parameter if required
    if (!alias) {
        alias <- ref[1]
        names(alias) <- names(coefs)[id[ref[1]]]
        coefs <- coefs[-alias]
        V <- V[-alias, -alias]
    }
    structure(coefs, class = "itempar", model = "PlackettLuce",
              ref = ref, alias = alias, vcov = V)
}
