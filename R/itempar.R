#' Extract Item Parameters of Plackett-Luce Models
#'
#' Methods for \code{\link[psychotools]{itempar}} to extract the item
#' parameters (worth or log-worth) from a Plackett-Luce model or tree.
#' In the case of a tree, item parameters are extracted for each terminal node.
#'
#' @param object a fitted model object as returned by
#' \code{\link{PlackettLuce}},  \code{\link{pladmm}}, or \code{\link{pltree}}.
#' @param ref  a vector of labels or position indices of item parameters which
#' should be used as restriction/for normalization. If \code{NULL}
#' (the default), all items are used with a zero sum (\code{log = TRUE}) or
#' unit sum (\code{log = FALSE}) constraint.
#' @param alias logical. If \code{TRUE} (the default), the aliased parameter is
#' included in the return vector (and in the variance-covariance matrix if
#' \code{vcov = TRUE}). If \code{FALSE}, it is removed. If the restriction given
#'  in ref depends on several parameters, the first parameter of the restriction
#'  specified is (arbitrarily) chosen to be removed if alias is \code{FALSE}.
#' @param vcov logical. If \code{TRUE} (the default), the (transformed)
#' variance-covariance matrix of the item parameters is attached as attribute
#' \code{vcov}. If \code{FALSE}, a \code{NA}-matrix is attached.
#' @param log logical. Whether to return log-abilities (\code{TRUE}) or
#' abilities (\code{FALSE}).
#' @param ... further arguments which are currently not used.
#' @return An object of class \code{"itempar"}, see
#' \code{\link[psychotools]{itempar}}.
#' @examples
#' R <- matrix(c(1, 2, 0, 0,
#'               4, 1, 2, 3,
#'               2, 1, 1, 1,
#'               1, 2, 3, 0,
#'               2, 1, 1, 0,
#'               1, 0, 3, 2), nrow = 6, byrow = TRUE)
#' colnames(R) <- c("apple", "banana", "orange", "pear")
#'
#' mod <- PlackettLuce(R)
#' coef(mod)
#'
#' # equivalent to default coefficients, i.e. log abilities
#' itempar(mod, ref= 1, log = TRUE)
#'
#' # abilities, normalized so abilities for apple and pear sum to 1
#' itempar(mod, ref = 1:2)
#'
#' @method itempar PlackettLuce
#' @export
itempar.PlackettLuce <- function(object, ref = NULL, alias = TRUE, vcov = TRUE,
                                 log = FALSE, ...){
    item <- seq_len(length(object$coefficients) - length(object$ties) + 1L)
    coefs <- log(object$coefficients[item])
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
        stop("Handling of contrast matrices in argument 'ref' currently not ",
             "implemented for itempar.PlackettLuce().")
    } else stop("Argument 'ref' is misspecified (see ?itempar.PlackettLuce ",
                "for possible values).")
    ref <- match(ref, id)
    # define parameters
    if (log){
        # based on contrasts
        D <- diag(length(id))
        D[, ref] <- D[, ref] - 1L/length(ref)
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
            V <- D %*% vcov(object)[id, id] %*% t(D)
        } else {
            # vcov of exp(coefs)
            V <- diag(alpha) %*% vcov(object)[id, id] %*% diag(alpha)
            # partial derivatives of scaled exp(coefs) wrt exp(coefs)
            D <- array(dim = dim(V))
            nonref <- setdiff(seq_along(id), ref)
            if (length(nonref)){
                D[, nonref] <- 0
                D[cbind(nonref, nonref)] <- 1L/denom
            }
            D[, ref] <- -alpha/denom^2
            D[cbind(ref, ref)] <- 1L/denom + D[cbind(ref, ref)]
            # vcov of scaled exp coefs
            V <- D %*% V %*% t(D)
        }
        dimnames(V) <- list(object_names[id], object_names[id])
    }
    # remove aliased parameter if required
    if (!alias) {
        alias <- ref[1L]
        names(alias) <- names(coefs)[id[ref[1L]]]
        coefs <- coefs[-alias]
        if (vcov) V <- V[-alias, -alias]
    }
    structure(coefs, class = "itempar", model = "PlackettLuce",
              ref = ref, alias = alias, vcov = if (vcov) V)
}

#' @importFrom psychotools itempar
#' @export
psychotools::itempar

#' @rdname itempar.PlackettLuce
#' @method itempar pltree
#' @importFrom partykit nodeapply
#' @export
itempar.pltree <- function (object, ...){
    # so unexported itempar.bttree is used from psychotree
    requireNamespace("psychotree")
    res <- NextMethod()
    if (is.vector(res)){
        return(matrix(res, nrow = 1L, dimnames = list("1", names(res))))
    }
    res
}

#' @rdname itempar.PlackettLuce
#' @method itempar PLADMM
#' @export
itempar.PLADMM <- function(object, ref = NULL, alias = TRUE, vcov = TRUE,
                           log = FALSE, ...){
    # log worths s.t. worths sum to 1 as given by linear predictor
    coefs <- drop(object$x %*% object$coefficients)
    object_names <- names(coefs)
    n <- length(coefs)
    id <- seq_len(n)
    # set reference to one or more indices
    if (is.null(ref)) {
        ref <- id
    } else if (is.vector(ref)){
        if (any(ref %in% object_names)) ref <- match(ref, object_names)
        if (!all(ref %in% id))
            stop("Could not match 'ref' to item names")
    } else if (is.matrix(ref)) {
        stop("Handling of contrast matrices in argument 'ref' currently not ",
             "implemented for itempar.PLADMM().")
    } else stop("Argument 'ref' is misspecified (see ?itempar.PLADMM ",
                "for possible values).")
    # define parameters
    if (log){
        # based on contrasts
        D <- diag(n)
        D[, ref] <- D[, ref] - 1L/length(ref)
        coefs <- structure(drop(D %*% coefs), names = object_names)
    } else {
        # constrained so sum of ref = 1
        alpha <- exp(coefs)
        denom <- sum(alpha[ref])
        coefs <- alpha/denom
    }
    # define vcov
    if (vcov){
        # get vcov for log worths (X %*% beta)
        V0 <- object$x[, -1, drop = FALSE] %*% vcov(object) %*%
            t(object$x[, -1, drop = FALSE])
        if (log) {
            # vcov of contrasts
            V <- D %*% V0 %*% t(D)
        } else {
            # vcov of exp(coefs)
            V <- diag(alpha) %*% V0 %*% diag(alpha)
            # partial derivatives of scaled exp(coefs) wrt exp(coefs)
            D <- array(dim = dim(V))
            nonref <- setdiff(seq_along(id), ref)
            if (length(nonref)){
                D[, nonref] <- 0
                D[cbind(nonref, nonref)] <- 1L/denom
            }
            D[, ref] <- -alpha/denom^2
            D[cbind(ref, ref)] <- 1L/denom + D[cbind(ref, ref)]
            # vcov of scaled exp coefs
            V <- D %*% V %*% t(D)
        }
        dimnames(V) <- list(object_names, object_names)
    }
    # remove aliased parameter if required
    if (!alias) {
        alias <- ref[1L]
        names(alias) <- names(coefs)[ref[1L]]
        coefs <- coefs[-alias]
        if (vcov) V <- V[-alias, -alias, drop = FALSE]
    }
    structure(coefs, class = "itempar", model = "PLADMM",
              ref = ref, alias = alias, vcov = if (vcov) V)
}
