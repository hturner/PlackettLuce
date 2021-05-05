#' @method predict PLADMM
#' @importFrom stats as.formula model.matrix
#' @export
predict.PLADMM <- function(object, newdata = NULL,
                           type = c("lp", "itempar"),
                           se.fit = FALSE,
                           ...){ #na.action?
    type <- match.arg(type)
    # if newdata, create new X matrix
    if (!is.null(newdata)){
        if (se.fit) object$vcov <- vcov(object) # vcov based on original X matrix
        # allow for missing factor levels (avoid terms etc for now)
        X <- matrix(0, nrow = nrow(newdata), ncol = ncol(object$x),
                    dimnames = list(seq(nrow(newdata)), colnames(object$x)))
        X1 <- model.matrix(as.formula(object$call$formula), newdata)
        X[, colnames(X1)] <- X1
        object$x <- X
    }
    # if itempar return constrained item parameters
    if (type == "itempar"){
        res <- itempar(object, vcov = se.fit, ...)
        if (!se.fit) return(c(res))
        return(list(fit = c(res),
                    se.fit = sqrt(diag(attr(res, "vcov")))))
    }
    # else return linear predictor (same location as original fit)
    res <- drop(object$x %*% object$coefficients)
    if (!se.fit) return(res)
    V <- object$x[, -1, drop = FALSE] %*% vcov(object) %*%
        t(object$x[, -1, drop = FALSE])
    return(list(fit = res,
                se.fit = sqrt(diag(V))))
}

#' @method fitted PLADMM
#' @importFrom stats predict
#' @export
fitted.PLADMM <- function(object, ...){
    predict(object)
}
