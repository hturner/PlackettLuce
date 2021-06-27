#' @method predict PLADMM
#' @importFrom stats as.formula model.matrix terms
#' @export
predict.PLADMM <- function(object, newdata = NULL,
                           type = c("lp", "itempar"),
                           se.fit = FALSE,
                           ...){ #na.action?
    type <- match.arg(type)
    # if newdata, create new X matrix
    if (!is.null(newdata)){
        if (se.fit) object$vcov <- vcov(object) # vcov based on original X matrix
        # create new model matrix
        worth_formula <- formula(object)
        environment(worth_formula) <- parent.frame()
        model_data <- model.frame(worth_formula, newdata, #na.action?
                                  xlev = object$xlevels)
        object$x <- model.matrix(worth_formula, model_data,
                                 contrasts.arg = object$contrasts)
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
