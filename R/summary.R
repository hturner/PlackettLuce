#' @rdname  summaries
#' @importFrom stats AIC deviance df.residual pnorm
#' @method summary PlackettLuce
#' @export
summary.PlackettLuce <- function(object,
                                 ref = 1,
                                 ...) {
    coefs <- coef(object, ref = ref)
    coefficients <- matrix(NA, nrow = length(coefs), ncol = 4,
                           dimnames = list(names(coefs),
                                           c("Estimate", "Std. Error",
                                             "z value", "Pr(>|z|)")))
    coefficients[,1] <- coefs
    se <- sqrt(diag(vcov(object, ref = ref)))
    coefficients[names(se), 2] <- se
    ref <- attr(coefs, "ref")
    if (length(ref) == 1) coefficients[attr(coefs, "ref"), 2] <- NA
    coefficients[,3] <- coefficients[,1]/coefficients[,2]
    coefficients[,4] <- 2 * pnorm(-abs(coefficients[, 3]))
    structure(list(call = object$call,
                   deviance = deviance(object),
                   aic = AIC(object),
                   df.residual = df.residual(object),
                   iter = object$iter,
                   coefficients = coefficients,
                   df = object$rank),
              class = "summary.PlackettLuce")
}

#' @method print summary.PlackettLuce
#' @importFrom stats printCoefmat
#' @export
print.summary.PlackettLuce <-  function(x,
                                        digits = max(3L,
                                                     getOption("digits") - 3L),
                                        ...) {
    cat(format_call(x$call), sep = "\n")

    cat("\nCoefficients:\n")
    printCoefmat(x$coefficients, digits = digits, na.print = "NA", ...)

    cat("\nResidual deviance:  ",
        format(x$deviance, digits = max(5, digits + 1)), " on ",
        format(x$df.residual, digits = max(5, digits + 1)),
        " degrees of freedom", fill = TRUE, sep = "")
    cat("AIC: ", format(x$aic, digits = max(4, digits + 1)), "\n")

    cat("Number of iterations: ",  x$iter, "\n", sep = "")
    invisible(x)
}
