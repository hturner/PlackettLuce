#' @importFrom stats AIC deviance df.residual pnorm
#' @method summary PlackettLuce
#' @export
summary.PlackettLuce <- function(object,
                                 ...) {
    coefs <- coef(object)
    se <- sqrt(diag(vcov(object)))
    ref <- attr(coefs, "ref")
    if (is.numeric(ref)) se[ref] <- NA
    tvalue <- coefs/se
    pvalue <- 2 * pnorm(-abs(tvalue))
    coefficients <- cbind(coefs, se, tvalue, pvalue)
    dimnames(coefficients) <- list(names(coefs),
                                   c("Estimate", "Std. Error",
                                     "z value", "Pr(>|z|)"))
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
                                        digits = max(3L, getOption("digits") - 3L),
                                        ...) {
    cat("Call: ", deparse(x$call), "\n", sep = "", fill = TRUE)

    if (length(coef(x))) {
        cat("Coefficients:\n")
        printCoefmat(x$coefficients, digits = digits, na.print = "NA", ...)
    }
    else cat("No coefficients\n")

    cat("\nResidual deviance: ",
        format(x$deviance, digits = max(5, digits + 1)), " on ",
        format(x$df.residual, digits = max(5, digits + 1)),
        " degrees of freedom\n")
    cat("AIC: ", format(x$aic, digits = max(4, digits + 1)), "\n")

    cat("Number of iterations: ",  x$iter, "\n", sep = "")
    invisible(x)
}
