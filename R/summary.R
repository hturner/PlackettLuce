#' @rdname  summaries
#' @importFrom stats AIC deviance df.residual pnorm
#' @method summary PlackettLuce
#' @export
summary.PlackettLuce <- function(object,
                                 ref = 1L,
                                 ...) {
    coefs <- coef(object, ref = ref)
    coefficients <- matrix(NA, nrow = length(coefs), ncol = 4L,
                           dimnames = list(names(coefs),
                                           c("Estimate", "Std. Error",
                                             "z value", "Pr(>|z|)")))
    coefficients[,1L] <- coefs
    se <- sqrt(diag(vcov(object, ref = ref, ...)))
    coefficients[names(se), 2L] <- se
    ref <- attr(coefs, "ref")
    if (length(ref) == 1L) coefficients[attr(coefs, "ref"), 2L] <- NA
    coefficients[,3L] <- coefficients[,1L]/coefficients[,2L]
    coefficients[,4L] <- 2L * pnorm(-abs(coefficients[, 3L]))
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
        format(x$deviance, digits = max(5L, digits + 1L)), " on ",
        format(x$df.residual, digits = max(5L, digits + 1L)),
        " degrees of freedom", fill = TRUE, sep = "")
    cat("AIC: ", format(x$aic, digits = max(4L, digits + 1L)), "\n")

    if (!is.null(x$call$gamma)){
        cat("Number of outer iterations: ",  x$iter, "\n", sep = "")
    } else  cat("Number of iterations: ",  x$iter, "\n", sep = "")
    invisible(x)
}
