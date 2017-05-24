#' @importFrom stats coef
#' @export
print.PlackettLuce <- function (x,
                                digits = max(3, getOption("digits") - 3),
                                ...)
{
    cat("Call: ", deparse(x$call), "\n", sep = "", fill = TRUE)
    if (length(coef(x))) {
        cat("Coefficients:\n")
        print.default(format(coef(x), digits = digits), print.gap = 2L,
                      quote = FALSE)
    }
    else cat("No coefficients\n")
    invisible(x)
}
