print.PlackettLuce0 <- function(x,
                               digits = max(3, getOption("digits") - 3),
                               ...) {
    cat("Call: ", deparse(x$call), "\n", sep = "", fill = TRUE)
    if (length(coef.PlackettLuce0(x))) {
        cat("Coefficients:\n")
        print.default(format(coef.PlackettLuce0(x), digits = digits),
                      print.gap = 2L, quote = FALSE)
    }
    else cat("No coefficients\n")
    invisible(x)
}
