#' @importFrom stats coef
#' @method print PlackettLuce
#' @export
print.PlackettLuce <- function(x,
                               digits = max(3, getOption("digits") - 3),
                               ...) {
    w <- getOption("width") - 6
    string <- deparse(x$call, width = w)
    string[1] <- paste("Call:", string[1])
    while (w >= 22 & max(nchar(string)) > getOption("width")){
        w <- w - 2
        string <- deparse(x$call, width = w)
        string[1] <- paste("Call:", string[1])
    }
    cat(string, sep = "\n")
    if (length(coef(x))) {
        cat("\nCoefficients:\n")
        print.default(format(coef(x), digits = digits),
                      print.gap = 2L, quote = FALSE)
    }
    else cat("\nNo coefficients\n")
    invisible(x)
}
