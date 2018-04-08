#' @importFrom stats coef
#' @method print PlackettLuce
#' @export
print.PlackettLuce <- function(x,
                               digits = max(3, getOption("digits") - 3),
                               ...) {
    cat(format_call(x$call), sep = "\n")

    cat("\nCoefficients:\n")
    print.default(format(coef(x), digits = digits),
                  print.gap = 2L, quote = FALSE)

    invisible(x)
}

format_call <- function(x){
    w <- getOption("width") - 6
    string <- deparse(x, width.cutoff = w)
    string[1] <- paste("Call:", string[1])
    # minimum width in deparse is 20
    while (w >= 21 & max(nchar(string)) > getOption("width")){
        w <- w - 1
        string <- deparse(x, width.cutoff = w)
        string[1] <- paste("Call:", string[1])
    }
    string
}
