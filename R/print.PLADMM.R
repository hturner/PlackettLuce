#' @importFrom stats coef
#' @method print PLADMM
#' @export
print.PLADMM <- function(x,
                         digits = max(3L, getOption("digits") - 3L),
                         ...) {
    cat(format_call(x$call), sep = "\n")

    cat("\nCoefficients:\n")
    print.default(format(coef(x), digits = digits),
                  print.gap = 2L, quote = FALSE)

    invisible(x)
}
