#' Choices and alternatives
#'
#' coerce a matrix of rankings to a list of choices, alternatives, and
#' rankings. The choices and the corresponding alternatives is the
#' exchangeable bit of the Plackett-Luce with ties.
#'
#' @param rankings a \code{"\link{rankings}"} object, or an object that can be
#' coerced by \code{as.rankings}.
#' @param names logical: if \code{TRUE} use the object names in the returned
#' \code{"choices"} object, else use object indices.
#'
#' @examples
#' R <- matrix(c(1, 2, 0, 0,
#'               4, 1, 2, 3,
#'               2, 1, 1, 1,
#'               1, 2, 3, 0,
#'               2, 1, 1, 0,
#'               1, 0, 3, 2), nrow = 6, byrow = TRUE)
#' colnames(R) <- c("apple", "banana", "orange", "pear")
#' actual_choices <- as.choices(R, names = TRUE)
#' coded_choices <- as.choices(R, names = FALSE)
#' attr(coded_choices, "objects")
#'
#' ## Coercion to tibble is straightforwards
#' if (require(tibble)){
#'     as.tibble(coded_choices)
#' }
#' @export
as.choices <- function(rankings, names = FALSE) {
    if (!inherits(rankings, "rankings")) rankings <- as.rankings(rankings)
    N <- ncol(rankings)
    J <- apply(rankings, 1, max)
    onames <- colnames(rankings)
    opt <- seq_len(N)
    if (names & !is.null(onames)) {
        opt <- onames
    }
    choices <- alternatives <- list()
    ranking <- c()
    for (j in seq_len(max(J))) {
        ## j-th choices
        cho <- apply((rankings == j)[J >= j, , drop = FALSE], 1, function(z) opt[z])
        if (is.matrix(cho)) {
            cho <- unname(split(cho, col(cho)))
        }
        choices <- c(choices, cho)
        ## j-th alternatives
        alt <- apply((rankings > j - 1)[J >= j, , drop = FALSE], 1,
                     function(z) opt[z])
        if (is.matrix(alt)) {
            alt <- unname(split(alt, col(alt)))
        }
        alternatives <- c(alternatives, alt)
        ranking <- c(ranking, which(J >= j))
    }
    ii <- order(ranking)
    out <- list(choices = choices[ii], alternatives = alternatives[ii],
                ranking = ranking[ii])
    attr(out, "nchoices") <- length(choices)
    attr(out, "objects") <- onames
    class(out) <- c("choices", class(out))
    out
    ## Alow weights per choice/alternatives combination?
}

## extend to print subset?
print.choices <- function(x, ...) {
    rankings <- x$ranking
    for (i in unique(rankings)) {
        cat("Ranking:", i, "\n")
        cat("-------------- \n")
        ccho <- x$choices[rankings == i]
        calt <- x$alternatives[rankings == i]
        for (j in seq_along(ccho)) {
            ch <- paste0("{", paste(ccho[[j]], collapse = ", "), "}")
            al <- paste0("{", paste(calt[[j]], collapse = ", "), "}")
            cat(ch, "from", al, "\n")
        }
        cat("============== \n")
    }
}
