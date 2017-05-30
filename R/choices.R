#' Choices and alternatives
#'
#' coerce a matrix of rankings to a list of choices, alternatives, and
#' rankings. The choices and the corresponding alternatives is the
#' exchangeable bit of the Plackett-Luce with ties.
#'
#' @param rankings a matrix of dense rankings.
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
#' tibble::as.tibble(coded_choices)
#' @export
as.choices <- function(rankings, names = FALSE) {
    N <- ncol(rankings)
    M <- t(Matrix(rankings, sparse = TRUE))
    J <- apply(M, 2, max)
    onames <- colnames(rankings)
    opt <- seq_len(N)
    if (names & !is.null(onames)) {
        opt <- onames
    }
    choices <- alternatives <-  list()
    rankings <- c()
    for (j in seq_len(max(J))) {
        ## j-th choices
        cho <- apply((M == j)[, J >= j, drop = FALSE], 2, function(z) opt[z])
        if (is.matrix(cho)) {
            cho <- split(cho, col(cho))
        }
        choices <- c(choices, cho)
        ## j-th alternatives
        alt <- apply((M > j - 1)[, J >= j, drop = FALSE], 2, function(z) opt[z])
        if (is.matrix(alt)) {
            alt <- split(alt, col(alt))
        }
        alternatives <- c(alternatives, alt)
        rankings <- c(rankings, which(J >= j))
    }
    ii <- order(rankings)
    out <- list(choices = choices[ii], alternatives = alternatives[ii], ranking = rankings[ii])
    attr(out, "nchoices") <- length(choices)
    attr(out, "objects") <- matrix(c(seq_len(N), onames), ncol = 2)
    class(out) <- c("choices", class(out))
    out
    ## Alow weights per choice/alternatives combination?
}

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
