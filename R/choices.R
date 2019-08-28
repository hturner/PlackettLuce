#' Choices Object
#'
#' Convert a set of rankings to a list of choices, alternatives, and
#' rankings. The choices and the corresponding alternatives make up the
#' exchangeable part of the Plackett-Luce with ties.
#'
#' @param rankings a \code{"\link{rankings}"} object, or an object that can be
#' coerced by \code{as.rankings}.
#' @param names logical: if \code{TRUE} use the object names in the returned
#' \code{"choices"} object, else use object indices.
#' @return A list of class \code{"choices"} with elements:
#' \item{choices}{ A list where each element represents the set of items
#' chosen for a single rank in the ranking.}
#' \item{alternatives}{ A list where each element represents the set of items
#' to choose from for a single rank in the ranking.}
#' \item{ranking}{ A list where each element represents the ranking that the
#' choice belongs to.}
#' The list stores the number of choices and the names of the objects as the
#' attributes \code{"nchoices"} and \code{"objects"} respectively.
#' @examples
#' R <- matrix(c(1, 2, 0, 0,
#'               4, 1, 2, 3,
#'               2, 1, 1, 1,
#'               1, 2, 3, 0,
#'               2, 1, 1, 0,
#'               1, 0, 3, 2), nrow = 6, byrow = TRUE)
#' colnames(R) <- c("apple", "banana", "orange", "pear")
#' actual_choices <- choices(R, names = TRUE)
#' coded_choices <- choices(R, names = FALSE)
#' attr(coded_choices, "objects")
#'
#' @export
choices <- function(rankings, names = FALSE) {
    # check rankings are valid
    if (!inherits(rankings, "rankings")) rankings <- as.rankings(rankings)
    # treat as matrix for faster indexing
    rankings <- unclass(rankings)
    N <- ncol(rankings)
    J <- apply(rankings, 1L, max)
    onames <- colnames(rankings)
    opt <- seq_len(N)
    if (names & !is.null(onames)) {
        opt <- onames
    }
    choices <- alternatives <- list()
    ranking <- c()
    for (j in seq_len(max(J))) {
        ## j-th choices
        cho <- apply((rankings == j)[J >= j, , drop = FALSE], 1L,
                     function(z) opt[z])
        if (is.matrix(cho)) {
            cho <- unname(split(cho, col(cho)))
        }
        choices <- c(choices, cho)
        ## j-th alternatives
        alt <- apply((rankings > j - 1L)[J >= j, , drop = FALSE], 1L,
                     function(z) opt[z])
        if (is.matrix(alt)) {
            alt <- unname(split(alt, col(alt)))
        }
        alternatives <- c(alternatives, alt)
        ranking <- c(ranking, which(J >= j))
    }
    ii <- order(ranking)
    nchoices <- length(choices)
    out <- data.frame(matrix(NA, nrow = nchoices, ncol = 0))
    out$choices <- choices[ii]
    out$alternatives <- alternatives[ii]
    out$ranking <- ranking[ii]
    attr(out, "nchoices") <- nchoices
    attr(out, "objects") <- onames
    class(out) <- c("choices", class(out))
    out
    ## Alow weights per choice/alternatives combination?
}

#' @method print choices
#' @export
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
