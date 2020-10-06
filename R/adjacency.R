#' Create an Adjacency Matrix for a set of Rankings
#'
#' Convert a set of rankings to an adjacency matrix summarising wins
#' and losses between pairs of items.
#'
#' For a \code{"rankings"} object based on N items, the adjacency matrix is an
#' N by N matrix, with element (i, j) being the number of times item i wins over
#' item j. For example, in the ranking \\{1\\} > \\{3, 4\\} > \\{2\\},
#' item 1 wins over items 2, 3, and 4, and items 3 and 4 win over item 2.
#'
#' If \code{weights} is specified, the values in the adjacency matrix are the
#' weighted counts.
#'
#' @param object a \code{\link{rankings}} object, or an object that can be
#' coerced by \code{as.rankings}.
#' @param weights an optional vector of weights for the rankings.
#' @param ... further arguments passed to/from methods.
#'
#' @return An N by N matrix, where N is the number of items that can be ranked.
#'
#' @examples
#' X <- matrix(c(2, 1, 2, 1, 2,
#'               3, 2, 0, 0, 1,
#'               1, 0, 2, 2, 3), nrow = 3, byrow = TRUE)
#' X <- as.rankings(X)
#' adjacency(X)
#'
#' adjacency(X, weights = c(1, 1, 2))
#'
#' @export
adjacency <- function(object, weights = NULL, ...){
    if (!inherits(object, "rankings")) object <- as.rankings(object)
    N <- ncol(object)
    if (is.null(weights)) {
        weights <- rep.int(1L, nrow(object))
    } else stopifnot(length(weights) == nrow(object))
    nset <- apply(object, 1L, max)
    m <- max(nset)
    nm <- colnames(object)
    X <- matrix(0.0, nrow = N, ncol = N, dimnames = list(nm, nm))
    for (i in 1L:m){
        r <- which(nset >= (i + 1L))
        for(j in r) {
            one <- object[j,] == i
            two <- object[j,] > i # > gives rest; == i + 1 gives next best
            X[one, two] <- X[one, two] + weights[j]
        }
    }
    structure(X, class = c("adjacency", "matrix"))
}
