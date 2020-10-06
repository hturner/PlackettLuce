#' Convert Objects to an Edge List
#'
#' A generic function to convert an object to an edge list of the corresponding
#' graph. The package provides a method for \code{"rankings"} objects.
#'
#' For a \code{"rankings"} object an edge is defined for consecutive pairs in
#' the ranking, with the winner specifying the \sQuote{from} vertex and the
#' loser specifying the \sQuote{to} vertex. For tied items an edge is defined
#' for all possible pairs between the winning and the losing group. Pairs are
#' ordered by rank combination, i.e. all pairs between items ranked first and
#' items ranked second, for all rankings, then all pairs between items ranked
#' second and items ranked third, etc.
#'
#' @param x a \code{\link{rankings}} object.
#' @param ... further argumjents passed to/from methods.
#'
#' @return a two column matrix
#'
#' @examples
#' X <- matrix(c(2, 1, 2, 1, 2,
#'               3, 2, 0, 0, 1,
#'               1, 0, 2, 2, 3), nrow = 3, byrow = TRUE)
#' as.edgelist(as.rankings(X))
#'
as.edgelist <- function(x, ...){
    UseMethod("as.edgelist")
}

as.edgelist.rankings <- function(x, ...){
    x <- unclass(x)
    maxRank <- max(x)
    nm <- colnames(x)
    res <- list()
    for (i in seq_len(maxRank)){
        res[[i]] <- list()
        for(j in seq_len(nrow(x))){
            if (!is.null(nm)) {
                res[[i]][[j]] <- nm[which(x[j, ] == i)]
            } else  res[[i]][[j]] <- which(x[j, ] == i)
        }
    }
    res <- unlist(res, recursive = FALSE)
    rep <- lengths(res)
    n <- nrow(x)
    m <- length(res)
    cbind(from = unlist(rep(res[seq(m - n)], rep[-seq(n)])),
          to = unlist(rep(res[-seq(n)], rep[seq(m - n)])))
}
