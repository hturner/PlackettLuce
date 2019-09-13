#' Aggregate Rankings
#'
#' Aggregate rankings, returning an `"aggregated_rankings"` object of the
#' unique rankings and their frequencies. The frequencies can be extracted via
#' the function `freq()`.
#'
#' @param x A [`"rankings"`][rankings] object for `aggregate()`; an
#' object that can be coerced to a \code{"aggregated_rankings"} object for
#' \code{as.aggregated_rankings()}, otherwise an `"aggregated_rankings"` object.
#' @param freq A vector of frequencies for rankings that have been previously
#' aggregated.
#' @param i indices specifying rankings to extract, as for \code{\link{[}}.
#' @param j indices specifying items to extract, as for \code{\link{[}}.
#' @param drop if \code{TRUE} return single row/column matrices as a vector.
#' @param as.aggregated_rankings if \code{TRUE} create an
#' `"aggregated_rankings"` object from the indexed rankings. Otherwise index
#' the underlying matrix of ranks and return in a data frame with the
#' corresponding frequencies.
#' @param ... Additional arguments, currently unused.
#' @return A data frame of class `"aggregated_rankings"`, with columns
#' \item{ranking}{ A [`"rankings"`][rankings] object of the unique rankings.}
#' \item{freq}{The corresponding frequencies.}
#' Methods are available for [`rbind()`] and [`as.matrix()`].
#' @seealso [preflib()] for an object that can be coerced to an
#' `"aggregated_rankings"` object.
#' @examples
#' # create a rankings object with duplicated rankings
#' R <- matrix(c(1, 2, 0, 0,
#'               0, 1, 2, 3,
#'               2, 1, 1, 0,
#'               1, 2, 0, 0,
#'               2, 1, 1, 0,
#'               1, 0, 3, 2), nrow = 6, byrow = TRUE)
#' colnames(R) <- c("apple", "banana", "orange", "pear")
#' R <- as.rankings(R)
#'
#' # aggregate the rankings
#' A <- aggregate(R)
#'
#' # subsetting applies to the rankings, e.g. first two unique rankings
#' A[1:2]
#'
#' # (partial) rankings of items 2 to 4 only
#' A[, 2:4]
#'
#' # convert to a matrix
#' as.matrix(A)
#'
#' # frequencies are automatically used as weights by PlackettLuce()
#' mod <- PlackettLuce(A)
#' mod$weights
#' @name aggregate
NULL

#' @method aggregate rankings
#' @rdname aggregate
#' @export
aggregate.rankings <- function(x, freq = NULL, ...){
    if (getRversion() < "3.6.0") {
        r <- lapply(seq(nrow(x)), function(i) x[i, , as.rankings = FALSE])
    } else r <- asplit(unclass(x), 1L)
    dup <- duplicated(r)
    if (any(dup)){
        r_new <-r[!dup]
        r_id <- match(r, r_new)
        if (!is.null(freq)) {
            freq <- as.vector(rowsum(freq, r_id))
        } else freq <- tabulate(r_id)
        x <- do.call("rbind", r_new)
        rownames(x) <- NULL
        res <- data.frame(ranking = as.rankings(x), freq = freq)
    } else {
        if (is.null(freq)) freq <- rep.int(1, length(r))
        res <- data.frame(ranking = x, freq = freq)
    }
    structure(res, class = c("aggregated_rankings", class(res)))
}

#' @export
aggregate.aggregated_rankings <- function(x, ...){
    aggregate(x$rankings, x$freq)
}

#' @rdname aggregate
#' @export
as.aggregated_rankings <- function(x, ...){
    UseMethod("as.aggregated_rankings")
}

#' @rdname aggregate
#' @method [ aggregated_rankings
#' @export
"[.aggregated_rankings" <- function(x, i, j, ..., drop = FALSE,
                                    as.aggregated_rankings = TRUE) {
    ranking <-  x$ranking[i, j, ..., drop = drop,
                          as.rankings = as.aggregated_rankings]
    if (as.aggregated_rankings){
        aggregate(ranking, freq = x$freq[i])
    } else {
        if (!is.null(dim(i))) i <- i[,1]
        data.frame(ranking = ranking, freq = x$freq[i])
    }
}

#' @rdname aggregate
#' @export
freq <- function(x){
    if (is.list(x)) x[["freq"]]
    else NULL
}

#' @method as.matrix aggregated_rankings
#' @export
as.matrix.aggregated_rankings <- function(x, ...){
    res <- cbind(x$ranking, freq = x$freq)
    rownames(res) <- NULL
    res
}

#' @method rbind aggregated_rankings
#' @export
rbind.aggregated_rankings <- function(...){
    x <- list(...)
    aggregate(do.call("rbind", lapply(x, `[[`, "ranking")),
              unlist(lapply(x, `[[`, "freq")))
}


