#' Aggregate Rankings
#'
#' Aggregate rankings, returning an `"aggregated_rankings"` object of the
#' unique rankings and their frequencies. The frequencies can be extracted via
#' the function `freq()`.
#'
#' @param x A [`"rankings"`][rankings] or `"aggregated_rankings"` object.
#' @param ... Additional arguments, currently unused.
#'
#' @method aggregate rankings
#' @export
aggregate.rankings <- function(x, ...){
    r <- asplit(unclass(x), 1L)
    dup <- duplicated(r)
    if (any(dup)){
        r_new <-r[!dup]
        r_id <- match(r, r_new)
        if (!is.null(attr(x, "freq"))) {
            freq <- as.vector(rowsum(attr(x, "freq"), r_id))
        } else freq <- tabulate(r_id)
        x <- do.call("rbind", r_new)
        res <- data.frame(ranking = as.rankings(x), freq = freq)
    } else {
        res <- data.frame(ranking = as.rankings(x), freq = seq_len(length(r)))
    }
    structure(res, class = c("aggregated_rankings", class(res)))
}

#' @rdname aggregate.rankings
#' @export
freq <- function(x){
    if (is.list(x)) x[["freq"]]
    else NULL
}

#' @rdname aggregate.rankings
#' @method as.data.frame aggregated.rankings
#' @export
as.matrix.aggregated_rankings <- function(x, ...){
    res <- cbind(x$ranking, freq = x$freq)
    rownames(res) <- rownames(x)
    res
}

