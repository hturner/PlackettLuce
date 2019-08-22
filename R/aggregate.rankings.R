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
        return(structure(x, freq = freq,
                         class = c("aggregated_rankings", "rankings")))
    } else {
        return(structure(x, freq = seq_len(length(r)),
                         class = c("aggregated_rankings", "rankings")))
    }
}

#' @rdname aggregate.rankings
#' @export
freq <- function(x){
    attr(x, "freq")
}

