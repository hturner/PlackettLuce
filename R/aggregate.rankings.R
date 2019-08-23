#' Aggregate Rankings
#'
#' Aggregate rankings, returning an `"aggregated_rankings"` object of the
#' unique rankings and their frequencies. The frequencies can be extracted via
#' the function `freq()`.
#'
#' @param x A [`"rankings"`][rankings] or `"aggregated_rankings"` object.
#' @param freq A vector of frequencies for rankings that have been previously
#' aggregated.
#' @param ... Additional arguments, currently unused.
#'
#' @method aggregate rankings
#' @export
aggregate.rankings <- function(x, freq = NULL, ...){
    r <- asplit(unclass(x), 1L)
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
        res <- data.frame(ranking = x,
                          freq = rep.int(1, length(r)))
    }
    structure(res, class = c("aggregated_rankings", class(res)))
}

#' @rdname aggregate.rankings
#' @export
aggregate.aggregated_rankings <- function(x, ...){
    aggregate(x$rankings, x$freq)
}

#' @rdname aggregate.rankings
#' @export
freq <- function(x){
    if (is.list(x)) x[["freq"]]
    else NULL
}

#' @rdname aggregate.rankings
#' @method as.matrix aggregated_rankings
#' @export
as.matrix.aggregated_rankings <- function(x, ...){
    res <- cbind(x$ranking, freq = x$freq)
    rownames(res) <- rownames(x)
    res
}

#' @rdname aggregate.rankings
#' @method rbind aggregated_rankings
#' @export
rbind.aggregated_rankings <- function(...){
    x <- list(...)
    aggregate(do.call("rbind", lapply(x, `[[`, "ranking")),
              unlist(lapply(x, `[[`, "freq")))
}

#' @rdname aggregate.rankings
#' @method [ aggregated_rankings
#' @export
"[.aggregated_rankings" <- function(x, i, j, ..., drop = TRUE,
                                    as.rankings = TRUE) {
    aggregate(x$ranking[i, j, ..., drop = TRUE, as.rankings = TRUE],
              freq = x$freq[i])
}
