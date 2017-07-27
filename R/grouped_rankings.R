# Currently just conversion from paircomp object
#' @rdname grouped_rankings
#' @export
as.grouped_rankings <- function(x, verbose = TRUE, ...){
    UseMethod("as.grouped_rankings")
}

#' @rdname grouped_rankings
#' @method as.grouped_rankings paircomp
#' @export
as.grouped_rankings.paircomp <- function(x, verbose = TRUE, ...){
    if (attr(x, "mscale")[1] < -1) {
        warning("strength of preference ignored")
        x <- sign(x)
    }
    id <- which(!is.na(as.matrix(x)), arr.ind = TRUE)
    ncomp <- nrow(id)
    nobj <- length(attr(x, "labels"))
    pairs <- which(upper.tri(diag(nobj)), arr.ind = TRUE)
    rankings <- matrix(0, nrow = ncomp, ncol = nobj,
                       dimnames = list(NULL, attr(x, "labels")))
    x <- as.matrix(x)[id]
    rankings[cbind(seq_len(ncomp), pairs[,1][id[,2]])] <- ifelse(x == -1, 2, 1)
    rankings[cbind(seq_len(ncomp), pairs[,2][id[,2]])] <- ifelse(x == 1, 2, 1)
    rankings <- as.rankings.matrix(rankings, verbose = verbose)
    rankings <- structure(seq_len(max(id[,1])), rankings = rankings, index = id[,1])
    class(rankings) <- "grouped_rankings"
    rankings
}

# print method would be nice to separate rankings but ok
#' @method as.data.frame grouped_rankings
#' @export
as.data.frame.grouped_rankings <-
    function(x, row.names = NULL, optional = FALSE, ...,
             nm = paste(deparse(substitute(x), width.cutoff = 20L), collapse = " ")){
    value <- list(x)
    if (!optional) {
        names(value) <- nm
    }
    if (is.null(row.names) & !is.null(rownames(x))) row.names <- rownames(x)
    if (is.null(row.names)) {
        row.names <- .set_row_names(length(x))
    } else {
        if (is.object(row.names) || !is.integer(row.names))
            row.names <- as.character(row.names)
        if (anyNA(row.names))
            stop("row names contain missing values")
        if (anyDuplicated(row.names))
            stop(paste("duplicate row.names: ",
                       paste(unique(row.names[duplicated(row.names)]),
                             collapse = ", ")))
    }
    attr(value, "row.names") <- row.names
    class(value) <- "data.frame"
    value
}

#' @method [ grouped_rankings
#' @export
"[.grouped_rankings" <- function(x, i, ...) {
    # subset subjects
    value <- unclass(x)[i]
    keep <- attr(x, "index") %in% value
    # check if reduced rankings connected
    rankings <- suppressWarnings(
        as.rankings(attr(x, "rankings")[keep, , drop = FALSE]))
    structure(value,
              index = attr(x, "index")[keep],
              rankings = rankings,
              class = "grouped_rankings")
}
