#' Grouped Rankings Object
#'
#' Create an object of class \code{"grouped_rankings"} which associates a
#' subject index with an object of class \code{"rankings"}. This allows the
#' rankings to be linked to subject covariates as the basis for
#' model-based recursive partitioning, see \code{\link{pltree}}.
#'
#' @param rankings a \code{\link{rankings}} object or an object that can be
#' coerced by \code{as.rankings}.
#' @param index a numeric vector of length equal to the number of rankings
#' specifying the subject for each ranking.
#' @param x an object that can be coerced to a \code{"grouped_rankings"} object
#' for \code{as.group_rankings}, or a \code{"grouped_rankings"} object for
#' \code{format}.
#' @param max the maximum number of rankings to format per subject.
#' @param width the maximum width in number of characters to format each
#' ranking.
#' @param ... additional arguments passed on to \code{\link{as.rankings}}
#' by \code{grouped_rankings} or \code{as.grouped_rankings}; unused by
#' \code{format}.
#' @return an object of class \code{"grouped_rankings"}.
#' @seealso \code{\link{pltree}}
#' @examples
#'
#' # ungrouped rankings (5 rankings, 4 items)
#' R <- as.rankings(matrix(c(1, 2, 0, 0,
#'                           0, 2, 1, 0,
#'                           0, 0, 1, 2,
#'                           2, 1, 0, 0,
#'                           0, 1, 2, 3), ncol = 4, byrow = TRUE))
#' length(R)
#' R
#'
#' # grouped rankings (1st 3 from subject 1, next 2 from subject 2)
#' G <- grouped_rankings(R, c(1, 1, 1, 2, 2))
#' length(G)
#' ## by default up to 2 rankings are shown per subject, "..." indicates if
#' ## there are further rankings
#' G
#' print(G, max = 1)
#' @export
grouped_rankings <- function(rankings, index, ...){
    if (!inherits(rankings, "rankings"))
        rankings <- as.rankings(rankings, ...)
    if (!(is.vector(index) & length(index) == nrow(rankings)))
        stop("index must be a vector with length equal to rankings")
    index <- as.numeric(index)
    structure(seq_len(max(index)), rankings = rankings, index = index,
              class = "grouped_rankings")
}

#' @rdname grouped_rankings
#' @export
as.grouped_rankings <- function(x, ...){
    UseMethod("as.grouped_rankings")
}

#' @rdname grouped_rankings
#' @method as.grouped_rankings paircomp
#' @export
as.grouped_rankings.paircomp <- function(x, ...){
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
    rankings <- as.rankings.matrix(rankings, ...)
    structure(seq_len(max(id[,1])), rankings = rankings, index = id[,1],
              class = "grouped_rankings")
}

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
    # renumber indices to match rows of new object
    index <- match(attr(x, "index")[keep], value)
    value <- match(value, value)
    # check if reduced rankings connected
    rankings <- suppressWarnings(
        as.rankings(unclass(attr(x, "rankings"))[keep, , drop = FALSE]))
    structure(value,
              index = index,
              rankings = rankings,
              class = "grouped_rankings")
}

#' @method print grouped_rankings
#' @export
print.grouped_rankings <- function(x, ...){
    print.default(format(x, ...))
}

#' @rdname grouped_rankings
#' @method format grouped_rankings
#' @export
format.grouped_rankings <- function(x, max = 2, width = 20, ...){
    tab <- tabulate(attr(x, "index"))
    rep <- numeric(length(attr(x, "index")))
    rep[order(attr(x, "index"))] <- sequence(tab)
    R <- attr(x, "rankings")[rep <= max, ]
    char <- format.rankings(R, width = width)
    value <- sapply(split(char, attr(x, "index")[rep <= max]), paste,
                    collapse = ", ")
    # add ... if more than max rankings
    trunc <- tab > max
    value[trunc] <- paste0(value[trunc], ", ...")
    value
}
