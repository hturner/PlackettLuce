#' Rankings Objects
#'
#' Create a \code{"rankings"} object from data or convert a matrix of rankings
#' to a \code{"rankings"} object. These functions validate that the data are
#' (possibly partial) dense rankings, recoding as necessary.
#'
#' Each ranking \eqn{r} should rank items from 1 (first place) to \eqn{n_r}
#' (last place). Items not ranked should have a rank of 0. Tied items are given
#' the same rank with no rank skipped. For example {1, 0, 2, 1}, ranks the first
#' and fourth items in first place and the third item in second place; the
#' second item is unranked.
#'
#' If the ranks are not in dense form, they will be recoded, e.g. {2, 2, 0, 4}
#' will become {1, 1, 0, 2}.
#'
#' Rankings with less than 2 items are dropped.
#'
#' The method for \code{[} will return a reduced rankings object by default,
#' recoding and dropping invalid rankings as necessary. To extract rows
#' and/or columns of the rankings as a matrix or vector,
#' set \code{as.rankings = FALSE}, see examples.
#' @param data a data frame with columns specified by \code{id}, \code{item} and
#' \code{rank}.
#' @param id an index of \code{data} specifying the column containing ranking
#' IDs.
#' @param item an index of \code{data} specifying the column containing item
#' IDs,
#' @param rank an index of \code{data} specifying the column containing item
#' ranks.
#' @param x for \code{as.rankings}, a matrix with one column per item and one
#' row per ranking; for \code{[} and \code{format}, a \code{"rankings"} object.
#' @param i indices specifying rankings to extract, as for \code{\link{[}}.
#' @param j indices specifying items to extract, as for \code{\link{[}}.
#' @param drop if \code{TRUE} return single row/column matrices as a vector.
#' @param as.rankings if \code{TRUE} return a rankings object, otherwise return
#' a matrix/vector.
#' @param verbose logical; if \code{TRUE} print messages when changes are made
#' to rankings data.
#' @param width the width in number of characters to format each ranking -
#' rankings that are too wide will be truncated.
#' @param ... further arguments passed to/from methods.
#'
#' @return a \code{"rankings"} object, which is a matrix of dense rankings
#' with one attribute \code{omit} the indices of any rankings that were
#' omitted due to insufficient data (less than two non-missing ranks).
#'
#' @examples
#' # create rankings from data in long form
#' x <- data.frame(ranking = c(rep(1:4, each = 4), 5, 5, 5),
#'                 letter = c(LETTERS[c(1:3, 3, 1:4, 2:5, 1:2, 1)], NA,
#'                            LETTERS[3:5]),
#'                 rank = c(4:1, rep(NA, 4), 3:4, NA, NA, 1, 3, 4, 2, 2, 2, 3))
#' # ranking 1 has different rank for same item, but order of items unambiguous
#' # all ranks are missing in ranking 2
#' # some ranks are missing in ranking 3
#' # ranking 4 has inconsistent ranks for two items and a rank with missing item
#' # ranking 5 is fine - an example of a tie
#' split(x, x$ranking)
#' # fix issues when creating rankings object
#' rankings(x, id = "ranking", item = "letter", rank = "rank")
#'
#' # convert existing matrix of rankings
#' R <- matrix(c(1, 2, 0, 0,
#'               4, 1, 2, 3,
#'               2, 1, 1, 1,
#'               1, 2, 3, 0,
#'               2, 1, 1, 0,
#'               1, 0, 3, 2), nrow = 6, byrow = TRUE)
#' colnames(R) <- c("apple", "banana", "orange", "pear")
#' R <- as.rankings(R)
#' # first three rankings
#' R[1:3,]
#' # exclude pear from the rankings
#' R[, -4]
#' # extract rankings 2 and 3 as numeric matrix
#' R[2:3, , as.rankings = FALSE]
#' # same as
#' unclass(R)[2:3,]
#' # extract rankings for item 1 as a vector
#' R[,1, as.rankings = FALSE]
#'
#' @importFrom stats complete.cases na.omit
#' @export
rankings <- function(data, id, item, rank, verbose = TRUE, ...){
    data <- data[c(id, item, rank)]
    if (ncol(data) != 3) stop("id, item and rank must specify columns in data")
    # id completely NA rankings
    complete <- complete.cases(data)
    # remove records with unknown id, item or rank
    if (any(!complete)){
        nm <- unique(data[[id]])
        omit <- setdiff(nm, unique(data[[id]][complete]))
        data <- data[complete, ]
        if (verbose){
            message("Removed records with unknown id, item or rank")
        }
    }
    # id duplicated items
    id <- paste(data[[1]], data[[2]], sep = ":")
    dup <- duplicated(id)
    if (any(dup)){
        if (verbose) {
            message("Duplicated items within rankings: ",
                    "removed redundant/inconsistent ranks.")
        }
        dups <- id[dup]
        ndups <- length(dups)
        keep <- logical(ndups)
        for (i in seq(ndups)){
            ranks <- data[[3]][id == dups[i]]
            # keep first if equal/consecutive ranks, else drop all (inconsistent)
            keep[i] <- diff(range(ranks)) < 2
        }
        include <- !id %in% dups
        first <- !include & !dup
        first[first] <- keep
        data <- data[include | first,]
    }
    # create rankings matrix (do not assume any form of ranking)
    lev1 <- sort(unique(data[[1]]))
    lev2 <- sort(unique(data[[2]]))
    R <- matrix(0, nrow = length(lev1), ncol = length(lev2),
                dimnames = list(lev1, lev2))
    R[cbind(match(data[[1]], lev1), match(data[[2]], lev2))] <- data[[3]]
    # convert to dense rankings and remove rankings with less than 2 items
    res <- as.rankings.matrix(R, verbose = verbose)
    if (length(attr(res, "omit")) && any(!complete)){
        attr(res, "omit") <- intersect(nm, c(attr(res, "omit"), omit))
    }
    res
}

#' @rdname rankings
#' @export
as.rankings <- function(x, verbose = TRUE, ...){
    UseMethod("as.rankings")
}

#' @rdname rankings
#' @export
as.rankings.matrix <- function(x, verbose = TRUE, ...){
    if (NCOL(x) >= 2) {
        # check rankings are dense rankings, recode if necessary
        x <- checkDense(x, verbose = verbose)
    }
    # remove rankings with less than 2 items
    id <- which(rowSums(x > 0) < 2)
    if (length(id)){
        if (verbose)
            message("Removed rankings with less than 2 items")
        if (!is.null(rownames(x))){
            omit <- rownames(x)[id]
        } else omit <- id
        x <- x[-id, , drop = FALSE]
    }
    # add item names if necessary
    if (is.null(colnames(x))) colnames(x) <- seq_len(ncol(x))
    structure(x, omit = if (length(id)) omit else NULL, class = "rankings")
}

checkDense <- function(x, verbose = TRUE){
    # check rankings are dense rankings
    nRank <- apply(x, 1, function(x) length(unique(x[x > 0])))
    maxRank <- apply(x, 1, max)
    bad <- maxRank != nRank
    # recode any ranking not in dense form
    if (any(bad)){
        if (verbose) message("Recoded rankings that are not in dense form")
        x[bad, ] <- t(apply(x[bad, , drop = FALSE], 1, function(x) {
            id <- x > 0
            x[id] <- match(x[id], sort(unique(x[id])))
            x
        }))
    }
    x
}

#' @method as.data.frame rankings
#' @export
as.data.frame.rankings <-
    function(x, row.names = NULL, optional = FALSE, ...,
             nm = paste(deparse(substitute(x), width.cutoff = 20L),
                        collapse = " ")){
    value <- list(x)
    if (!optional) {
        names(value) <- nm
    } else names(value) <- make.names(nm)
    if (is.null(row.names) & !is.null(rownames(x))) row.names <- rownames(x)
    if (is.null(row.names)) {
        row.names <- .set_row_names(nrow(x))
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

#' @method length rankings
#' @export
length.rankings <- function(x) {
   nrow(x)
}

#' @importFrom utils str
#' @export
utils::str

#' @method str rankings
str.rankings <- function(object, ...) {
    str(unclass(object))
}


#' @rdname rankings
#' @method [ rankings
#' @export
"[.rankings" <- function(x, i, j, ..., drop = TRUE, as.rankings = TRUE) {
    if (missing(j)) {
        if (missing(i)) return(x)
        # always a vector if picking out elements of rankings matrix
        if (is.matrix(i)) return(.subset(x, i))
        # else subset of rankings
        value <- .subset(x, i, TRUE, drop = FALSE)
    } else {
        # subset items (never drop)
        if (missing(i)) i <- TRUE
        value <- .subset(x, i, j, drop = FALSE)
        # recode if necessary
        if (as.rankings && ncol(value) != ncol(x)) {
            value <- suppressWarnings(as.rankings.matrix(value, ...))
        }
    }
    if (!as.rankings) {
        if (drop) return(drop(value))
        return(value)
    }
    structure(value, class = "rankings")
}

#' @method print rankings
#' @export
print.rankings <- function(x, ...){
    print.default(format(x, ...))
}

#' @method format rankings
#' @rdname rankings
#' @export
format.rankings <- function(x, width = 40, ...){
    f <- function(i, items) {
        obj <- items[i != 0]
        i <- i[i != 0]
        ord <- order(i)
        if (length(obj) > 1){
            op <- ifelse(diff(i[ord]) == 0, " = ", " > ")
            paste(obj[ord], c(op, ""), sep = "", collapse = "")
        } else obj
    }
    value <- apply(x, 1, f, colnames(x))
    nc <- nchar(value)
    trunc <- nc > width
    value[trunc] <- paste(strtrim(value[trunc], width - 4), "...")
    value
}
