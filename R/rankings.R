#' Rankings Object
#'
#' Create a \code{"rankings"} object from data or convert a matrix of rankings
#' or ordered items to a \code{"rankings"} object.
#'
#' Each ranking in the input data will be converted to a dense ranking, which
#' rank items from 1 (first place) to \eqn{n_r} (last place). Items not ranked
#' should have a rank of 0. Tied items are given the same rank with no rank
#' skipped. For example {1, 0, 2, 1}, ranks the first and fourth items in
#' first place and the third item in second place; the second item is unranked.
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
#' row per ranking, or an object that can be coerced to such as matrix; for
#' \code{[} and \code{format}, a \code{"rankings"} object.
#' @param freq an optional column index (number, character or logical)
#' specifying a column of \code{x} that holds ranking frequencies, or a vector
#' of ranking frequencies.
#' @param input for \code{as.rankings}, whether each row in the input matrix
#' contains a numeric \code{"ranking"} (dense, standard/modified competition or
#' fractional ranking) or an \code{"ordering"}, i.e. the items ordered by rank.
#' @param labels for \code{input = "ordering"} an optional vector of labels for
#' the items. If \code{NULL}, the items will be labelled by the sorted unique
#' values of \code{x}.
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
#' with the following attributes:
#' \item{freq}{The frequencies of aggregated rankings.}
#' \item{omit}{The indices of any rankings that were omitted due to
#' insufficient data (less than two non-missing ranks).}
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
    if (ncol(data) != 3L) stop("id, item and rank must specify columns in data")
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
    id <- paste(data[[1L]], data[[2L]], sep = ":")
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
            ranks <- data[[3L]][id == dups[i]]
            # keep first if equal/consecutive ranks,
            # else drop all (inconsistent)
            keep[i] <- diff(range(ranks)) < 2L
        }
        include <- !id %in% dups
        first <- !include & !dup
        first[first] <- keep
        data <- data[include | first,]
    }
    # create rankings matrix (do not assume any form of ranking)
    lev1 <- sort(unique(data[[1L]]))
    lev2 <- sort(unique(data[[2L]]))
    R <- matrix(0L, nrow = length(lev1), ncol = length(lev2),
                dimnames = list(lev1, lev2))
    R[cbind(match(data[[1L]], lev1), match(data[[2L]], lev2))] <- data[[3L]]
    # convert to dense rankings and remove rankings with less than 2 items
    res <- as.rankings.matrix(R, verbose = verbose)
    if (length(attr(res, "omit")) && any(!complete)){
        attr(res, "omit") <- intersect(nm, c(attr(res, "omit"), omit))
    }
    res
}

#' @rdname rankings
#' @export
as.rankings <- function(x,
                        verbose = TRUE, ...){
    UseMethod("as.rankings")
}

#' @rdname rankings
#' @export
as.rankings.default <- function(x,
                                freq = NULL,
                                input = c("ranking", "ordering"),
                                aggregate = TRUE,
                                labels = NULL,
                                verbose = TRUE, ...){
    x <- as.matrix(x)
    as.rankings.matrix(x, freq = freq, input = input, aggregate = aggregate,
                       labels = labels, verbose = verbose, ...)
}

#' @rdname rankings
#' @export
as.rankings.preflib <- function(x, verbose = TRUE, ...){
    as.rankings.matrix(as.matrix(x[, -1]), freq = x[, 1], input = "ordering",
                       labels = attr(x, "item"), verbose = verbose, ...)
}

#' @rdname rankings
#' @export
as.rankings.matrix <- function(x,
                               freq = NULL,
                               input = c("ranking", "ordering"),
                               aggregate = TRUE,
                               labels = NULL,
                               verbose = TRUE, ...){
    input <- match.arg(input, c("ranking", "ordering"))
    if (!is.null(freq) && (length(freq) == 1 | is.logical(freq))) {
        freq_id <- seq(ncol(x))[freq]
        if (length(freq_id) != 1)
            stop("`freq` should identify exactly one column of `x`, found\n",
                 freq_id)
        freq <- unname(unlist(x[, freq_id]))
        x <- x[, -freq_id]
    }
    if (mode(x) != "numeric" && input == "ranking"){
        stop("values should be numeric ranks for `input = ranking`")
    }
    if (input == "ordering"){
        # define items, N.B. matrix cells may be vectors; may have NAs
        if (mode(x[[1]]) == "character"){
            if (is.null(labels)) {
                item <- labels <- sort(unique(c(x)))
            } else item <- labels
            m <- length(item)
        } else {
            m <- max(unlist(x), na.rm = TRUE)
            item <- seq_len(m)
        }
        # convert ordered items to dense ranking
        if (mode(x) == "list"){
            # i.e. there are ties
            x <- t(apply(x, 1L, function(ordering){
                g <- rep(seq_along(ordering), lengths(ordering))
                r <- match(item, unlist(ordering), nomatch = 0L)
                r[r != 0L] <- g[r[r != 0L]]
                r
            }))
        } else {
            x <- t(apply(x, 1L, function(ordering) {
                match(item, ordering, nomatch = 0L)
            }))
        }
        if (!is.null(labels)){
            if (length(labels) > m){
                unused <- length(labels) - m
                x <- cbind(x, matrix(0L, nrow = nrow(x), ncol = unused))
            }
            colnames(x) <- labels
        }
    } else if (NCOL(x) >= 2L) {
            # check rankings are dense rankings, recode if necessary
            x <- checkDense(x, verbose = verbose)
    }
    # remove rankings with less than 2 items
    omit_id <- which(rowSums(x > 0L) < 2L)
    if (length(omit_id)){
        if (verbose)
            message("Removed rankings with less than 2 items")
        if (!is.null(rownames(x))){
            omit <- rownames(x)[omit_id]
        } else omit <- omit_id
        x <- x[-omit_id, , drop = FALSE]
        if (!is.null(freq)) freq <- freq[-omit_id]
    }
    # add item names if necessary
    if (is.null(colnames(x))) colnames(x) <- seq_len(ncol(x))
    mode(x) <- "integer"
    # aggregating
    out <- structure(x, freq = freq, omit = if (length(omit_id)) omit else NULL,
                    class = "rankings")
    if (aggregate){
        aggregate(out)
    } else out
}

#' @rdname rankings
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
        return(structure(x, freq = freq, class = "rankings"))
    } else {
        attr(x, "freq") <- seq_len(length(r))
        return(x)
    }
}

checkDense <- function(x, verbose = TRUE){
    # replace NA with 0 (unranked)
    x[is.na(x)] <- 0
    # check rankings are dense rankings
    nRank <- apply(x, 1L, function(x) length(unique(x[x > 0L])))
    maxRank <- apply(x, 1L, max)
    bad <- maxRank != nRank
    # recode any ranking not in dense form
    if (any(bad)){
        if (verbose) message("Recoded rankings that are not in dense form")
        x[bad, ] <- t(apply(x[bad, , drop = FALSE], 1L, function(x) {
            id <- x > 0L
            x[id] <- match(x[id], sort(unique(x[id])))
            x
        }))
    }
    x
}

#' @method as.data.frame rankings
#' @export
as.data.frame.rankings <- function(x, row.names = NULL, optional = FALSE, ...,
                                   freq = c("last", "first"),
                                   col.names = colnames(x)){
    value <- asplit(x, 2)
    freq <- match.arg(freq)
    if (freq == "first") {
        value <- c(list(attr(x, "freq")), value)
        if (length(value) > length(col.names)) col.names <- c("freq", col.names)
    } else {
        value <- c(value, list(attr(x, "freq")))
        if (length(value) > length(col.names)) col.names <- c(col.names, "freq")
    }
    if (!optional) {
        names(value) <- col.names
    } else names(value) <- make.names(col.names)
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
        attr(value, "freq") <- attr(x, "freq")[i]
    } else {
        # subset items (never drop)
        if (missing(i)) i <- TRUE
        value <- .subset(x, i, j, drop = FALSE)
        # recode and drop items with <=2 items if necessary
        if (as.rankings && ncol(value) != ncol(x)) {
            value <- suppressWarnings(as.rankings.matrix(value,
                                                         freq = attr(x, "freq"),
                                                         ...))
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
format.rankings <- function(x, width = 40L, ...){
    f <- function(i, items) {
        obj <- items[i != 0L]
        i <- i[i != 0L]
        ord <- order(i)
        if (length(obj) > 1L){
            op <- ifelse(diff(i[ord]) == 0L, " = ", " > ")
            paste(obj[ord], c(op, ""), sep = "", collapse = "")
        } else obj
    }
    value <- apply(x, 1L, f, colnames(x))
    nc <- nchar(value)
    trunc <- nc > width
    value[trunc] <- paste(strtrim(value[trunc], width - 4), "...")
    value
}

#' @method rbind rankings
#' @rdname rankings
#' @export
rbind.rankings <- function(..., labels = NULL){
    # check contain the same items
    R <- list(...)
    nm <- lapply(R, colnames)
    ref <- nm[[1L]]
    ok <- vapply(nm, identical, TRUE, ref)
    if (any(!ok)){
        if (is.null(labels)) labels <- sort(unique(unlist(nm)))
        R <- lapply(R, function(x){
            R <- matrix(0L, nrow = nrow(x), ncol = length(labels),
                        dimnames = list(NULL, labels))
            R[, colnames(x)] <- x
            R
        })
    }
    # ranking attributes
    n <- vapply(R, length, numeric(1))
    freq <- lapply(R, attr, "freq")
    # rbind ranking values
    R <- do.call("rbind", lapply(R, unclass))
    # if any aggregated, reaggregate if necessary
    agg <- !vapply(freq, is.null, logical(1))
    if (any(agg)){
        for (i in seq_along(freq)){
            if (!agg[i]) freq[[i]] <- rep.int(1L, length(n[i]))
        }
        R <- structure(R, freq = unlist(freq), class = "rankings")
        aggregate(R)
    } else {
        structure(R, class = "rankings")
    }
}
