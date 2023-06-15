#' Rankings Object
#'
#' Create a \code{"rankings"} object from data or convert a matrix of rankings
#' or ordered items to a \code{"rankings"} object.
#'
#' Each ranking in the input data will be converted to a dense ranking, which
#' rank items from 1 (first place) to \eqn{n_r} (last place). Items not ranked
#' should have a rank of 0 or `NA`. Tied items are given the same rank with no
#' rank skipped. For example {1, 0, 2, 1}, ranks the first and fourth items in
#' first place and the third item in second place; the second item is unranked.
#'
#' Records in `data` with missing `id` or `item` are dropped. Duplicated items
#' in the rankings are resolved if possible: redundant or inconsistent ranks
#' are set to `NA`. Rankings with only 1 item are set to `NA` (rankings with
#' zero items are automatically treated as `NA`). Any issues
#' causing records to be removed or recoded produce a message if
#' `verbose = TRUE`.
#'
#' For `as.rankings` with `input = "orderings"`, unused ranks may be filled with
#' zeroes for numeric `x` or `NA`. It is only necessary to have as many columns
#' as ranks that are used.
#'
#' The method for \code{[} will return a reduced rankings object by default,
#' recoding as dense rankings and setting invalid rankings to `NA` as necessary.
#' To extract rows and/or columns of the rankings as a matrix or vector,
#' set \code{as.rankings = FALSE}, see examples.
#' @param data a data frame with columns specified by \code{id}, \code{item} and
#' \code{rank}.
#' @param id an index of \code{data} specifying the column containing ranking
#' IDs.
#' @param item an index of \code{data} specifying the column containing item
#' IDs,
#' @param rank an index of \code{data} specifying the column containing item
#' ranks.
#' @param aggregate if `TRUE`, aggregate the rankings via
#' [`aggregate()`][aggregate.rankings] before returning.
#' @param x for \code{as.rankings}, a matrix with one column per item and one
#' row per ranking, or an object that can be coerced to such as matrix; for
#' \code{[} and \code{format}, a \code{"rankings"} object.
#' @param freq an optional column index (number, character or logical)
#' specifying a column of \code{x} that holds ranking frequencies, or a vector
#' of ranking frequencies. If provided, an `"aggregated_rankings"` object
#' will be returned.
#' @param index an optional column index (number, character or logical)
#' specifying a column of \code{x} that holds a grouping index, or a
#' numeric vector to for grouping. If provided, the rankings will be grouped by
#' [group()] before returning.
#' @param input for \code{as.rankings}, whether rows in the input matrix
#' contain numeric \code{"rankings"} (dense, standard/modified competition or
#' fractional rankings) or \code{"orderings"}, i.e. the items ordered by rank.
#' @param labels for \code{input = "orderings"} an optional vector of labels for
#' the items, corresponding to the sorted unique values of \code{x}.
#' @param items for \code{input = "orderings"}, a character vector specifying
#' the full set of items. Values in `x` are matched to this by value (if
#' character) or position (if numeric). Use [decode()] for orderings requiring
#' more complex decoding.
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
#' @return By default, a \code{"rankings"} object, which is a
#' matrix of dense rankings with methods for several generics including
#' [`aggregate`][aggregate.rankings], `[`, `format`, [rbind()] and
#' [as.matrix()].
#'
#' If the object is created with `aggregate = TRUE`, or ranking frequencies are
#' specified via `freq`, the rankings are post-processed to create an
#' `"aggregated_rankings"` object.
#'
#' If a group index is specified via `index`, the (possibly aggregated) rankings
#' are post-processed to create a `"grouped_rankings"` object.
#'
#' @examples
#' # create rankings from data in long form
#'
#' # example long form data
#' x <- data.frame(ranking = c(rep(1:4, each = 4), 5, 5, 5),
#'                 letter = c(LETTERS[c(1:3, 3, 1:4, 2:5, 1:2, 1)], NA,
#'                            LETTERS[3:5]),
#'                 rank = c(4:1, rep(NA, 4), 3:4, NA, NA, 1, 3, 4, 2, 2, 2, 3))
#'
#' # ranking 1 has different rank for same item, but order of items unambiguous
#' # all ranks are missing in ranking 2
#' # some ranks are missing in ranking 3
#' # ranking 4 has inconsistent ranks for two items and a rank with missing item
#' # ranking 5 is fine - an example of a tie
#' split(x, x$ranking)
#'
#' # fix issues when creating rankings object
#' rankings(x, id = "ranking", item = "letter", rank = "rank")
#'
#' # convert existing matrix of rankings
#'
#' R <- matrix(c(1, 2, 0, 0,
#'               4, 1, 2, 3,
#'               2, 1, 1, 1,
#'               1, 2, 3, 0,
#'               2, 1, 1, 0,
#'               1, 0, 3, 2), nrow = 6, byrow = TRUE)
#' colnames(R) <- c("apple", "banana", "orange", "pear")
#' R <- as.rankings(R)
#'
#' # first three rankings
#' R[1:3,]
#'
#' # exclude pear from the rankings
#' R[, -4]
#'
#' # extract rankings 2 and 3 as numeric matrix
#' R[2:3, , as.rankings = FALSE]
#'
#' # same as
#' as.matrix(R)[2:3,]
#'
#' # extract rankings for item 1 as a vector
#' R[,1, as.rankings = FALSE]
#'
#' @export
rankings <- function(data, id, item, rank, aggregate = FALSE,
                     verbose = TRUE, ...){
    data <- data[c(id, item, rank)]
    if (ncol(data) != 3L) stop("id, item and rank must specify columns in data")
    # remove records with unknown id or item
    unknown <- is.na(data[[id]]) | is.na(data[[item]])
    if (any(unknown)){
        data <- data[!unknown, ]
        if (verbose){
            message("Removed records with unknown id or item")
        }
    }
    # id duplicated items
    id <- paste(data[[1L]], data[[2L]], sep = ":")
    dup <- duplicated(id)
    if (any(dup)){
        if (verbose) {
            message("Duplicated items within rankings: ",
                    "set redundant/inconsistent to `NA`.")
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
        data[!include & !first, rank] <- NA
    }
    # create rankings matrix (do not assume any form of ranking)
    lev1 <- sort(unique(data[[1L]]))
    lev2 <- sort(unique(data[[2L]]))
    R <- matrix(0L, nrow = length(lev1), ncol = length(lev2),
                dimnames = list(lev1, lev2))
    R[cbind(match(data[[1L]], lev1), match(data[[2L]], lev2))] <- data[[3L]]
    # convert to dense rankings and remove rankings with less than 2 items
    as.rankings.matrix(R, aggregate = aggregate, verbose = verbose)
}

#' @rdname rankings
#' @export
as.rankings <- function(x, ...,
                        verbose = TRUE){
    UseMethod("as.rankings")
}

#' @rdname rankings
#' @export
as.rankings.default <- function(x,
                                input = c("rankings", "orderings"),
                                freq = NULL,
                                index = NULL,
                                aggregate = FALSE,
                                items = NULL,
                                labels = NULL,
                                ...,
                                verbose = TRUE){
    x <- as.matrix(x)
    as.rankings.matrix(x, input = input, freq = freq,  index = index,
                       aggregate = aggregate, items = items,
                       labels = labels, ..., verbose = verbose)
}

#' @rdname rankings
#' @export
as.rankings.grouped_rankings <- function(x, ...,
                                         verbose = TRUE){
    attr(x, "rankings")
}

#' @rdname rankings
#' @export
as.rankings.matrix <- function(x,
                               input = c("rankings", "orderings"),
                               freq = NULL,
                               index = NULL,
                               aggregate = FALSE,
                               items = NULL,
                               labels = NULL,
                               ...,
                               verbose = TRUE){
    if (!is.null(labels)) {
        warning("argument `labels`` is deprecated; please use `items` instead.",
                call. = FALSE)
        items <- labels
    }
    input <- match.arg(input, c("rankings", "orderings"))
    freq_id <- getID(x, freq)
    if (!is.null(freq_id)) {
        freq <- unname(unlist(x[, freq_id]))
        x <- x[, -freq_id]
    }
    index_id <- getID(x, index)
    if (!is.null(index_id)) {
        index <- unname(unlist(x[, index_id]))
        x <- x[, -index_id]
    }
    if (mode(x) != "numeric" && input == "rankings"){
        stop("values should be numeric ranks for `input = rankings`")
    }
    if (input == "orderings"){
        # define items, N.B. matrix cells may be vectors; may have NAs
        code <- sort(setdiff(unlist(c(x)), 0L))
        if (is.null(items)){
            items  <- code
        } else if (mode(x[[1]]) == "numeric"){
            unnamed <- setdiff(code, seq_along(items))
            n <- length(unnamed)
            if (n) {
               stop("Coded items with no name:\n",
                    paste(unnamed[seq(min(n, 10L))], ", ..."[n > 10L],
                          collapse = ", "))
            } else items <- items[code]
        } else {
            unnamed <- setdiff(code, items)
            n <- length(unnamed)
            if (n) {
                warning("Some items not in `items`:\n",
                        paste(unnamed[seq(min(n, 10L))], ", ..."[n > 10L],
                              collapse = ", "),
                        "\nUsing observed unique items.")
            } else code <- items
        }
        # convert ordered items to dense ranking
        if (mode(x) == "list"){
            # i.e. there are ties
            x <- t(apply(x, 1L, function(ordering){
                g <- rep(seq_along(ordering), lengths(ordering))
                r <- match(code, unlist(ordering), nomatch = 0L)
                r[r != 0L] <- g[r[r != 0L]]
                r
            }))
        } else {
            x <- t(apply(x, 1L, function(ordering) {
                match(code, ordering, nomatch = 0L)
            }))
        }
        if (!is.null(labels)) colnames(x) <- labels
    } else {
        items <- seq_len(ncol(x))
        # check rankings are dense rankings, recode if necessary
        x[is.na(x)] <- 0
        x <- checkDense(x, verbose = verbose)
    }
    # add item names if necessary
    if (is.null(colnames(x))) colnames(x) <- items
    mode(x) <- "integer"
    out <- structure(x, class = "rankings")
    # pre-aggregated
    if (!is.null(freq)){
        out <- structure(data.frame(ranking = out, freq = freq),
                         class = c("aggregated_rankings", "data.frame"))
    }
    # aggregating (further)
    if (aggregate) {
        out <- aggregate(out, freq = freq)
    }
    # grouping
    if (!is.null(index)){
        out <- group(out, index = index)
    }
    out
}

getID <- function(x, var){
    if (!is.null(var) && (length(var) == 1 | is.logical(var))) {
        id <- seq(ncol(x))[var]
        if (length(id) != 1)
            stop("`", substitute(var),
                 "` should identify exactly one column of `x`, found\n",
                 id)
        id
    }
}

checkDense <- function(x, verbose = TRUE){
    # check rankings are dense rankings
    nRank <- apply(x, 1L, function(x) length(unique(x[x > 0L])))
    maxRank <- apply(x, 1L, max)
    bad <- maxRank != nRank & nRank > 1
    # recode any ranking not in dense form
    if (any(bad)){
        if (verbose) message("Recoded rankings that are not in dense form")
        x[bad, ] <- t(apply(x[bad, , drop = FALSE], 1L, function(x) {
            id <- x > 0L
            x[id] <- match(x[id], sort(unique(x[id])))
            x
        }))
    }
    # set rankings with only 1 item to NA (all ranks 0)
    omit_id <- which(rowSums(x > 0L) == 1L)
    if (length(omit_id)){
        if (verbose)
            message("Rankings with only 1 item set to `NA`")
        x[omit_id, ] <- 0L
    }
    x
}

#' @method as.data.frame rankings
#' @export
as.data.frame.rankings <- function(x, row.names = NULL, optional = FALSE, ...,
                                   nm = paste(deparse(substitute(x),
                                                      width.cutoff = 20L),
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

#' @method as.data.frame rankings
#' @export
as.data.frame.rankings <- function(x, row.names = NULL, optional = FALSE, ...,
                                   nm = paste(deparse(substitute(x),
                                                      width.cutoff = 20L),
                                              collapse = " ")){
    value <- list(x)
    if (!optional) names(value) <- nm
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

#' @method is.na rankings
#' @export
is.na.rankings <- function(x) {
    rowSums(x) == 0
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
        if (missing(i)) {
            value <- unclass(x)
        } else {
            # always a vector if picking out elements of rankings matrix
            if (is.matrix(i)) return(.subset(x, i))
            # else subset of rankings
            value <- .subset(x, i, TRUE, drop = FALSE)
        }
    } else {
        # subset items (never drop)
        if (missing(i)) i <- TRUE
        value <- .subset(x, i, j, drop = FALSE)
        # recode and drop items with <=2 items if necessary
        if (as.rankings && ncol(value) != ncol(x)) {
            value <- checkDense(value, ...)
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
    value <- format(x, ...)
    if (!is.null(freq(x))){
        print.data.frame(data.frame(ranking = value, freq = freq(x)))
    } else print.default(value)
}

#' @method format rankings
#' @rdname rankings
#' @export
format.rankings <- function(x, width = 40L, ...){
    f <- function(i, items) {
        keep <- !is.na(i) & i != 0L
        obj <- items[keep]
        i <- i[keep]
        ord <- order(i)
        if (length(obj) > 1L){
            op <- ifelse(diff(i[ord]) == 0L, " = ", " > ")
            paste(obj[ord], c(op, ""), sep = "", collapse = "")
        } else NA
    }
    value <- apply(x, 1L, f, colnames(x))
    nc <- nchar(value)
    trunc <- !is.na(nc) & nc > width
    value[trunc] <- paste(strtrim(value[trunc], width - 4), "...")
    value
}
#' @method rbind rankings
#' @export
rbind.rankings <- function(..., labels = NULL){
    # check contain the same items
    R <- list(...)
    nm <- lapply(R, colnames)
    ref <- nm[[1L]]
    ok <- vapply(nm, identical, TRUE, ref)
    if (any(!ok)){
        if (!is.null(labels)) {
            warning("argument `labels`` is deprecated; ",
                    "labels are automatically combined.",
                    call. = FALSE)
        }
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
    # rbind ranking values
    R <- do.call("rbind", lapply(R, unclass))
    structure(R, class = "rankings")
}

#' @method as.matrix rankings
#' @export
as.matrix.rankings <- function(x, ...){
    unclass(x)
}
