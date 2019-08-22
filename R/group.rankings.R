#' Group Rankings
#'
#' Create an object of class \code{"grouped_rankings"} which associates a
#' group index with an object of class \code{"rankings"}. This allows the
#' rankings to be linked to covariates with group-specific values as the basis
#' for model-based recursive partitioning, see \code{\link{pltree}}.
#'
#' @param rankings a \code{\link{rankings}} object or an object that can be
#' coerced by \code{as.rankings}.
#' @param index a numeric vector of length equal to the number of rankings
#' specifying the subject for each ranking.
#' @param x an object that can be coerced to a \code{"grouped_rankings"} object
#' for \code{as.grouped_rankings}, or a \code{"grouped_rankings"} object for
#' \code{[} and \code{format}.
#' @param i indices specifying groups to extract, may be any data type accepted
#' by \code{\link{[}}.
#' @param j indices specifying items to extract, as for \code{\link{[}}.
#' @param drop if \code{TRUE} return single row/column matrices as a vector.
#' @param as.grouped_rankings if \code{TRUE} return a grouped_rankings object,
#' otherwise return a matrix/vector.
#' @param max the maximum number of rankings to format per subject.
#' @param width the maximum width in number of characters to format each
#' ranking.
#' @param ... additional arguments passed on to \code{\link{as.rankings}}
#' by \code{grouped_rankings} or \code{as.grouped_rankings}; unused by
#' \code{format}.
#' @return an object of class \code{"grouped_rankings"}, which is a vector of
#' of group IDs with the following attributes:
#' \item{rankings}{ The \code{"rankings"} object.}
#' \item{index}{ An index match each ranking to each group ID.}
#' \item{R}{ A matrix with items ordered from last to first place, for each
#' ranking. }
#' \item{S}{ The rankings matrix with the ranks replaced by the size of the
#' chosen set for free choices and zero for forced choices. }
#' \item{id}{ A list with elements of the adjacency matrix that are incremented
#' by each ranking. }
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
#' # group rankings (first three in group 1, next two in group 2)
#' G <- group(R, c(1, 1, 1, 2, 2))
#' length(G)
#' ## by default up to 2 rankings are shown per group, "..." indicates if
#' ## there are further rankings
#' G
#' print(G, max = 1)
#' ## select rankings from group 1
#' G[1,]
#' ## exclude item 3 from ranking
#' G[, -3]
#' ## rankings from group 2, excluding item 3
#' ## - note group 2 becomes the first group
#' G[2, -3]
#' ## index underlying rankings without creating new grouped_rankings object
#' G[2, -3, as.grouped_rankings = FALSE]
#' @aliases grouped_rankings
#' @export
group <- function(x, index, ...){
    UseMethod("group")
}

#' @rdname group
#' @export
group.rankings <- function(x, index, ...){
    if (!(is.vector(index) & length(index) == nrow(x)))
        stop("index must be a vector with length equal to rankings")
    nm <- rownames(x)
    if (!inherits(x, "rankings"))
        x <- as.rankings(x, ...)
    index <- as.numeric(index)
    do.call("structure",
            c(list(seq_len(max(index)), rankings = x, index = index),
              ranking_stats(x),
              list(class = "grouped_rankings")))
}


# ranking stats - summaries used in model fitting, compute once for all
ranking_stats <- function(rankings){
    rankings <- unclass(rankings)
    nr <- nrow(rankings)
    nc <- ncol(rankings)
    R <- S <- matrix(0L, nr, nc)
    id <- list()
    for (i in seq_len(nr)){
        x <- rankings[i, ]
        ind <- which(as.logical(x))
        if (length(ind) < 2L) next # no contribution to modelling
        ord <- order(x[ind], decreasing = TRUE)
        j <- seq_along(ind)
        # items ranked from last to 1st place
        R[i, j] <- as.integer(ind[ord])
        # 1 in column s if ranking includes choice from set of size |s|
        x <- x[R[i, j]]
        # size of chosen set at each rank (ignore "choice" of one from one)
        size <- tabulate(x)[x]
        if (size[1L] == 1L) size[1L] <- 0L
        S[i, j] <- size
        # contribution to adjacency matrix
        add <- list()
        for (s in seq_len(x[1L] - 1L)){
            one <- which(rankings[i, ] == s)
            # > gives rest; == s + 1 gives next best
            two <- which(rankings[i, ] > s)
            add[[s]] <- kronecker(one, (two - 1L)*nc, "+")
        }
        id[[i]] <- unlist(add)
    }
    list(R = R, S = S, id = id)
}

#' @rdname PlackettLuce-deprecated
#' @section grouped_rankings:
#' `grouped_rankings()` has been replaced by [group()].
#' @export
grouped_rankings <- function(rankings, index, ...){
    .Deprecated("group", package = "PlackettLuce")
    group(rankings, index, ...)
}

#' @rdname group
#' @method [ grouped_rankings
#' @export
"[.grouped_rankings" <- function(x, i, j, ..., drop = TRUE,
                                 as.grouped_rankings = TRUE) {
    if (!missing(i)) {
        if (missing(j)) j <- TRUE
        # always a vector if picking out elements of rankings matrix
        if (is.matrix(i)) {
            r <- split(seq_along(attr(x, "index")), attr(x, "index"))
            i1 <- unlist(r[i[,1L]])
            i2 <- rep(i[,2L], lengths(r))
            return(.subset(attr(x, "rankings"), cbind(i1, i2)))
        }
        # convert index of groups to index of rankings
        g <- .subset(x, i)
        # create index for rankings matrix
        i <- which(attr(x, "index") %in% g)
        groups <- split(i, attr(x, "index")[i])[as.character(g)]
        i <- unlist(groups)
        # update value and index to remove omitted groups
        value <- seq_along(groups)
        index <- rep(value, lengths(groups))
    } else {
        if (missing(j)) return(x)
        value <- x
        i <- TRUE
        index <- attr(x, "index")
    }
    # now subset rankings matrix
    rankings <- .subset(attr(x, "rankings"), i, j, drop = FALSE)
    if (!as.grouped_rankings) {
        if (drop) return(drop(rankings))
        return(rankings)
    }
    if (ncol(rankings) == ncol(attr(x, "rankings"))) {
        # subset attributes to match selected rankings
        structure(value,
                  rankings = structure(rankings, class = "rankings"),
                  index = index,
                  R = attr(x, "R")[i, , drop = FALSE],
                  S = attr(x, "S")[i, , drop = FALSE],
                  id = attr(x, "id")[i],
                  class = "grouped_rankings")
    } else {
        # convert rankings matrix to grouped_rankings
        # (will recode as necessary, omit redundant rankings, create R, S, id)
        group(rankings, index)
    }
}

#' @rdname group
#' @export
as.grouped_rankings <- function(x, ...){
    UseMethod("as.grouped_rankings")
}

#' @rdname group
#' @method as.grouped_rankings paircomp
#' @export
as.grouped_rankings.paircomp <- function(x, ...){
    if (attr(x, "mscale")[1L] < -1L) {
        warning("strength of preference ignored")
        x <- sign(x)
    }
    id <- which(!is.na(as.matrix(x)), arr.ind = TRUE)
    ncomp <- nrow(id)
    nobj <- length(attr(x, "labels"))
    pairs <- which(upper.tri(diag(nobj)), arr.ind = TRUE)
    rankings <- matrix(0L, nrow = ncomp, ncol = nobj,
                       dimnames = list(NULL, attr(x, "labels")))
    x <- as.matrix(x)[id]
    rankings[cbind(seq_len(ncomp), pairs[,1L][id[,2L]])] <-
        ifelse(x == -1L, 2L, 1L)
    rankings[cbind(seq_len(ncomp), pairs[,2L][id[,2L]])] <-
        ifelse(x == 1L, 2L, 1L)
    rankings <- structure(rankings, class = "rankings")
    do.call("structure",
            c(list(seq_len(max(id[,1L])), rankings = rankings, index = id[,1L]),
              ranking_stats(rankings),
              list(class = "grouped_rankings")))
}

#' @method as.data.frame grouped_rankings
#' @export
as.data.frame.grouped_rankings <-
    function(x, row.names = NULL, optional = FALSE, ...,
             nm = paste(deparse(substitute(x), width.cutoff = 20L),
                        collapse = " ")){
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

#' @method print grouped_rankings
#' @export
print.grouped_rankings <- function(x, max = 2L, width = 20L, ...){
    print.default(format(x, max = max, width = width, ...))
}

#' @rdname group
#' @method format grouped_rankings
#' @export
format.grouped_rankings <- function(x, max = 2L, width = 20L, ...){
    tab <- tabulate(attr(x, "index"))
    rep <- numeric(length(attr(x, "index")))
    rep[order(attr(x, "index"))] <- sequence(tab)
    R <- attr(x, "rankings")[rep <= max, ]
    char <- format.rankings(R, width = width)
    value <- vapply(split(char, attr(x, "index")[rep <= max]),
                    function(x) {
                        if (all(is.na(x))) return(NA_character_)
                        paste(x, collapse = ", ")
                        }, "a")
    # add ... if more than max rankings
    trunc <- tab > max & !is.na(value)
    value[trunc] <- paste0(value[trunc], ", ...")
    value
}

#' @rdname group
#' @method na.omit grouped_rankings
#' @export
na.omit.grouped_rankings <- function(object, ...) {
    omit <- seq_along(attr(object, "rankings"))[is.na(attr(object, "rankings"))]
    if (length(omit) == 0L)
        return(object)
    nm <- names(object)
    index <- attr(object, "index")[-omit]
    index <- match(index, unique(index))
    names(omit) <- nm[omit]
    attr(omit, "class") <- "omit"
    structure(unique(index),
              rankings = attr(object, "rankings")[-omit, , drop = FALSE],
              index = index,
              R = attr(object, "R")[-omit, , drop = FALSE],
              S = attr(object, "S")[-omit, , drop = FALSE],
              id = attr(object, "id")[-omit],
              na.action = omit,
              class = "grouped_rankings")
}

#' @rdname group
#' @method na.exclude grouped_rankings
#' @export
na.exclude.grouped_rankings <- function(object, ...) {
    out  <- na.omit(object)
    class(attr(out, "na.action")) <- "na.exclude"
    out
}

#' @method is.na grouped_rankings
#' @export
is.na.grouped_rankings <- function(x) {
    out <- tapply(attr(x, "rankings"), attr(x, "index"), sum) == 0
    names(out) <- names(G)
    out
}
