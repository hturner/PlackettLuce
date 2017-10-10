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
#' \code{[} and \code{format}.
#' @param i indices specifying groups to extract, may be any data type accepted
#' by \code{\link{[}}.
#' @param j indices specifying items to extract, as for \code{\link{[}}.
#' @param drop if \code{TRUE} return single row/column matrices as a vector.
#' @param as.grouped_rankings if \code{TRUE} return a rankings object, otherwise
#' return a matrix/vector.
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
#' # grouped rankings (1st 3 from subject 1, next 2 from subject 2)
#' G <- grouped_rankings(R, c(1, 1, 1, 2, 2))
#' length(G)
#' ## by default up to 2 rankings are shown per subject, "..." indicates if
#' ## there are further rankings
#' G
#' print(G, max = 1)
#' ## select rankings from subject 1
#' G[1,]
#' ## exclude item 3 from ranking
#' G[, -3]
#' ## rankings from subject 2, excluding item 3
#' ## - note subject 2 becomes the first subject
#' G[2, -3]
#' ## index underlying rankings without creating new grouped_rankings object
#' G[2, -3, as.grouped_rankings = FALSE]
#' @export
grouped_rankings <- function(rankings, index, ...){
    if (!(is.vector(index) & length(index) == nrow(rankings)))
        stop("index must be a vector with length equal to rankings")
    nm <- rownames(rankings)
    if (!inherits(rankings, "rankings"))
        rankings <- as.rankings(rankings, ...)
    index <- as.numeric(index)
    if (!is.null(attr(rankings, "omit"))){
        if (!is.null(nm)) {
            omit <- match(attr(rankings, "omit"), nm)
        } else omit <- attr(rankings, "omit")
        index <- index[-omit]
    }
    do.call("structure",
            c(list(seq_len(max(index)), rankings = rankings, index = index),
              ranking_stats(rankings),
              list(class = "grouped_rankings")))
}

# ranking stats - summaries used in model fitting, compute once for all
ranking_stats <- function(rankings){
    rankings <- unclass(rankings)
    nr <- nrow(rankings)
    nc <- ncol(rankings)
    R <- S <- matrix(0, nr, nc)
    id <- list()
    for (i in seq_len(nr)){
        x <- rankings[i, ]
        ind <- which(as.logical(x))
        ord <- order(x[ind], decreasing = TRUE)
        j <- seq_along(ind)
        # items ranked from last to 1st place
        R[i, j] <- ind[ord]
        # 1 in column s if ranking includes choice from set of size |s|
        x <- x[R[i, j]]
        # size of chosen set at each rank (ignore "choice" of one from one)
        size <- tabulate(x)[x]
        if (size[1] == 1) size[1] <- 0
        S[i, j] <- size
        # contribution to adjacency matrix
        if (x[1] < 2) next # x[1] gives max rank
        add <- list()
        for (s in seq_len(x[1] - 1)){
            one <- which(rankings[i, ] == s)
            # > gives rest; == s + 1 gives next best
            two <- which(rankings[i, ] > s)
            add[[s]] <- kronecker(one, (two - 1)*nc, "+")
        }
        id[[i]] <- unlist(add)
    }
    list(R = R, S = S, id = id)
}

#' @rdname grouped_rankings
#' @method [ grouped_rankings
#' @export
"[.grouped_rankings" <- function(x, i, j, ..., drop = TRUE,
                                 as.grouped_rankings = TRUE) {
    if (!missing(i)) {
        if (missing(j)) j <- TRUE
        # always a vector if picking out elements of rankings matrix
        if (is.matrix(i)) {
            r <- split(seq_along(attr(x, "index")), attr(x, "index"))
            i1 <- unlist(r[i[,1]])
            i2 <- rep(i[,2], lengths(r))
            return(.subset(attr(x, "rankings"), cbind(i1, i2)))
        }
        # convert index of groups to index of rankings
        value <- .subset(x, i)
        i <- attr(x, "index") %in% value # or match(value, x)
        # update index to remove omitted groups
        index <- match(attr(x, "index")[i], value)
        value <- seq_len(length(value))
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
        grouped_rankings(rankings, index)
    }
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
    rankings <- structure(rankings, class = "rankings")
    do.call("structure",
            c(list(seq_len(max(id[,1])), rankings = rankings, index = id[,1]),
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
    value <- vapply(split(char, attr(x, "index")[rep <= max]), paste,
                    collapse = ", ", "a")
    # add ... if more than max rankings
    trunc <- tab > max
    value[trunc] <- paste0(value[trunc], ", ...")
    value
}
