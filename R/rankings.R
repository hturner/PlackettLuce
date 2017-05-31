#' Rankings Objects
#'
#' Create a \code{"rankings"} object from data or convert a matrix of rankings
#' to a \code{"rankings"} object. These functions validate that the data are
#' (possibly partial) dense rankings, recoding as necessary. The connectivity of
#' inferred pairwise comparisons is also checked and information on clusters in
#' the network is returned.
#'
#' Each ranking \eqn{r} should rank items from 1 (first place) to \eqn{n_r}
#' (last place). Items not ranked should have a rank of 0. Tied items are given
#' the same rank with no rank skipped. For example {1, 0, 2, 1}, ranks the first
#' and fourth items in first place and the third item in second place; the
#' second item is unranked.
#'
#' \code{as.rankings} with throw an error if \code{x} contains anything other
#' than integers \code{0:M} where M is the maximum rank.
#'
#' If the ranks are not in dense form, they will be recoded, e.g. {2, 2, 0, 4}
#' will become {1, 1, 0, 2}.
#'
#' The underlying network of paired comparisons is inferred and checked for
#' connectivity. A warning is given if the network is not strongly connected,
#' i.e. with at least one win and one loss between all partitions of the network
#' into two groups. Attributes are returned on the clusters in the network - if
#' the network is strongly connected, all items belong to the same cluster.
#' @param data a data frame with columns specified by \code{id}, \code{item} and
#' \code{rank}.
#' @param id an index of \code{data} specifying the column containing ranking
#' IDs.
#' @param item an index of \code{data} specifying the column containing item
#' IDs,
#' @param rank an index of \code{data} specifying the column containing item
#' ranks.
#' @param x a matrix with one column per item and one row per ranking.
#' @param verbose logical; if \code{TRUE} print messages when changes are made
#' to rankings data.
#' @param ... further arguments passed to/from methods.
#'
#' @return a \code{"rankings"} object, which is a matrix of rankings with
#' attributes
#' \code{membership}{a labelled vector of indices specifying membership of
#' clusters in the network of items}
#' \code{csize}{the sizes of clusters in the network of items}
#' \code{no}{the number of clusters in the network of items}
#'
#' @examples
#' # create rankings from data in long form
#'
#' ## many issues with the raw rankings!
#' x <- data.frame(ranking = c(rep(1:4, each = 4), 5, 5, 5),
#'                 letter = c(LETTERS[c(1:3, 3, 1:4, 2:5, 1:2, 1)], NA,
#'                            LETTERS[3:5]),
#'                 rank = c(4:1, rep(NA, 4), 3:4, NA, NA, 1, 3, 4, 2, 2, 2, 3))
#' rankings(x, id = "ranking", item = "letter", rank = "rank")
#'
#' # convert existing matrix of rankings
#'
#' ## weakly connected network:
#' ## one win between two clusters
#' X <- matrix(c(1, 2, 0, 0,
#'               2, 1, 3, 0,
#'               0, 0, 1, 2,
#'               0, 0, 2, 1), ncol = 4, byrow = TRUE)
#' R <- as.rankings(X)
#' attr(R, "membership")
#'
#' ## two weakly connected items:
#' ## item 1 always loses; item 4 only wins against item 1
#' X <- matrix(c(4, 1, 2, 3,
#'               0, 2, 1, 3), nr = 2, byrow = TRUE)
#' R <- as.rankings(X)
#' attr(R, "membership")
#'
#' ## item 1 always wins; item 4 always loses
#' X <- matrix(c(1, 2, 3, 4,
#'               1, 3, 2, 4), nr = 2, byrow = TRUE)
#' R <- as.rankings(X)
#' attr(R, "membership")
#'
#' ## all in separate clusters: always 1 > 2 > 3 > 4
#' ## also miscoded rankings and redundant ranking
#' X <- matrix(c(1, 2, 3, 4,
#'               1, 0, 2, 3,
#'               1, 1, 2, 0,
#'               1, 0, 3, 4,
#'               2, 2, 0, 4,
#'               0, 0, 3, 0,
#'               2, 4, 0, 0), ncol = 4, byrow = TRUE)
#'               R <- as.rankings(X)
#' R
#' attr(R, "recoded")
#' attr(R, "membership")
#' @importFrom stats na.omit
#' @export
rankings <- function(data, id, item, rank, verbose = TRUE, ...){
    x <- x[c(id, item, rank)]
    if (ncol(x) != 3) stop("id, item and rank must specify columns in x")
    # remove records with unknown id, item or rank
    x <- na.omit(x)
    if (verbose && inherits(x, "omit")){
        message("Removed records with unknown id, item or rank")
    }
    # id duplicated items
    id <- paste(x[[1]], x[[2]], sep = ":")
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
            ranks <- x[[3]][id == dups[i]]
            # keep first if equal/consecutive ranks, else drop all (inconsistent)
            keep[i] <- diff(range(ranks)) < 2
        }
        include <- !id %in% dups
        first <- !include & !dup
        first[first] <- keep
        x <- x[include | first,]
    }
    # remove rankings with less than 2 items
    n <- rowsum(as.numeric(x[[3]] > 0), x[[1]], na.rm = TRUE)
    id <- rownames(n)[n < 2]
    if (length(id)){
        if (verbose) message("Removed rankings with less than 2 items.")
        x <- x[!as.character(x[[1]]) %in% id, , drop = FALSE]
    }
    # create rankings matrix (do not assume any form of ranking)
    lev1 <- sort(unique(x[[1]]))
    lev2 <- sort(unique(x[[2]]))
    R <- matrix(0, nrow = length(lev1), ncol = length(lev2),
                dimnames = list(lev1, lev2))
    R[cbind(match(x[[1]], lev1), match(x[[2]], lev2))] <- x[[3]]
    # convert to dense rankings and check connectivity
    as.rankings.matrix(R, verbose = verbose)
}

#' @rdname rankings
#' @export
as.rankings <- function(x, verbose = TRUE, ...){
    UseMethod("as.rankings")
}

#' @rdname rankings
#' @export
as.rankings.Matrix <- function(x, verbose = TRUE, ...){
    as.rankings.matrix(as.matrix(x), verbose = verbose)
}

#' @rdname rankings
#' @export
#' @importFrom igraph graph_from_edgelist simplify distances components
as.rankings.matrix <- function(x, verbose = TRUE, ...){
    # check values are valid integers
    if (!all(unique(c(x)) %in% 0:ncol(x)))
        stop("Rankings should consist of integers between 0 and ", ncol(x))
    # check rankings are dense rankings
    x <- checkDense(x, verbose = verbose)
    # check network is strongly connected
    # (win and loss connection between all subgroups)
    net <- graph_from_edgelist(as.edgelist.rankings(x))
    net <- simplify(net, remove.multiple = TRUE)
    clus <- components(net, "strong")
    if (clus$no > 1){
        warning("Network of items is not strongly connected")
    }
    structure(x,
              membership = clus$membership,
              csize = clus$csize,
              no = clus$no,
              class = "rankings")
}

checkDense <- function(x, verbose = TRUE){
    # check rankings are dense rankings
    nRank <- apply(x, 1, function(x) length(unique(x[x > 0])))
    maxRank <- apply(x, 1, max)
    bad <- maxRank != nRank
    # recode any ranking not in dense form
    if (any(bad)){
        message("Recoded rankings that are not in dense form")
        x[bad, ] <- t(apply(x[bad, , drop = FALSE], 1, function(x) {
            id <- x > 0
            x[id] <- match(x[id], sort(unique(x[id])))
            x
        }))
    }
    structure(x, recoded = bad)
}

#' @method print rankings
#' @export
print.rankings <- function(x, ...){
    print(x[seq(nrow(x)), seq(ncol(x))])
}
