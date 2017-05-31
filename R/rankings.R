#' Convert Rankings Data to a Rankings Object
#'
#' Convert a simple data structure containing rankings to a \code{"rankings"}
#' object. This validates that the data are (possibly partial) dense rankings,
#' recoding if possible. The connectivity of inferred pairwise comparisons is
#' also checked and information on clusters in the network is returned.
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
#' @param x a matrix with one column per item and one row per ranking.
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
#' # weakly connected network:
#' # one win between two clusters
#' X <- matrix(c(1, 2, 0, 0,
#'               2, 1, 3, 0,
#'               0, 0, 1, 2,
#'               0, 0, 2, 1), ncol = 4, byrow = TRUE)
#' R <- as.rankings(X)
#' attr(R, "membership")
#'
#' # two weakly connected items:
#' # item 1 always loses; item 4 only wins against item 1
#' X <- matrix(c(4, 1, 2, 3,
#'               0, 2, 1, 3), nr = 2, byrow = TRUE)
#' R <- as.rankings(X)
#' attr(R, "membership")
#'
#' # item 1 always wins; item 4 always loses
#' X <- matrix(c(1, 2, 3, 4,
#'               1, 3, 2, 4), nr = 2, byrow = TRUE)
#' R <- as.rankings(X)
#' attr(R, "membership")
#'
#' # all in separate clusters: always 1 > 2 > 3 > 4
#' # also miscoded rankings and redundant ranking
#' X <- matrix(c(1, 2, 3, 4,
#'               1, 0, 2, 3,
#'               1, 1, 2, 0,
#'               1, 0, 3, 4,
#'               2, 2, 0, 4,
#'               0, 0, 3, 0,
#'               2, 4, 0, 0), ncol = 4, byrow = TRUE)
#'               R <- as.rankings(X)
#' R
#' attr(R, "singleton")
#' attr(R, "recoded")
#' attr(R, "membership")
#' @aliases rankings
#' @export
as.rankings <- function(x, ...){
    UseMethod("as.rankings")
}

#' @rdname as.rankings
#' @export
as.rankings.Matrix <- function(x, ...){
    as.rankings.matrix(as.matrix(x))
}

#' @rdname as.rankings
#' @export
#' @importFrom igraph graph_from_edgelist simplify distances components
as.rankings.matrix <- function(x, ...){
    # check values are valid integers
    if (!all(unique(c(x)) %in% 0:ncol(x)))
        stop("Rankings should consist of integers between 0 and ", ncol(x))
    # check rankings are dense rankings
    x <- checkDense(x)
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

checkDense <- function(x){
    # check rankings are dense rankings
    nRank <- apply(x, 1, function(x) length(unique(x[x > 0])))
    maxRank <- apply(x, 1, max)
    bad <- maxRank != nRank
    # recode any ranking not in dense form
    if (any(bad)){
        warning("Recoding rankings that are not in dense form")
        x[bad, ] <- t(apply(x[bad, , drop = FALSE], 1, function(x) {
            id <- x > 0
            x[id] <- match(x[id], sort(unique(x[id])))
            x
        }))
    }
    # note any "ranking" of one object, but leave for now
    structure(x, singleton = nRank == 1, recoded = bad)
}

#' @method print rankings
#' @export
print.rankings <- function(x, ...){
    print(x[seq(nrow(x)), seq(ncol(x))])
}
