#' Check Connectivity of Rankings
#'
#' Check the connectivity of the network underlying a set of rankings.
#'
#' Ranked items are connected in a directed graph according to the implied
#' wins and loses between pairs of items. The wins and losses can be
#' summarised as an adjacency matrix using \code{\link{adjacency}}. From this
#' adjacency matrix, the graph is inferred and it is checked for
#' connectivity. A message is given if the network is not strongly connected,
#' i.e. with at least one win and one loss between all partitions of the network
#' into two groups. Features of clusters in the network are returned - if
#' the network is strongly connected, all items belong to the same cluster.
#' @param x an adjacency matrix as returned by \code{\link{adjacency}}, a
#' \code{"\link{rankings}"} object, or an object that can be coerced
#' by \code{as.rankings}.
#' @param verbose logical, if \code{TRUE}, a message is given if the network
#' is not strongly connected.
#' @return A list with elements
#' \item{membership}{a labelled vector of indices specifying membership of
#' clusters in the network of items}
#' \item{csize}{the sizes of clusters in the network of items}
#' \item{no}{the number of clusters in the network of items}
#' @examples
#' ## weakly connected network:
#' ## one win between two clusters
#' X <- matrix(c(1, 2, 0, 0,
#'               2, 1, 3, 0,
#'               0, 0, 1, 2,
#'               0, 0, 2, 1), ncol = 4, byrow = TRUE)
#' X <- as.rankings(X)
#' res <- connectivity(X)
#' res$membership
#' ## keep items in cluster 1
#' na.omit(X[,res$membership == 1])
#'
#' ## two weakly connected items:
#' ## item 1 always loses; item 4 only wins against item 1
#' X <- matrix(c(4, 1, 2, 3,
#'               0, 2, 1, 3), nr = 2, byrow = TRUE)
#' X <- as.rankings(X)
#' res <- connectivity(X)
#' res$membership
#'
#' ## item 1 always wins; item 4 always loses
#' X <- matrix(c(1, 2, 3, 4,
#'               1, 3, 2, 4), nr = 2, byrow = TRUE)
#' res <- connectivity(as.rankings(X))
#' res$membership
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
#' res <- connectivity(as.rankings(X))
#' res$membership
#'
#' @export
#' @importFrom igraph graph_from_adjacency_matrix simplify components
connectivity <- function(x, verbose = TRUE){
    # check network is strongly connected
    # (win and loss connection between all subgroups)
    if (!inherits(x, "adjacency")) x <- adjacency(x)
    net <- graph_from_adjacency_matrix(x > 0L)
    clus <- components(net, "strong")
    if (verbose && clus$no > 1L){
        message("Network of items is not strongly connected")
    }
    id <- match(colnames(x), names(clus$membership))
    list(membership = clus$membership[id],
         csize = clus$csize,
         no = clus$no)
}
