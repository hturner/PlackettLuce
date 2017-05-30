as.rankings <- function(x, ...){
    UseMethod("as.rankings")
}


as.edgelist <- function(x, ...){
    UseMethod("as.edgelist")
}

#' @importFrom matrixStats colMaxs
as.edgelist.rankings <- function(x, ...){
    maxRank <- max(x)
    colnames(x)c
    res <- list()
    for (i in seq_len(maxRank)){
        res[[i]] <- list()
        for(j in seq_len(nrow(x))){
            res[[i]][[j]] <- which(x[j, ] == i)
        }
    }
    res <- unlist(res, recursive = FALSE)
    rep <- lengths(res)
    nr <- nrow(x)
    np <- nr * maxRank
    cbind(unlist(res[rep(seq(np - nr), rep[-seq_len(nr)])]),
          unlist(res[rep((nr + 1):np, rep[seq_len(np - nr)])]))
}
