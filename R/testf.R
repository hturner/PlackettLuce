#' @import Matrix lbfgs
#' @importFrom igraph as_adj graph_from_edgelist
#' @importFrom rARPACK eigs
#' @export
teste <- function(rankings, ref = NULL,
                  network = c("adaptive", "pseudodata", "connected",
                              "cluster"),
                  npseudo = 1,
                  method = c("iterative scaling", "BFGS", "L-BFGS"),
                  epsilon = 1e-7, steffensen = 1e-4, maxit = 100,
                  trace = FALSE, verbose = TRUE){
    call <- match.call()
    network <- match.arg(network, c("adaptive", "pseudodata", "connected",
                                    "cluster"))
    # check rankings
    if (!inherits(rankings, "rankings")){
        rankings <- suppressWarnings(as.rankings(rankings, verbose = verbose))
    }
    items <- colnames(rankings)
    # if pseudodata or (adaptive and disconnected) add pseudodata
    if (network == "pseudodata" ||
        (network == "adaptive" & attr(rankings, "no") > 1)){
        nobj <- ncol(rankings)
        rankings <- cbind("NULL" = 0, rankings)
        pseudo <- matrix(0, nrow = 2*npseudo*nobj,
                         ncol = ncol(rankings))
        pseudo[, 1] <- 1:2
        pseudo[cbind(seq_len(nrow(pseudo)),
                     rep(seq_len(nobj) + 1, each = 2))] <- 2:1
        rankings <- as.rankings(rbind(pseudo, rankings))
        pseudo <- TRUE
    } else pseudo <- FALSE
    # if disconnected, fit model to largest cluster
    if (!is.null(attr(rankings, "no")) && attr(rankings, "no") > 1){
        if (network == "connected")
            stop("Network is not strongly connected.")
        id <- which.max(attr(rankings, "csize"))
        size <- attr(rankings, "csize")[id]
        if (size == 1) {
            stop("All items are weakly connected, cannot estimate item abilities.")
        } else{
            warning("The network of items is split into weakly connected clusters\n",
                    "Analysing the largest cluster, with ", size, " items")
            # drop items not in largest cluster and recode
            rankings <- rankings[, attr(rankings, "membership") == id]
            rankings <- suppressMessages(checkDense(rankings))
            if (!is.null(ref)){
                if ((is.character(ref) && !ref %in% colnames(rankings)) ||
                    !ref %in% id){
                    warning("re-setting `ref` to first object in largest cluster")
                    ref <- id[1]
                }
            }
        }
    }
    if (is.null(ref)) ref <- 1

    # express rankings as choices (list of selected sets)
    rankings <- unclass(rankings)
    nm <- colnames(rankings)
    colnames(rankings) <- NULL
    nr <- nrow(rankings)
    seqr <- seq_len(nr)

    rankings <- lapply(seqr, function(i){
        ind <- which(rankings[i,] > 0)
        unname(split(ind, rankings[i, ind]))
    })

    # sizes of selected sets
    len <- lapply(rankings, function(x){
        len <- lengths(x)
        n <- length(len)
        # keep nontrivial nsets (exclude untied last place)
        if (len[n] == 1) len[n] <- 0
        len
    })

    # statistics required by sufficient statistics/likelihood

    # for delta d, number of sets with cardinality d
    B <- tabulate(unlist(len))
    D <- length(B)
    # for alpha i, sum over all sets st object i is in selected set divided by
    # size of selected set
    i <- unlist(lapply(len, seq_along))
    len <- unlist(len)
    id <- len > 0
    len <- rep(len, len)
    A <- unname(c(rowsum(1/len,
                         unlist(unlist(rankings, recursive = FALSE)[id]))))

    # starting values

    # total number of objects
    N <- ncol(R)

    # adjacency matrix: here just connect direct wins i.e. over next best
    n <- lengths(rankings) # number of sets within ranking
    m <- max(n)
    X <- matrix(0, N, N)
    for (i in 1:m){
        r <- n >= (i + 1)
        one <- lapply(rankings[r], "[[", i)
        two <- lapply(rankings[r], "[[", i + 1)
        for(j in seq_along(one)) {
            X[one[[j]], two[[j]]] <- X[one[[j]], two[[j]]] + 1
        }
    }

    # (scaled, un-damped) PageRank based on underlying paired comparisons
    # - as in BradleyTerryScalable for paired comparison
    alpha <- drop(abs(eigs(X/colSums(X), 1,
                           opts = list(ncv = min(nrow(X), 10)))$vectors))
    # alpha <- alpha/sum(alpha) current version doesnt divide by alpha
    delta <- rep.int(0.1, D)
    delta[1] <- 1

    # maximum size of set to select from (set of alternatives)
    len <- vapply(rankings, function(r) sum(lengths(r)), 1)
    nc <- max(len)

    # express rankings as items order from last to first place
    R <- matrix(0, nrow = nr, ncol = nc)
    for (i in seqr){
        R[i, seq_len(len[[i]])] <- rev(unlist(rankings[[i]]))
    }

    # location of tied items as n'th from last place
    if (D > 1){
        # n'th from last place by ranking
        T <- lapply(rankings, function(r){
            len <- rev(lengths(r))
            tie <- len > 1
            n <- length(r)
            if (any(tie)){
                clen <- c(0, cumsum(len[-n]))
                len <- len[tie] - 1
                sequence(len) + rep(clen[tie], len)
            } else integer(0)
        })
        # convert to ranking by n'th from last place
        id <- rep(seqr, lengths(T))
        T <- unlist(T)
        T2 <- list()
        for (i in seq_len(nc)) T2[[i]] <- id[T == i]
    } else {
        T2 <- list()
        for (i in seq_len(nc)) T2[[i]] <- integer(0)
    }

    # unique rows with >= 1 element for logical matrix
    # case where p is fixed
    uniquerow <- function(M, n, p){
        if (n == 1) return(1)
        j <- rep(seq_len(n), p)
        pattern <- numeric(n)
        pattern[j[M == 1]] <- 1
        for (k in 2:n){
            pattern <- 2*pattern
            id2 <- j[M == k]
            pattern[id2] <- pattern[id2] + 1
            if (k == n ||
                i %% .Machine$double.digits == (.Machine$double.digits - 1))
                pattern <- match(pattern, unique(pattern))
        }
        pattern
    }

    # representative sets (ind) and weights (S) for all set sizes (P)
    S <- ind <- list()
    P <- logical(nc)
    for (i in seq_len(nc)){
        p <- (nc - i + 1)
        r <- setdiff(which(R[,p] > 0), T2[[p]])
        n <- length(r)
        P[p] <- p != 1 && n
        if (!P[p]) next
        g <- uniquerow(c(R[r, 1:p, drop = FALSE]), n, p)
        S[[p]] <- tabulate(g)
        ind[[p]] <- r[!duplicated(g)]
    }

    P <- which(P)
    S <- structure(S, ind = ind)
}
