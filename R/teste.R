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
    # sparse matrix of dense rankings
    # rankings object should probably be sparse, but some methods may need update
    R <- Matrix(unclass(rankings), sparse = TRUE)
    rm(rankings)
    # nontrivial nsets (excluding untied last place)
    J <- apply(R, 1, max)
    J <- J - as.numeric(rowSums(R == J) == 1)
    # sizes of selected sets (not particularly efficient - could all be == 1!)
    S <- R
    S[which(R > J)] <- 0
    S <- t(S)
    S@x <- as.double(unlist(apply(S, 2, function(x) tabulate(x)[x])))
    # sufficient statistics
    # for alpha i, sum over all sets st object i is in selected set/size of selected set
    A <- unname(rowsum(1/S@x, S@i)[,1])
    # for delta d, number of sets with cardinality d/cardinality
    B <- tabulate(S@x)
    D <- length(B)
    B <- B/seq(D)

    # starting values - as.edgelist.rankings doesn't currently work for Matrix

    # (scaled, un-damped) PageRank based on underlying paired comparisons
    if (is.null(colnames(R))) colnames(R) <- 1:ncol(R)
    X <- as_adj(graph_from_edgelist(as.edgelist.rankings(as.matrix(R))))[colnames(R),
                                                                         colnames(R)]
    alpha <- drop(abs(eigs(X/colSums(X), 1,
                           opts = list(ncv = min(nrow(X), 10)))$vectors))
    # alpha <- alpha/sum(alpha) current version doesnt divide by alpha
    delta <- rep.int(0.1, D)
    delta[1] <- 1

    # pre-process rankings so easier to compute on

    # unique rows with >= 1 element for logical matrix
    uniquerow <- function(M, rep = TRUE){
        for (i in seq_len(ncol(M))){
            if (i == 1) {
                pattern <- M[,1]
            }
            else {
                pattern <- 2*pattern + M[,i]
                pattern <- match(pattern, unique(pattern))
            }
        }
        pattern
    }


    # total number of objects
    N <- ncol(R)

    # max number of objects in set
    nc <- max(rowSums(R > 0))

    # number of rankings
    nr <- nrow(R)

    # create S so cols are potential set sizes and value > 0 indicates ranking
    # aggregate common sets
    # - incorporate ranking weights by weighted count vs simple tabulate
    S <- as(sparseMatrix(dims = c(nr, nc), i = {}, j = {}), "dgCMatrix")
    T <- sparseMatrix(dims = c(nr, N), i = {}, j = {})
    P <- logical(nc)  # set sizes present in rankings
    minrank <- rep.int(1, nr)
    for (i in seq_len(nc)){
        p <- (nc - i + 1)
        pattern <- R >= minrank
        r <- rowSums(pattern) == p
        tied <- !r & minrank != 1
        if (any(tied)) T[tied, p] <- TRUE
        P[p] <- p != 1 && any(r)
        if (!P[p] ) next
        g <- uniquerow(pattern[r, , drop = FALSE])
        ng <- tabulate(g)
        S[r, , drop = FALSE][!duplicated(g), p] <- ng
        minrank[r] <- minrank[r] + 1
    }
    P <- which(P)


    # multiply by rankings weights here

    # replace rankings with items ordered by ranking (all that is needed now)
    R <- as(t(R), "dgTMatrix")
    R@x <- R@i[do.call(order, list(R@j, R@x, decreasing = c(FALSE, TRUE),
                                   method = "radix"))] + 1
    R@i <- sequence(colSums(R > 0)) - 1L
    R <- t(R)

    grp <- rep(1:nc, diff(S@p))
    ind <- lapply(1:nc, function(p) S@i[grp == p] + 1)
    S <- structure(lapply(1:nc, function(p) S@x[grp == p]), ind = ind)

    R <- as.matrix(R)

    # quasi-newton methods ---

    key_quantities <- function(par) {
        alpha <- par[1:N]
        delta <- par[-c(1:N)] # includes delta1
        res <- expectation2e("all", alpha, c(1, delta), R, S, N, D, P)
        # sum of (log(normalising_constants_per_set) * rep)
        c_contr <- sum(res$theta)
        list(alpha = alpha, delta = delta, c_contr = c_contr,
             expA = res$expA, expB = res$expB)
    }

    # Design loglik as brglm2::brglmFit
    # log-likelihood and score functions
    # Within optim or nlminb use obj and gr wrappers below
    #
    # assign key quantities to function environment to re-use
    loglik <- function(par) {
        assign("fit", key_quantities(par), envir = parent.env(environment()))
        b_contr <- sum(B[-1]*log(fit$delta)) + sum(A*log(fit$alpha))
        b_contr - fit$c_contr
    }

    # log-likelihood derivatives
    score <- function(par) {
        alpha <- par[1:N]
        delta <- par[-c(1:N)]
        c(A/alpha - fit$expA/alpha,
          B[-1]/delta - fit$expB)
    }

    # Alternative optimization via
    obj <- function(par) {
        al <- exp(par[1:N])
        de <- exp(par[-c(1:N)])
        -loglik(c(al, de))
    }
    gr <- function(par) {
        al <- exp(par[1:N])
        de <- exp(par[-c(1:N)])
        -score(c(al, de)) * c(al, de)
    }

    method <- match.arg(method, c("iterative scaling", "BFGS", "L-BFGS"))

    res <- list(alpha = alpha, delta = delta)
    res[c("expA", "expB")] <-
        expectation2e("all", alpha, delta,  R, S, N, D, P)[c("expA", "expB")]
    oneUpdate <- function(res){
        # update all alphas
        if (pseudo){
            res$alpha[-1] <- res$alpha[-1]*A[-1]/res$expA[-1]
        } else {
            res$alpha <- res$alpha*A/res$expA
        }
        # update all deltas
        if (D > 1) res$delta[-1] <- B[-1]/
                expectation2e("delta", res$alpha, res$delta, R, S, N, D, P)$expB
        res[c("expA", "expB")] <-
            expectation2e("all", res$alpha, res$delta, R, S, N, D, P)[c("expA", "expB")]
        res
    }
    accelerate <- function(p, p1, p2){
        # only accelerate if parameter has changed in last iteration
        d <- p2 != p1
        p2[d] <- p[d] - (p1[d] - p[d])^2 / (p2[d] - 2 * p1[d] + p[d])
        p2
    }
    # stopping rule: compare observed & expected sufficient stats
    checkConv <- function(res){
        assign("eps", abs(c(A, B[-1]) - c(res$expA, res$delta[-1]*res$expB)),
               envir = parent.env(environment()))
        all(eps < epsilon)
    }
    if (conv <- checkConv(res)) {
        maxit <- iter <- 0
    } else iter <- 1
    updateIter <- function(){
        if (trace) message("iter: ", iter)
        assign("iter", iter + 1,
               envir = parent.env(environment()))
    }
    doSteffensen <- FALSE
    for (i in 1:10){
        res <- oneUpdate(res)
    }
}
