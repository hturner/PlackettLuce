## N.B. current version computes expA twice when not doing Steffensen!
library(igraph)
library(Matrix)
library(rARPACK)

PL2 <- function(R, epsilon = 1e-7, maxit = 100, trace = FALSE){
    M <- t(Matrix(R, sparse = TRUE))

    # max nsets
    J <- apply(M, 2, max)

    # nontrivial nsets (excluding untied last place)
    J <- J - as.numeric(rowSums(t(M) == J) == 1)
    Jmax <- max(J)

    # remove singletons - is this needed now rankings are checked?
    singleton <- J == 0
    M <- M[, !singleton, drop = FALSE]
    J <- J[!singleton]

    # sizes of selected sets (not particularly efficient - could all be == 1!)
    S <- M
    S[t(t(M) > J)] <- 0
    # S is copied onto T here to be used by loglik
    T <- S
    S@x <- 1/as.double(unlist(apply(S, 2, function(x) tabulate(x)[x])))

    # sufficient statistics

    # for alpha i, sum over all sets of object i is in selected set/size of selected set
    A <- rowSums(S)
    # for delta d, number of sets with cardinality d
    B <- tabulate(1/S@x)
    D <- length(B)
    B <- B/seq(D)

    # starting values
    N <- ncol(rankings)
    ## (scaled, un-damped) PageRank based on underlying paired comparisons
    if (is.null(colnames(R))) colnames(R) <- 1:ncol(R)
    X <- as_adj(graph_from_edgelist(as.edgelist.rankings(R)))[colnames(R),
                                                              colnames(R)]
    alpha <- drop(abs(eigs(X/colSums(X), 1,
                           opts = list(ncv = min(nrow(X), 10)))$vectors))
    # alpha <- alpha/sum(alpha) current version doesnt divide by alpha
    delta <- rep.int(0.1, D)
    delta[1] <- 1

    # item indices (specific to pairs for now)
    n <- length(M@i)
    i <- (M@i + 1)[seq(1, n, by = 2)]
    j <- (M@i + 1)[seq(2, n, by = 2)]
    r <- rep_len(seq(nrow(M)), length(i) + length(j))

    expectation <- function(par, alpha, delta, M){
        # numerators
        if (par == "alpha") {
            y1 <- (M > 0) * alpha
        } else y1 <- numeric(D)
        if (D == 2){ # just D == 2 for now
            p <- (alpha[i]*alpha[j])^(1/2)
            if (par == "alpha") {
                y1[cbind(r, c(i, j))] <- y1 + delta[D]/D*p
            } else y1[D] <- p
        }
        # denominators
        z1 <- alpha[i] + alpha[j] #rowSums(M)
        if (D == 2){
            z1 <- z1 + delta[2]*p
        }
        colSums(t(y1)/z1)
    }

    for (iter in seq_len(maxit)){
        # update all alphas
        expA <- expectation("alpha", alpha, delta, M)
        # stopping rule: compare observed & expected sufficient stats for alphas
        eps <- abs(A - expA)
        if (all(eps < epsilon)) {
            conv <- TRUE
            break
        }
        alpha <- alpha*A/expA
        # update all deltas
        if (D > 1) delta[-1] <- B[-1]/
                expectation("delta", par$alpha, par$delta, M)[-1]
        # trace
        if (trace){
            message("iter ", iter)
        }
    }
    structure(c(alpha/sum(alpha), delta[-1]), iter = iter)
}
