## N.B. current version computes expA twice when not doing Steffensen!
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
    N <- ncol(R)
    ## (scaled, un-damped) PageRank based on underlying paired comparisons
    if (is.null(colnames(R))) colnames(R) <- 1:ncol(R)
    X <- as_adj(graph_from_edgelist(as.edgelist.rankings(R)))[colnames(R),
                                                              colnames(R)]
    alpha <- drop(abs(eigs(X/colSums(X), 1,
                           opts = list(ncv = min(nrow(X), 10)))$vectors))
    # alpha <- alpha/sum(alpha) current version doesnt divide by alpha
    delta <- rep.int(0.1, D)
    delta[1] <- 1

    # item indices
    nz <- rowSums(R == 0)
    K <- apply(R, 1, sort)
    grp <- apply(rbind(0, K[-ncol(R), ]), 2, diff)
    K <- t(apply(R, 1, order))
    K[cbind(rep.int(1:nrow(R), nz), sequence(nz))] <- 0

    # set sizes to consider
    S <- which(colSums(K[,-ncol(R)]) > 0)

    expectation <- function(par, alpha, delta, K, N, D, S, trace = FALSE){
        n <- switch(par,
                    "alpha" = N,
                    "delta" = D - 1)
        if (trace) message("par: ", par)
        res <- numeric(n)
        for (s in S){
            # number of objects to select from
            nobj <- N - s + 1
            # D == 1
            ## numerators (for par == "alpha", else just to compute z1)
            y1 <- matrix(alpha[as.vector(K[grp[s,] == 1, s:N])],
                         nrow = sum(grp[s,]), ncol = nobj)
            ## denominators
            z1 <- rowSums(y1)
            # D > 1
            d <- min(D, nobj)
            if (d > 1){
                if (par == "delta")
                    y1 <- matrix(0, nrow = sum(grp[s,]), ncol = n)
                # index up to d items: start with 1:n
                i <- seq_len(d)
                # id = index to change next; id2 = first index changed
                if (d == nobj) {
                    id <- nobj - 1
                } else id <- d
                id2 <- 1
                repeat{
                    # work along index vector from 1 to end/first index = nobj
                    x1 <- alpha[K[grp[s,] == 1, (s:N)[i[1]]]] # ability for first ranked item
                    last <- i[id] == nobj
                    if (last) {
                        end <- id
                    } else end <- min(d, id + 1)
                    for (k in 2:end){
                        # product of first k alphas indexed by i
                        x1 <- (x1 * alpha[K[grp[s,] == 1, (s:N)[i[k]]]])
                        # ignore if already recorded
                        if (k < id2) next
                       # if (trace) message("i: ", paste(i, collapse = ", "))
                        # add to numerators/denominators for sets of order nobj
                        if (par == "alpha") {
                            x2 <- delta[k]*x1^(1/k)
                            # add to numerators for objects in sets
                            y1[, i[1:k]] <- y1[, i[1:k]] + x2/k
                        #    if (trace)
                         #       message("x2/k: ", paste(round(x2/k, 7), collapse = ", "))
                            # add to denominators for sets
                            z1 <- z1 + x2
                          #  if (trace)
                           #     message("x2: ", x2)
                        } else {
                            x2 <- x1^(1/k)
                            # add to numerator for current tie order for sets
                            if (par == "delta")
                                y1[, k - 1] <- y1[, k - 1] + x2
                            # add to denominators for sets
                            z1 <- z1 + delta[k]*x2
                        }
                    }
                    # update index
                    if (i[1] == (nobj - 1)) break
                    if (last){
                        id2 <- id - 1
                        v <- i[id2]
                        len <- min(nobj - 2 - v, d - id2)
                        id <- id2 + len
                        i[id2:id] <- v + seq_len(len + 1)
                    } else {
                        id2 <- id
                        i[id] <- i[id] + 1
                    }
                }
            }
            # add contribution for sets of size nobj to expectation
            if (trace){
                message("items: ")
                print(K[grp[s,] == 1, s:N])
                message("y1: ")
                print(y1)
                message("z1: ")
                print(z1)
            }
            if (par == "alpha"){
                # K[s:N, grp[s,] == 1] may only index some alphas
                add <- drop(rowsum(as.vector(y1/z1),
                                   as.vector(K[grp[s,] == 1, s:N])))
                res[as.numeric(names(add))] <- res[as.numeric(names(add))] + add
            } else res <- res + colSums(y1/z1)
        }
        res
    }

    expA <- expectation("alpha", alpha, delta, K, N, D, S, trace = trace)
    for (iter in seq_len(maxit)){
        # update all alphas
        alpha <- alpha*A/expA
        # update all deltas
        if (D > 1) delta[-1] <- B[-1]/
                expectation("delta", alpha, delta, K, N, D, S, trace = trace)
        # trace
        if (trace){
            message("iter ", iter)
        }
        # stopping rule: compare observed & expected sufficient stats for alphas
        expA <- expectation("alpha", alpha, delta, K, N, D, S)
        eps <- abs(A - expA)
        if (all(eps < epsilon)) {
            conv <- TRUE
            break
        }
    }
    structure(c(alpha/sum(alpha), delta[-1]), iter = iter)
}
