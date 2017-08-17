## N.B. current version computes expA twice when not doing Steffensen!
PL2 <- function(R, epsilon = 1e-7, maxit = 100, trace = FALSE){
    # sparse matrix of dense rankings (rankings object should be sparse already)
    R <- Matrix(R, sparse = TRUE)

    # nontrivial nsets (excluding untied last place)
    J <- apply(R, 1, max)
    J <- J - as.numeric(rowSums(R == J) == 1)

    # size of selected sets
    S <- R
    S[which(R > J)] <- 0
    S <- t(S)
    S@x <- 1/as.double(unlist(apply(S, 2, function(x) tabulate(x)[x])))
    S <- t(S)

    # sufficient statistics

    # for alpha i, sum over all sets st object i is in selected set/size of selected set
    A <- colSums(S)
    # for delta d, number of sets with cardinality d/cardinality
    S@x <- 1/S@x
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

    # update S so cols are potential set sizes and value > 0 indicates ranking
    # selects from that set size
    S <- apply(R, 1, sort, decreasing = TRUE)
    S <- as(-1*t(apply(rbind(S, 0), 2, diff)), "dgCMatrix")

    # set sizes present in rankings
    P <- which(colSums(S[, -1, drop = FALSE]) > 0) + 1

    # max number of objects
    N <- ncol(S)

    # aggregate partial rankings by set size
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

    for (i in P){
        r <- which(S[,i] > 0)
        nr <- length(r)
        if (i == N) {
            S[,i][S[,i] > 0] <- c(nr, numeric(nr - 1)) # always 1 group
            next
        }
        # (one of the) item(s) in i'th from last place
        minrank <- rowSums(S[r,i:N])
        # items in final set of size i
        pattern <- R[r,] >= minrank
        # find unique sets and select representative ranking to compute on
        g <- uniquerow(pattern)
        ng <- tabulate(g)
        if (any(ng > 1)){
            dup <- duplicated(g)
            S[r[dup], i] <- 0
            S[r[!dup], i] <- ng
        }
    }

    # multiply by rankings weights here

    # replace rankings with items ordered by ranking (all that is needed now)
    R <- t(apply(R, 1, order, decreasing = TRUE))

    # drop any completely replicated rankings
    keep <- diff(S@p) != 0
    if (length(keep) < N){
        R <- R[,keep]
        S <- S[,keep]
    }

    expectation <- function(par, alpha, delta, R, S, N, D, P, trace = FALSE){
        n <- switch(par,
                    "alpha" = N,
                    "delta" = D - 1)
        if (trace) message("par: ", par)
        res <- numeric(n)
        for (p in P){
            # D == 1
            ## numerators (for par == "alpha", else just to compute z1)
            r <- S[, p] > 0
            nr <- sum(r)
            y1 <- matrix(alpha[as.vector(R[r, 1:p])],
                         nrow = nr, ncol = p)
            ## denominators
            z1 <- rowSums(y1)
            # D > 1
            d <- min(D, p)
            if (d > 1){
                if (par == "delta")
                    y1 <- matrix(0, nrow = nr, ncol = n)
                # index up to d items: start with 1:n
                i <- seq_len(d)
                # id = index to change next; id2 = first index changed
                if (d == p) {
                    id <- p - 1
                } else id <- d
                id2 <- 1
                repeat{
                    # work along index vector from 1 to end/first index = p
                    x1 <- alpha[R[r, i[1]]] # ability for first ranked item
                    last <- i[id] == p
                    if (last) {
                        end <- id
                    } else end <- min(d, id + 1)
                    for (k in 2:end){
                        # product of first k alphas indexed by i
                        x1 <- x1 * alpha[R[r, i[k]]]
                        # ignore if already recorded
                        if (k < id2) next
                       if (trace) message("i: ", paste(i, collapse = ", "))
                        # add to numerators/denominators for sets of order p
                        if (par == "alpha") {
                            x2 <- delta[k]*x1^(1/k)
                            # add to numerators for objects in sets
                            y1[, i[1:k]] <- y1[, i[1:k]] + x2/k
                            if (trace)
                                message("x2/k: ", paste(round(x2/k, 7), collapse = ", "))
                            # add to denominators for sets
                            z1 <- z1 + x2
                            if (trace)
                                message("x2: ", x2)
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
                    if (i[1] == (p - 1)) break
                    if (last){
                        id2 <- id - 1
                        v <- i[id2]
                        len <- min(p - 2 - v, d - id2)
                        id <- id2 + len
                        i[id2:id] <- v + seq_len(len + 1)
                    } else {
                        id2 <- id
                        i[id] <- i[id] + 1
                    }
                }
            }
            # add contribution for sets of size p to expectation
            if (trace){
                message("items: ")
                print(R[r, 1:p])
                message("y1: ")
                print(y1)
                message("z1: ")
                print(z1)
            }
            if (par == "alpha"){
                # R[r, 1:p] may only index some alphas
                add <- drop(rowsum(as.vector(S[r, p] * y1/z1),
                                   as.vector(R[r, 1:p])))
                res[as.numeric(names(add))] <- res[as.numeric(names(add))] + add
            } else res <- res + colSums(S[r, p] * y1/z1)
        }
        res
    }

    expA <- expectation("alpha", alpha, delta, R, S, N, D, P, trace = trace)
    for (iter in seq_len(maxit)){
        # update all alphas
        alpha <- alpha*A/expA
        # update all deltas
        if (D > 1) delta[-1] <- B[-1]/
                expectation("delta", alpha, delta, R, S, N, D, P, trace = trace)
        # trace
        if (trace){
            message("iter ", iter)
        }
        # stopping rule: compare observed & expected sufficient stats for alphas
        expA <- expectation("alpha", alpha, delta, R, S, N, D, P)
        eps <- abs(A - expA)
        if (all(eps < epsilon)) {
            conv <- TRUE
            break
        }
    }

    structure(c(alpha/sum(alpha), delta[-1]), iter = iter)
}
