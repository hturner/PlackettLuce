## N.B. current version computes expA twice when not doing Steffensen!
PL3 <- function(R, epsilon = 1e-7, maxit = 100, trace = FALSE, alpha = NULL){
    # sparse matrix of dense rankings (rankings object should be sparse already)
    R <- Matrix(R, sparse = TRUE)

    # shift so max rank the same
    S <- R
    nitem <- rowSums(R != 0)
    m <- max(nitem)
    R@x <- (m - nitem + S)[which(R > 0)]

    # nontrivial nsets (excluding untied last place)
    J <- m - as.numeric(rowSums(R == m) == 1)

    # sufficient statistics

    # for alpha i, sum over all sets st object i is in selected set/size of selected set
    ## S = sizes of selected sets
    S[which(R > J)] <- 0
    S <- t(S)
    S@x <- as.double(unlist(apply(S, 2, function(x) tabulate(x)[x])))
    A <- unname(rowsum(1/S@x, S@i)[,1])

    # for delta d, number of sets with cardinality d/cardinality
    # now cols of S are potential set sizes and value > 0 indicates ranking
    # selects from that set size
    B <- tabulate(S@x)
    D <- length(B)
    B <- B/seq(D)

    # max number of objects
    N <- ncol(R)

    # starting values - as.edgelist.rankings doesn't currently work for Matrix

    # (scaled, un-damped) PageRank based on underlying paired comparisons
    if (is.null(colnames(R))) colnames(R) <- seq_len(N)
    #X <- matrix(0, ncol = N, nrow = N)
    #for (i in 1:nrow(R)){
    #    r <- sort(unique(R@x[R@i == (i - 1)]), decreasing = TRUE)
    #    nr <- length(r)
    #    if (nr == 1) next
    #    res1 <- which(R[i,] == r[1])
    #    for (j in 2:nr){
    #        if (j %% 2 == 0) {
    #            res2 <- which(R[i,] == r[j])
    #            X[res2, res1] <- X[res2, res1] + 1
    #        } else {
    #            res1 <- which(R[i,] == r[j])
    #            X[res1, res2] <- X[res1, res2] + 1
    #        }

    #    }
    #}
    #alpha <- drop(abs(eigs(X/colSums(X), 1,
    #                       opts = list(ncv = min(nrow(X), 10)))$vectors))
    # alpha <- alpha/sum(alpha) current version doesnt divide by alpha
    delta <- rep.int(0.1, D)

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

    # set sizes present in rankings
    P <- setdiff(sort(m - unique(R@x) + 1), 1)

    # create S so cols are potential set sizes and value > 0 indicates ranking
    # aggregate common sets
    # - incorporate ranking weights by weighted count vs simple tabulate
    S <- as(sparseMatrix(dims = dim(R), i = {}, j = {}), "dgCMatrix")
    for (i in P){
        r <- rowSums(drop0(R == (m - i + 1))) > 0
        pattern <- drop0(R[r,, drop = FALSE] >= (m - i + 1))
        g <- uniquerow(pattern)
        ng <- tabulate(g)
        S[r,, drop = FALSE][!duplicated(g), i] <- ng
    }

    # replace rankings with items ordered by ranking (all that is needed now)
    R <- t(apply(R, 1, order, decreasing = TRUE))

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
