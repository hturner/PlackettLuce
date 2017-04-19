##' Fit Plackett-Luce Model
##'
##' @param rankings a matrix of dense rankings (rankings by objects)
##' @param maxit the maximum number of iterations
##'
##' @return
##' @export
##'
##' @examples
##' @import Matrix
PlackettLuce <- function(rankings, maxit = 100, trace = FALSE){
    N <- ncol(rankings)
    M <- t(Matrix(rankings, sparse = TRUE))

    ## max nsets
    J <- apply(M, 2, max)

    ## highest degree of ties
    ## (specific to dense ranking - need to check and convert other rankings)
    n <- diff(M@p)
    ties0 <- n - J
    D <- max(ties0) + 1

    ## nontrivial nsets (excluding untied last place)
    J <- J - as.numeric(rowSums(t(M) == J) == 1)

    ## sizes of selected sets (not particularly efficient - could all be == 1!)
    S <- M
    S[t(t(M) > J)] <- 0
    S@x <- 1/as.double(unlist(apply(S, 2, function(x) tabulate(x)[x])))

    ## sufficient statistics

    ## for alpha i, sum over all sets of object i is in selected set/size of selected set
    A <- rowSums(S)
    ## for delta d, number of sets with cardinality d
    B <- tabulate(1/S@x)

    ## unique columns with >= 1 element for logical matrix
    uniquecol <- function(M, rep = TRUE){
        min2 <- colSums(M) > 1
        if (!any(min2)) return(NULL)
        for (i in seq_len(nrow(M))){
            if (i == 1) {
                pattern <- M[1,]
            } else {
                pattern <- 2*pattern + M[i,]
                pattern <- match(pattern, unique(pattern))
            }
        }
        uniq <- !duplicated(pattern) & min2
        ind <- match(pattern, pattern[uniq])
        if (rep){
            rep <- tabulate(ind)
            structure(M[, uniq, drop = FALSE], rep = rep)
        } else structure(M[, uniq, drop = FALSE], ind = ind)
    }

    ## get unique sets and reps
    pattern <- list()
    for (j in seq_len(max(J))){
        pattern[[j]] <- uniquecol(M >= j)
    }
    rep <- unlist(lapply(pattern, attr, "rep"))
    pattern <- uniquecol(do.call("cBind", pattern), rep = FALSE)
    rep <- c(rowsum(rep, attr(pattern, "ind")))
    S <- length(rep)

    ## starting values
    alpha <- rep.int(1, N)
    delta <- rep.int(0.1, D)
    delta[1] <- 1
    theta <- rep.int(1, S)

    ## iterative scaling
    expectation <- function(par){
        n <- switch(par,
                    "alpha" = N,
                    "beta" = D,
                    "theta" = S)
        res <- numeric(n)

        for (k in seq_len(S)){
            ## objects in set
            w <- which(pattern[, k])
            nobj <- length(w)
            ## ties of order d
            for (d in seq_len(min(D, nobj))){
                i <- seq_len(d)
                maxi <- rev(nobj - seq_len(d) + 1)
                ## loop through all combinations of objects in set
                repeat{
                    x <- theta[k] * rep[k] * delta[d] * prod(alpha[w[i]])^(1/d)
                    ## add to sums for all objects in set
                    if (par == "alpha") res[w[i]] <- res[w[i]] + x
                    ## add to sum for current order
                    if (par == "beta") res[d] <- res[d] + x
                    ## add to sum for current set
                    if (par == "theta") res[k] <- res[k] + x
                    if (i[1] == maxi[1]) break
                    id <- max(which(maxi - i > 0))
                    i[id:d] <- i[id] + seq_len(d - id + 1)
                }
            }
        }
        res
    }

    for (iter in seq_len(maxit)){
        ## update all alphas
        old <- alpha
        alpha <- alpha*A/expectation("alpha")
        ## scale alphas to mean 1
        alpha <- alpha/mean(alpha)
        ## update all deltas
        if (D > 1) delta[-1] <- delta[-1] * B[-1]/expectation("beta")[-1]
        ## scale all multinomial totals to rep
        theta <- theta * rep/expectation("theta")
        ## trace
        if (trace){
            message("iter ", iter, "\n")
        }
        ## simple stopping rule just based on alphas for now
        if (isTRUE(all.equal(log(old), log(alpha)))) break
    }

    list(coefficients = structure(c(alpha, delta[-1]),
                                  eliminated = theta),
         iter = iter)
}
