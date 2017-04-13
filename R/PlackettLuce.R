##' Fit Plackett-Luce Model
##'
##' @param rankings
##'
##' @return
##' @export
##'
##' @examples
##' @import Matrix
plackettLuce <- function(rankings){
    N <- ncol(rankings)
    M <- t(Matrix(rankings, sparse = TRUE))

    ## max nsets
    J <- apply(M, 2, max)

    ## highest degree of ranking
    ## (specific to dense ranking - need to check and convert other rankings)
    n <- diff(M@p)
    ties0 <- n - J
    D <- max(ties0) + 1

    ## nontrivial nsets (excluding untied last place)
    J <- J - as.numeric(colSums(M == J) == 1)

    ## sizes of selected sets (not particularly efficient - could all be == 1!)
    S <- M
    S@x <- 1/as.double(unlist(apply(M, 2, function(x) tabulate(x)[x])))

    ## sufficient statistics

    ## for alpha i, sum over all sets of object i is in selected set/size of selected set
    A <- rowSums(S)
    ## for delta d, number of sets with cardinality d
    B <- tabulate(1/S@x)

    ## unique columns with >= 1 element for logical matrix
    ## only good for ~1000 obj, else need to iterate across columns
    uniquecol <- function(M, rep = TRUE){
        min2 <- colSums(M) > 1
        if (!any(min2)) return(NULL)
        pattern <- drop(2^(nrow(M):1) %*% M)
        uniq <- !duplicated(pattern) & min2
        ind <- match(pattern, pattern[uniq])
        if (rep){
            rep <- tabulate(ind)
            structure(M[, uniq], rep = rep)
        } else structure(M[, uniq], ind = ind)
    }

    ## get unique sets and reps
    pattern <- list()
    for (j in seq_len(max(J))){
        pattern[[j]] <- uniquecol(M >= j)
    }
    rep <- unlist(lapply(pattern, attr, "rep"))
    pattern <- uniquecol(do.call("cBind", pattern), rep = FALSE)
    rep <- c(rowsum(rep, attr(pattern, "ind")))

    ## starting values
    alpha <- rep.int(1, N)
    delta <- rep.int(0.1, D)
    delta[1] <- 1
    theta <- rep.int(1, length(rep))

    ## iterative scaling

    for (iter in seq_len(maxit)){
        ## set expectations to zero
        ## per object (sum of combinations including that object; all orders)
        EA <- numeric(N)
        ## per tie order (sum of all object combinations)
        EB <- numeric(D)
        ## per (unique) set
        ET <- numeric(length(rep))

        ## compute expectations
        for (k in seq_along(rep)){
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
                    EA[w[i]] <- EA[w[i]] + x
                    if (d > 1) EB[d] <- EB[d] + x
                    ET[k] <- ET[k] + x
                    if (i[1] == maxi[1]) break
                    id <- max(which(maxi - i > 0))
                    i[id:d] <- i[id] + seq_len(d - id + 1)
                }
            }
        }

        ## update all alphas
        old <- alpha
        alpha <- alpha*A/EA
        ## scale alphas to mean 1
        alpha <- alpha/mean(alpha)
        ## update all deltas
        delta[-1] <- delta[-1] * B[-1]/EB[-1]
        ## scale all multinomial totals to 1
        theta <- theta/ET

        ## simple stopping rule just based on alphas for now
        if (isTRUE(all.equal(old, alpha))) break
    }

    structure(list(alpha = alpha, beta = beta[-1]),
              theta = theta)
}
