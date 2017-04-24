# utility functions to create ranking data structures/X matrix for log-linear models
# maybe just for testing or at some point could be tidied up and exported

# basic function to create multinomial outcomes for partial rankings, no ties
# assumes records are items ranked 1st, 2nd, ..., with 0 for no item ranked
longdat <- function(dat){
    N <- max(dat)
    X <- diag(N)
    resX <- resY <- resZ <- list()
    k <- 1
    for (i in 1:nrow(dat)){
        y <- numeric(N)
        nobj <- max(which(dat[i,] != 0))
        resX[[i]] <- resY[[i]] <-resZ[[i]] <- list()
        for(j in 1:(nobj - 1)){
            y[dat[i,j]] <- 1
            ind <- unlist(dat[i, j:nobj])
            resY[[i]][[j]] <- y[ind]
            resX[[i]][[j]] <- X[ind, ]
            resZ[[i]][[j]] <- rep(k, nobj - j + 1)
            k <- k + 1
        }
    }
    res <- data.frame(y = unlist(resY))
    res$X <- do.call("rbind", unlist(resX, recursive = FALSE))
    res$z <- factor(unlist(resZ))
    res
}

# basic function to create multinomial outcomes for partial rankings with ties
# assumes records are dense rankings
#' @importFrom utils combn
longdat2 <- function(R){
    N <- ncol(R)
    D <- max(apply(R, 1, function(x) max(tabulate(x))))
    X <- list()
    for (d in 1:D){
        comb <- combn(1:N, d)
        A <- matrix(0, nrow = ncol(comb), ncol = N)
        A[cbind(rep(1:ncol(comb), each = nrow(comb)), c(comb))] <- 1/d
        B <- matrix(0, nrow = ncol(comb), ncol = D - 1)
        if (ncol(B)) B[, d - 1] <- 1
        X[[d]] <- cbind(A, B)
    }
    resX <- resY <- resZ <- list()
    k <- 1
    for (i in 1:nrow(R)){
        J <- max(R[i,])
        J <- J - (sum(R[i,] == J) == 1)
        resX[[i]] <- resY[[i]] <- resZ[[i]] <- list()
        for (j in seq_len(J)){
            id <- which(R[i,] < j)
            Xij <- list()
            for (d in 1:min(D, N - length(id))){
                keep <- rowSums(X[[d]][, id, drop = FALSE]) == 0
                Xij[[d]] <- X[[d]][keep,]
            }
            resX[[i]][[j]] <- do.call("rbind", Xij)
            resZ[[i]][[j]] <- rep(k, nrow(resX[[i]][[j]]))
            resY[[i]][[j]] <- numeric(nrow(resX[[i]][[j]]))
            id <- colSums(t(resX[[i]][[j]][, 1:N]) == (R[i,] == j)/sum(R[i,] == j)) == N
            resY[[i]][[j]][id] <- 1
            k <- k + 1
        }
    }
    res <- data.frame(y = unlist(resY))
    res$X <- do.call("rbind", unlist(resX, recursive = FALSE))
    res$z <- factor(unlist(resZ))
    res
}

# convert ranked items to dense rankings for each object
denseRanking <- function(M){
    t(apply(M, 1, function(x) match(1:max(M), x, nomatch = 0)))
}
