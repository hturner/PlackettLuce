# MM algorithm for simple BT
#' @importFrom stats model.matrix
BT <- function(M, maxit = 500){
    # number of players
    N <- max(M)
    gamma <- rep.int(1/N, N)
    X <- model.matrix(~as.factor(M[,1]) - 1)
    X <- X + model.matrix(~as.factor(M[,2]) - 1)
    wins <- tabulate(M[,1])
    for (i in seq_len(maxit)){
        denom <- 1/(gamma[M[,1]] + gamma[M[,2]])
        gamma2 <- c(wins * 1/(t(X) %*% denom))
        if (isTRUE(all.equal(log(gamma), log(gamma2)))) break
        gamma <- gamma2
    }
    structure(gamma2, iter = i)
}

# MM algorithm for simple PL
PL <- function(M, maxit = 500){
    M <- as.matrix(M)
    N <- max(M)
    gamma <- rep.int(1/N, N)
    J <- rowSums(M != 0) - 1
    j <- sequence(J)
    i <- rep(seq_along(J), J)
    wins <- tabulate(c(M[cbind(i,j)]))
    for (k in seq_len(maxit)){
        gamma2 <- numeric(N)
        for (i in seq_len(N)){
            for (j in seq_len(nrow(M))){
                if (sum(M[j,i:N] > 0) > 1){
                    gamma2[M[j,i:N]] <-
                        gamma2[M[j,i:N]] + 1/sum(gamma[M[j,i:N]])
                }
            }
        }
        gamma2 <- wins/gamma2
        if (isTRUE(all.equal(log(gamma), log(gamma2)))) break
        gamma <- gamma2
    }
    structure(gamma2, iter = i)
}

# MM algorithm for BT model with ties (Davidson) - not right
BTties <- function(R, maxit = 500){
    # number of players
    N <- ncol(R)
    tie <- apply(R, 1, max) == 1
    wins <- colSums(R[!tie,] == 1)
    ties <- colSums(R[tie,] == 1)
    t <- sum(tie)
    theta <- 1
    gamma <- rep.int(1/N, N)
    i <- apply(R > 0, 1, function(x) which(x)[1])
    j <- apply(R > 0, 1, function(x) which(x)[2])
    obji <- sort(unique(i))
    objj <- sort(unique(j))
    for (k in seq_len(maxit)){
        g <- (2 + theta*sqrt(gamma[j]/gamma[i]))/(
                gamma[i] + gamma[j] + theta*sqrt(gamma[i]*gamma[j]))
        denom <- numeric(N)
        denom[obji] <- rowsum(g, i)
        denom[objj] <- denom[objj] + rowsum(g, j)
        gamma2 <- (2*wins + ties)/denom
        # normalize
        gamma2 <- gamma2/sum(gamma2)
        # ?? 4*t* produces silly answer, 4*t/ still not MLE
        theta2 <- 4*t/sum((2-tie)*(gamma2[i] + gamma2[j])/(
            gamma2[i] + gamma2[j] + theta*sqrt(gamma2[i]*gamma2[j])))
        if (isTRUE(all.equal(log(gamma), log(gamma2)))) break
        gamma <- gamma2
        theta <- theta2
    }
    structure(c(theta2, gamma2), iter = k)
}
