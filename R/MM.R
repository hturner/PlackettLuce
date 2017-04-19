## MM algorithm for simple BT
BT <- function(M, maxit = 500){
    ## number of players
    N <- max(M)
    gamma <- rep.int(1/N, N)
    X <- model.matrix(~as.factor(M[,1]) - 1)
    X <- X + model.matrix(~as.factor(M[,2]) - 1)
    wins <- tabulate(M[,1])
    for (i in 1:maxit){
        denom <- 1/(gamma[M[,1]] + gamma[M[,2]])
        gamma2 <- c(wins * 1/(t(X) %*% denom))
        if (isTRUE(all.equal(log(gamma), log(gamma2)))) break
        gamma <- gamma2
    }
    structure(gamma2, iter = i)
}

## MM algorithm for simple PL
PL <- function(M, maxit = 500){
    M <- as.matrix(M)
    N <- max(M)
    gamma <- rep.int(1/N, N)
    J <- rowSums(M != 0) - 1
    j <- sequence(J)
    i <- rep(seq_along(J), J)
    wins <- tabulate(c(M[cbind(i,j)]))
    for (k in 1:maxit){
        gamma2 <- numeric(N)
        for (i in 1:N){
            for (j in 1:nrow(M)){
                if (sum(M[j,i:N] > 0) > 1){
                    gamma2[M[j,i:N]] <- gamma2[M[j,i:N]] + 1/sum(gamma[M[j,i:N]])
                }
            }
        }
        gamma2 <- wins/gamma2
        if (k == 1){
            wins <<- wins
            gamma1 <<- gamma
            gamma2 <<- gamma2
            J <<- J
        }
        if (isTRUE(all.equal(log(gamma), log(gamma2)))) break
        gamma <- gamma2
    }
    gamma2
}
