# basic function to create multinomial outcomes for partial rankings with ties
# assumes records are dense rankings
#' @importFrom utils combn
longdat2 <- function(R){
    N <- ncol(R)
    D <- max(apply(R, 1, function(x) max(tabulate(x))))
    X <- list()
    for (d in seq_len(D)){
        comb <- combn(seq_len(N), d)
        A <- matrix(0, nrow = ncol(comb), ncol = N)
        A[cbind(rep(seq_len(ncol(comb)), each = nrow(comb)), c(comb))] <- 1/d
        B <- matrix(0, nrow = ncol(comb), ncol = D - 1)
        if (ncol(B)) B[, d - 1] <- 1
        X[[d]] <- cbind(A, B)
    }
    resX <- resY <- resZ <- list()
    k <- 1
    for (i in seq_len(nrow(R))){
        J <- max(R[i,])
        J <- J - (sum(R[i,] == J) == 1)
        resX[[i]] <- resY[[i]] <- resZ[[i]] <- list()
        for (j in seq_len(J)){
            id <- which(R[i,] < j)
            Xij <- list()
            for (d in seq_len(min(D, N - length(id)))){
                keep <- rowSums(X[[d]][, id, drop = FALSE]) == 0
                Xij[[d]] <- X[[d]][keep,]
            }
            resX[[i]][[j]] <- do.call("rbind", Xij)
            resZ[[i]][[j]] <- rep(k, nrow(resX[[i]][[j]]))
            resY[[i]][[j]] <- numeric(nrow(resX[[i]][[j]]))
            id <- colSums(t(resX[[i]][[j]][, seq_len(N)]) == (R[i,] == j)/
                              sum(R[i,] == j)) == N
            resY[[i]][[j]][id] <- 1
            k <- k + 1
        }
    }
    res <- data.frame(y = unlist(resY))
    res$X <- do.call("rbind", unlist(resX, recursive = FALSE))
    res$z <- factor(unlist(resZ))
    res
}
