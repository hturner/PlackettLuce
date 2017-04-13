## basic function to create multinomial outcomes for partial rankings, no ties
## assumes records are items ranked 1st, 2nd, ..., with 0 for no item ranked
longdat <- function(dat){
    N <- ncol(dat)
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

## Nascar example from Hunter
## 36 races. Partial rankings of length (42 or 43), ranking 83 drivers in 1st to
## 83rd place (puts zero for 43rd or 44th to last place). No ties.
library(StatRank)
data(Data.Nascar)
## takes ~10min; not sure how to compare
## a <- Estimation.PL.MLE(Data.Nascar)$Mean

dat <- longdat(Data.Nascar)
## fairly quick - A. Cameron (driver with ID 1) is fixed at zero
mod <- gnm(y ~ -1 + X, family = poisson, eliminate = z, data = dat,
           constrain = 1)
coef(mod)
## results seem to match okay (need to incorporate names etc for better example)
coef(mod)[58] # P. Jones
coef(mod)[68] # S. Pruett
coef(mod)[40] # J. Varde
## slow, leave for now
## summary(mod)

## now poisson model for simple example as starter case
M <- matrix(c(1, 2, 0, 0,
              3, 1, 4, 0,
              1, 4, 0, 0,
              2, 1, 4, 3,
              2, 3, 4, 0,
              1, 2, 3, 0), nrow = 6, byrow = TRUE)
dat <- longdat(M)
mod <- gnm(y ~ -1 + X, family = poisson, eliminate = z, data = dat, constrain = 1)
coef(mod)

## need to convert data to ranking per object format
## then compare to new function

## later look at Davidson model example to test ties
