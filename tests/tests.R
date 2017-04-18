## basic function to create multinomial outcomes for partial rankings, no ties
## assumes records are items ranked 1st, 2nd, ..., with 0 for no item ranked
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

## convert ranked items to dense rankings for each object
denseRanking <- function(M){
    t(apply(M, 1, function(x) match(1:max(M), x, nomatch = 0)))
}

## simple BT model
M <- matrix(c(1, 2,
              3, 1,
              1, 4,
              2, 1,
              2, 3,
              4, 3), nrow = 6, byrow = TRUE)
dat <- longdat(M)
mod <- gnm(y ~ -1 + X, family = poisson, eliminate = z, data = dat, constrain = 1)
coef(mod)

library(BradleyTerry2)
mod2 <- BTm(rep(1, 6), factor(M[,1]), factor(M[,2]))

R <- denseRanking(M)
mod3 <- PlackettLuce(R)
lambda <- log(coef(mod3))
lambda <- lambda - lambda[1]
all.equal(lambda[-1], unname(mod$coefficients[-1]), tolerance = 1e-7)
all.equal(lambda[-1], unname(mod2$coefficients), tolerance = 1e-7)

## partial rankings, no ties
M <- matrix(c(1, 2, 0, 0,
              3, 1, 4, 0,
              1, 4, 0, 0,
              2, 1, 4, 3,
              2, 3, 4, 0,
              1, 2, 3, 0), nrow = 6, byrow = TRUE)
dat <- longdat(M)
mod <- gnm(y ~ -1 + X, family = poisson, eliminate = z, data = dat, constrain = 1)
coef(mod)

R <- denseRanking(M)
mod2 <- PlackettLuce(R)
lambda <- log(coef(mod2))
lambda <- lambda - lambda[1]
all.equal(lambda[-1], unname(mod$coefficients[-1]), tolerance = 1e-7)

## Nascar example from Hunter
## 36 races. Partial rankings of length (42 or 43), ranking 83 drivers in 1st to
## 83rd place (puts zero for 43rd or 44th to last place). No ties.
library(StatRank)
data(Data.Nascar)
## takes ~10min; not sure how to compare
## a <- Estimation.PL.MLE(Data.Nascar)$Mean

dat <- longdat(Data.Nascar)
## fairly quick - A. Cameron (driver with ID 1) is fixed at zero
library(gnm)
mod <- gnm(y ~ -1 + X, family = poisson, eliminate = z, data = dat,
           constrain = 1)
coef(mod)
## results seem to match okay (need to incorporate names etc for better example)
coef(mod)[58] # P. Jones
coef(mod)[68] # S. Pruett
coef(mod)[40] # J. Varde
## slow, leave for now
## summary(mod)

## slower but not too bad; doesn't agree though
R <- denseRanking(Data.Nascar)
mod2 <- PlackettLuce(R, trace = TRUE)
lambda <- log(coef(mod2))
lambda <- lambda - lambda[1]
all.equal(lambda[-1], unname(mod$coefficients[-1]), tolerance = 1e-7)
lambda[58]



## later look at Davidson model example to test ties
