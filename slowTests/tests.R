library(PlackettLuce)

## simple BT model
M <- matrix(c(1, 2,
              3, 1,
              1, 4,
              2, 1,
              2, 3,
              4, 3), nrow = 6, byrow = TRUE)

R <- PlackettLuce:::denseRanking(M)
mod <- PlackettLuce(R)

if (require(gnm)){
    ## fit loglinear model
    dat <- PlackettLuce:::longdat(M)
    mod2 <- gnm(y ~ -1 + X, family = poisson, eliminate = z, data = dat,
                constrain = 1)
    ## compare coef
    all.equal(unname(coef(mod)[-1]), unname(coef(mod2)[-1]), tolerance = 1e-6)
}

if (require(BradleyTerry2)){
    ## fit loglinear model via BTm
    mod3 <- BTm(rep(1, 6), factor(M[,1]), factor(M[,2]))
    ## compare coef
    all.equal(unname(coef(mod)[-1]), unname(coef(mod3)), tolerance = 1e-6)
}

if (require(BradleyTerryScalable)){
    mod4 <- btfit(btdata(as.data.frame(cbind(M, 1))), 1, epsilon = 1e-7)
    pi <- mod4$pi[[1]]
    (pi/sum(pi))[dat$components$`1`]
    all.equal((pi/sum(pi))[dat$components$`1`], mod$coefficients,
              tolerance = 1e-7)
}

## bigger BT model (data from BradleyTerry2)
if (require(BradleyTerry2)){
    ## loglinear model with BTm
    icehockey2 <- subset(icehockey, result != 0.5) #remove ties
    standardBT <- BTm(outcome = result,
                      player1 = visitor, player2 = opponent,
                      id = "team", data = icehockey2)
    ## compare to PlackettLuce
    R <- matrix(0, nrow = nrow(icehockey2),
                ncol = nlevels(icehockey2$visitor))
    R[cbind(1:nrow(icehockey2), icehockey2$visitor)] <- 2 - icehockey2$result
    R[cbind(1:nrow(icehockey2), icehockey2$opponent)] <- icehockey2$result + 1
    ## needs 170 iterations, slow
    mod3 <- PlackettLuce(R, maxit = 500, epsilon = 1e-5)
    all.equal(unname(c(coef(mod3)[-1], mod3$loglik)), unname(c(standardBT$coefficients, logLik(standardBT))), tolerance = 1e-5)
}

## partial rankings, no ties
M <- matrix(c(1, 2, 0, 0,
              3, 1, 4, 0,
              1, 4, 0, 0,
              2, 1, 4, 3,
              2, 3, 4, 0,
              1, 2, 3, 0), nrow = 6, byrow = TRUE)

## via Hunter's MM (unexported function)
gamma <- PlackettLuce:::PL(M)

## via PlackettLuce
R <- PlackettLuce:::denseRanking(M)
mod <- PlackettLuce(R)
lambda <- log(c(gamma/sum(gamma)))
lambda <- lambda - lambda[1]
all.equal(unname(lambda), c(unname(coef(mod))), tol = 1e-7)

if (require(gnm)){
    ## fit loglinear model
    dat <- PlackettLuce:::longdat(M)
    mod2 <- gnm(y ~ -1 + X, family = poisson, eliminate = z, data = dat,
                constrain = 1)
    ## compare coef
    all.equal(unname(coef(mod)[-1]), unname(coef(mod2)[-1]), tolerance = 1e-7)
}

## Nascar example from Hunter
if (require(StatRank)){
    ## 36 races. Partial rankings of length (42 or 43), ranking 83 drivers in 1st to
    ## 83rd place (puts zero for 43rd or 44th to last place). No ties.
    data(Data.Nascar)
    ## StatRank PL function takes ~10min; not sure how to compare coef
    ## a <- Estimation.PL.MLE(Data.Nascar)$Mean

    dat <- PlackettLuce:::longdat(Data.Nascar)
    ## fairly quick - A. Cameron (driver with ID 1) is fixed at zero
    mod <- gnm(y ~ -1 + X, family = poisson, eliminate = z, data = dat,
               constrain = 1)
    ## results seem to match okay (need to incorporate names etc for better example)
    coef(mod)[58] # P. Jones
    coef(mod)[68] # S. Pruett
    coef(mod)[40] # J. Varde
    ## slow, leave for now
    ## summary(mod)

    ## actually faster than gnm
    gamma <- PlackettLuce:::PL(Data.Nascar)
    alpha <- log(gamma)
    alpha <- alpha - alpha[1]
    all.equal(alpha[-1], unname(coef(mod)[-1]), tolerance = 1e-5)

    ## much slower
    R <- PlackettLuce:::denseRanking(Data.Nascar)
    mod2 <- PlackettLuce(R)
    all.equal(unname(coef(mod2)[-1]), unname(mod$coefficients[-1]), tolerance = 1e-6)
}

## simple BT model with ties (Davidson)

## pudding data (Example 2, Davidson 1970) is in tests/pudding.rda
load("pudding.rda")
## to match paper
Dav <- with(pudding, PlackettLuce:::Davidson(i, j, r_ij, w_ij, w_ji, t_ij, maxit = 7))
## more precise
Dav <- with(pudding, PlackettLuce:::Davidson(i, j, r_ij, w_ij, w_ji, t_ij))

## inefficient representation here...
R <- with(pudding,
     {
         R <- list()
         n <- max(i, j)
         for (r in seq_len(nrow(pudding))){
             R[[r]] <- matrix(0, nrow = r_ij[r], ncol = n)
             a <- rep(c(1, 2, 1), c(w_ij[r], w_ji[r], t_ij[r]))
             b <- rep(c(2, 1, 1), c(w_ij[r], w_ji[r], t_ij[r]))
             R[[r]][, c(i[r], j[r])] <- c(a, b)
         }
         do.call("rbind", R)
     })
mod <- PlackettLuce(R)
alpha <- coef(mod)
# tie parameter
n <- length(alpha)
all.equal(log(Dav[1]), unname(alpha[n]))
# abilities
lambda <- log(Dav[-1])
lambda <- lambda - lambda[1]
all.equal(lambda, unname(alpha[-n]), tol = 1e-6)

if (require(gnm)){
    dat <- PlackettLuce:::longdat2(R)
    mod2 <- gnm(y ~ -1 + X, family = poisson, eliminate = z, data = dat,
                constrain = 1)
    coef(mod2)
   all.equal(unname(coef(mod2)[-1]), unname(coef(mod)[-1]), tol = 1e-6)
}

## partial rankings, three-way ties
R <- matrix(c(1, 2, 0, 0,
              4, 1, 2, 3,
              1, 2, 3, 0,
              2, 1, 1, 1,
              2, 1, 1, 0,
              1, 0, 3, 2), nrow = 6, byrow = TRUE)

mod <- PlackettLuce(R)

if (require(gnm)){
    dat <- PlackettLuce:::longdat2(R)
    mod2 <- gnm(y ~ -1 + X, family = poisson, eliminate = z, data = dat,
                constrain = 1)
    coef(mod2)
    n <- ncol(R)
    all.equal(unname(coef(mod2)[-1]), unname(coef(mod)[-1]),
              tolerance = 1e-7)
}

## disconnected networks

## simple BT model
R <- matrix(c(1, 2, 0, 0,
              2, 0, 1, 0,
              1, 0, 0, 2,
              2, 1, 0, 0,
              0, 1, 2, 0,
              0, 0, 2, 0), byrow = TRUE, ncol = 4,
            dimnames = list(NULL, letters[1:4]))

## error
mod <- PlackettLuce(R, network = "connected")
## first 3 items only
mod <- PlackettLuce(R, network = "cluster")
coef(mod)
summary(mod)
## with pseudodata (same as network = "pseudodata")
mod <- PlackettLuce(R, network = "adaptive")
coef(mod)
summary(mod)
