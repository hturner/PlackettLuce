context("implementation [log-likelihood and coefficients]")

## Get the legacy implementation
source_files <- c("PlackettLuce0.R", "coef0.R",
                  "fitted0.R", "logLik0.R",
                  "print.PlackettLuce0.R", "summary0.R",
                  "vcov0.R")

for (file0 in source_files) {
    source(system.file("PlackettLuce0", file0, package = "PlackettLuce"))
}


coef_tol <- 1e-06
loglik_tol <- 1e-12

## Extractor for the log-likelihood of poisson gnm's adjusting for the fixed part (-mu) due to the extra nuisance parameters
logLik_poisson.gnm <- function(x) {
    logLik(x) + nlevels(x$eliminate)
}

## The artificial example in ?PlackettLuce
R <- matrix(c(1, 2, 0, 0,
              4, 1, 2, 3,
              2, 1, 1, 1,
              1, 2, 3, 0,
              2, 1, 1, 0,
              1, 0, 3, 2), nrow = 6, byrow = TRUE)
colnames(R) <- c("apple", "banana", "orange", "pear")
if (require("Matrix") & require("igraph") & require("rARPACK")) {
    model0 <- PlackettLuce0(rankings = R, ref = "orange")
    model1 <- PlackettLuce(rankings = R, ref = "orange")
    test_that("estimates match legacy implementation [artificial partial rankings with ties]", {
        # coefficients
        expect_equal(unname(unclass(coef(model0))), unname(unclass(coef(model1))), tolerance = coef_tol)
    })
    test_that("log-likelihood matches legacy implementation [artificial partial rankings with ties]", {
        # log-likelihood
        expect_equal(logLik(model0), logLik(model1), tolerance = loglik_tol)
    })
}

## simple BT model
M <- matrix(c(1, 2,
              3, 1,
              1, 4,
              2, 1,
              2, 3,
              4, 3), nrow = 6, byrow = TRUE)
R <- PlackettLuce:::denseRanking(M)
mod1 <- PlackettLuce(R)
dat <- PlackettLuce:::longdat(M)
if (require(gnm) & require(BradleyTerry2) & require(BradleyTerryScalable)){
    ## fit loglinear model
    mod2 <- gnm(y ~ -1 + X, family = poisson, eliminate = z, data = dat,
                constrain = 1)
    BT_data <- data.frame(p1 = factor(M[,1]), p2 = factor(M[,2]))
    mod3 <- BTm(rep(1, 6), p1, p2, data = BT_data)
    cdat <- btdata(as.data.frame(cbind(M, 1)))
    mod4 <- btfit(cdat, 1, epsilon = 1e-7)
    pp <- mod4$pi[[1]]
    pp <- (pp/sum(pp))[cdat$components$`1`]
    pp <- log(pp) - log(pp)[1]
    test_that("estimates match gnm, BTm, btfit [artificial paired comparisons]", {
        ## coefficients
        expect_equal(unname(coef(mod1)[-1]), unname(coef(mod2)[-1]), tolerance = coef_tol)
        expect_equal(unname(coef(mod2)[-1]), unname(coef(mod3)), tolerance = coef_tol)
        expect_equal(unname(coef(mod3)), unname(pp[-1]), tolerance = coef_tol)
    })
    test_that("log-likelihood matches gnm, BTm, btfit [artificial paired comparison]", {
        ## log-likelihood
        expect_equal(logLik(mod1), logLik(mod3), check.attributes = FALSE, tolerance = 1e-12)
        expect_equal(logLik(mod3), logLik_poisson.gnm(mod2), check.attributes = FALSE, tolerance = 1e-12)
    })
}


## Icehockey data
if (require(BradleyTerry2)){
    ## loglinear model with BTm
    icehockey2 <- subset(icehockey, result != 0.5) #remove ties
    mod_BT <- BTm(outcome = result,
                      player1 = visitor, player2 = opponent,
                      id = "team", data = icehockey2)
    ## compare to PlackettLuce
    R <- matrix(0, nrow = nrow(icehockey2),
                ncol = nlevels(icehockey2$visitor))
    R[cbind(1:nrow(icehockey2), icehockey2$visitor)] <- 2 - icehockey2$result
    R[cbind(1:nrow(icehockey2), icehockey2$opponent)] <- icehockey2$result + 1
    ## needs 170 iterations, slow
    mod_PL <- PlackettLuce(R, maxit = 500, epsilon = 1e-5)
    test_that("estimates match BTm [icehockey]", {
        expect_equal(unname(coef(mod_BT)), unname(coef(mod_PL))[-1], tolerance = coef_tol)
    })
    test_that("log-likelihood matches BTm [icehockey]", {
        expect_equal(logLik(mod_BT), logLik(mod_PL), check.attributes = FALSE, tolerance = loglik_tol)
    })
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
mod1 <- PlackettLuce(R)
lambda <- log(c(gamma/sum(gamma)))
lambda <- lambda - lambda[1]
if (require(gnm)){
    ## fit loglinear model
    dat <- PlackettLuce:::longdat(M)
    mod2 <- gnm(y ~ -1 + X, family = poisson, eliminate = z, data = dat,
                constrain = 1)
    test_that("estimates match Hunter's MM, gnm [artificial partial rankings with no ties]", {
        expect_equal(unname(coef(mod1))[-1], unname(coef(mod2))[-1], tolerance = coef_tol)
        expect_equal(unname(c(coef(mod1))), lambda, tolerance = coef_tol)
    })
    test_that("log-likelihood matches Hunter's MM, gnm [artificial partial rankings with no ties]", {
        expect_equal(logLik(mod1), logLik_poisson.gnm(mod2), check.attributes = FALSE, tolerance = loglik_tol)
    })
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
    mod2 <- gnm(y ~ -1 + X, family = poisson, eliminate = z, data = dat,
                constrain = 1)
    gamma <- PlackettLuce:::PL(Data.Nascar)
    R <- PlackettLuce:::denseRanking(Data.Nascar)
    mod1 <- PlackettLuce(R)
    alpha <- log(gamma)
    alpha <- alpha - alpha[1]
    test_that("estimates match Hunter's MM, gnm [artificial partial rankings with no ties]", {
        expect_equal(unname(coef(mod1))[-1], unname(coef(mod2))[-1], tolerance = coef_tol)
        expect_equal(unname(c(coef(mod1))), alpha, tolerance = coef_tol, check.attributes = FALSE)
    })
    test_that("log-likelihood matches Hunter's MM, gnm [artificial partial rankings with no ties]", {
        expect_equal(logLik(mod1), logLik_poisson.gnm(mod2), check.attributes = FALSE, tolerance = loglik_tol)
    })
}
