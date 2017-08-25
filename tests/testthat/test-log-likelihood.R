context("implementation of the log-likelihood")

## Get the legacy implementation
source_files <- c("PlackettLuce0.R", "coef0.R",
                  "fitted0.R", "logLik0.R",
                  "print.PlackettLuce0.R", "summary0.R",
                  "vcov0.R")

for (file0 in source_files) {
    source(system.file("PlackettLuce0", file0, package = "PlackettLuce"))
}


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
    test_that("Current and legacy implementations return the same log-likelihood", {
        # coefficients
        expect_equal(unname(unclass(coef(model0))), unname(unclass(coef(model1))), tolerance = 1e-06)
        # log-likelihood
        expect_equal(logLik(model0), logLik(model1), tolerance = 1e-12)
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
    mod3 <- BTm(rep(1, 6), factor(M[,1]), factor(M[,2]))
    cdat <- btdata(as.data.frame(cbind(M, 1)))
    mod4 <- btfit(cdat, 1, epsilon = 1e-7)
    pp <- mod4$pi[[1]]
    pp <- (pp/sum(pp))[cdat$components$`1`]
    pp <- log(pp) - log(pp)[1]
    test_that("PlackettLuce returns the same coefficients with gnm, BTm, btfit for an artificial data set", {
        ## coefficients
        expect_equal(unname(coef(mod1)[-1]), unname(coef(mod2)[-1]), tolerance = 1e-06)
        expect_equal(unname(coef(mod2)[-1]), unname(coef(mod3)), tolerance = 1e-06)
        expect_equal(unname(coef(mod3)), unname(pp[-1]), tolerance = 1e-06)
        ## log-likelihood
        expect_equal(logLik(mod1), logLik(mod3), check.attributes = FALSE, tolerance = 1e-12)
        expect_equal(logLik(mod3), logLik_poisson.gnm(mod2), check.attributes = FALSE, tolerance = 1e-12)
    })
}
