context("implementation [simulate.PlackettLuce]")

## Get the legacy implementation
source_files <- dir(system.file("PlackettLuce0", package = "PlackettLuce"),
                    full.names = TRUE)

## N.B. fitted0 requires tibble but unused in tests
for (file0 in source_files) source(file0)

R <- matrix(c(1, 2, 0, 0,
              4, 1, 2, 3,
              2, 1, 1, 1,
              1, 2, 3, 0,
              2, 1, 1, 0,
              1, 0, 3, 2), nrow = 6, byrow = TRUE)
colnames(R) <- c("apple", "banana", "orange", "pear")
mod <- PlackettLuce(R)

s1 <- simulate(mod, 5, seed = 112)
s2 <- simulate(mod, 4, seed = 112)

test_that("the seed argument is respected in [simulate.PlackettLuce]", {
    expect_identical(s1[1:3], s2[1:3])
})


## A small simulation study
## repeat simulation for legacy implementation too, to make sure comparable on
## test machine (robust to changes in RNG)
if (require("Matrix")){
    R <- PlackettLuce:::generate_rankings(maxi = 5, n_rankings = 100, tie = 0,
                                          seed = 123)
    sim <- function(mod){
        samples <- simulate.PlackettLuce(mod, 100, seed = 123)
        fits <- lapply(samples, PlackettLuce, npseudo = 0.5)
        coefs <- vapply(fits, function(fit) {
            cc <- coef(fit)
            if (length(cc) < 9)
                c(cc, rep(0, 9 - length(cc)))
            else
                cc
        }, numeric(length(coef(mod))))
        unname(unclass(rowMeans(coefs) - coef(mod)))
    }

    result_biases <- sim(PlackettLuce(R, npseudo = 0))
    result_biases0 <- sim(PlackettLuce0(R))

    test_that("simulation results are consistent to first version", {
        expect_equivalent(result_biases, result_biases0, tolerance = 1e-06)
    })
}

## par(mfrow = c(3, 3))
## for (j in 1:9) { hist(coefs[j,], main = paste(j)); abline(v = coef(mod1)[j]) }

## ## ## No ties
## R <- PlackettLuce:::generate_rankings(maxi = 10, n_rankings = 100,
##                                       tie = 1000, seed = 123)
## mod1 <- PlackettLuce(R)

## ## Using Diaconis (1998). Chapter 9D
## samples <- simulate(mod1, 1000, seed = 123, multinomial = FALSE)
## fits <- mclapply(samples, PlackettLuce, npseudo = 0.5, mc.cores = 4)
## coefs <- sapply(fits, function(fit) {
##     cc <- coef(fit)
##     cc
## })
## result_biases <- unname(unclass(rowMeans(coefs) - coef(mod1)))

## ## Using multinomial sampling
## samples2 <- simulate(mod1, 1000, seed = 123, multinomial = TRUE)
## fits2 <- mclapply(samples2, PlackettLuce, npseudo = 0.5, mc.cores = 4)
## coefs2 <- sapply(fits2, function(fit) {
##     cc <- coef(fit)
##     cc
## })
## result_biases2 <- unname(unclass(rowMeans(coefs2) - coef(mod1)))



