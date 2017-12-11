context("implementation [simulate.PlackettLuce]")


R <- matrix(c(1, 2, 0, 0,
              4, 1, 2, 3,
              2, 1, 1, 1,
              1, 2, 3, 0,
              2, 1, 1, 0,
              1, 0, 3, 2), nrow = 6, byrow = TRUE)
colnames(R) <- c("apple", "banana", "orange", "pear")
mod <- PlackettLuce(R)
simulate(mod, 5)
s1 <- simulate(mod, 5, seed = 112)
s2 <- simulate(mod, 4, seed = 112)

test_that("the seed argument is respected in [simulate.PlackettLuce]", {
    expect_identical(s1[1:3], s2[1:3])
})


## A small simulation study
## R <- PlackettLuce:::generate_rankings(maxi = 5, n_rankings = 200, tie = 0, seed = 123)
## mod1 <- PlackettLuce(R)
## samples <- simulate(mod1, 100, set.seed = 123)
## fits <- lapply(samples, PlackettLuce, npseudo = 1/2)
## coefs <- sapply(fits, function(fit) {
##     cc <- coef(fit)
##     if (length(cc) < 9)
##         c(cc, rep(0, 9 - length(cc)))
##     else
##         cc
## })
## par(mfrow = c(3, 3))
## for (j in 1:9) { hist(coefs[j,], main = paste(j)); abline(v = coef(mod1)[j]) }
