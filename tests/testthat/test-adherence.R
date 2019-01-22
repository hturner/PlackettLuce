context("implementation [adherence in PlackettLuce]")

# Get reference agRank implementation
source(system.file(file.path("Reference_Implementations", "sgdPL.R"),
                   package = "PlackettLuce"))

# Paired/triple comparisons, no ties
R <- matrix(c(1, 2, 3, 0,
              0, 1, 2, 3,
              2, 1, 0, 3,
              1, 2, 3, 0,
              2, 0, 1, 3,
              1, 0, 3, 2,
              1, 2, 0, 0,
              0, 1, 2, 0), nrow = 8, byrow = TRUE)
colnames(R) <- c("apple", "banana", "orange", "pear")

test_that("logLik matches agRank w/ fixed adherence [fake triple comparisons]", {
    m <- ncol(R)
    n <- nrow(R)
    alpha <- seq(0.25, 0.75, length.out = m)
    adherence <- seq(0.75, 1.25, length.out = n)
    # No iterations, just checking log-likelihood calculations
    mod_PL1 <- PlackettLuce(rankings = R, npseudo = 0, maxit = 0,
                            adherence = adherence, start = alpha)
    mod_PL2 <- PlackettLuce(rankings = R, npseudo = 0, maxit = 0,
                            method = "BFGS",
                            adherence = adherence, start = alpha)
    # Fit model using sgdPL from AgRank
    ## non-informative prior on log-worths (large variance)
    mu <- log(alpha)
    sigma <- diag(1000000, m)
    ## fit model with adherence parameter
    res <- sgdPL(R, mu, sigma, rate = 0.1, adherence = FALSE, maxiter = 0,
                tol = 1e-12, start = c(mu, adherence), decay = 1.001)
    ## lowish tolerance as stochastic gradient descent only approximate
    expect_equal(logLik(mod_PL1)[1], -res$value[1],
                 tolerance = 1e-6)
    expect_equal(logLik(mod_PL2)[1], -res$value[1],
                 tolerance = 1e-6)
})
