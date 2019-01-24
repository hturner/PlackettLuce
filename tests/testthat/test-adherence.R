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
    adherence <- seq(0.75, 1.25, length.out = n)
    alpha <- seq(0.25, 0.75, length.out = m)
    # Non-informative prior on log-worths (large variance)
    mu <- log(alpha)
    sigma <- diag(1000000, m)
    p <- 6 # switch for interactive testing (max = 8)
    # Fit model using sgdPL from AgRank with fixed adherence
    ## - no iterations, just checking log-likelihood calculations
    res <- sgdPL(R[1:p,], mu, sigma, rate = 0.1, adherence = FALSE, maxiter = 0,
                 tol = 1e-12, start = c(mu, adherence[1:p]), decay = 1.001)
    # Fit model using PlackettLuce
    ## with normal prior to allow low p (BFGS by default)
    mod_PL1 <- PlackettLuce(rankings = R[1:p,], npseudo = 0, maxit = 0,
                            adherence = adherence[1:p], start = alpha,
                            normal = list(mu = mu, Sigma = sigma))
    ## N.B. - can't test L-BFGS with zero iterations
    ##      - iterative scaling not implemented with adherence
    expect_equal(logLik(mod_PL1)[1], -res$value[1])
    ## cannot iterate sgdPL with fixed adherence
    mod_PL2 <- PlackettLuce(rankings = R[1:p,], npseudo = 0,
                            adherence = adherence[1:p], start = alpha,
                            normal = list(mu = mu, Sigma = sigma))
    mod_PL3 <- PlackettLuce(rankings = R[1:p,], npseudo = 0, method = "L-BFGS",
                            adherence = adherence[1:p], start = alpha,
                            normal = list(mu = mu, Sigma = sigma))
    expect_equal(logLik(mod_PL1)[1], -res$value[1])
})
