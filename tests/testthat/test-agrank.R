context("implementation [MAP in PlackettLuce]")

# Get reference agRank implementation
source(system.file(file.path("Reference_Implementations", "sgdPL.R"),
                   package = "PlackettLuce"))

# Triple comparisons, no ties
R <- matrix(c(1, 2, 3, 0,
              0, 1, 2, 3,
              2, 1, 0, 3,
              1, 2, 3, 0,
              2, 0, 1, 3,
              1, 0, 3, 2), nrow = 6, byrow = TRUE)
colnames(R) <- c("apple", "banana", "orange", "pear")

test_that("logLik matches agRank [fake triple comparisons]", {
    # Fit model using sgdPL from AgRank
    ## non-informative prior on log-worths (large variance)
    m <- ncol(R)
    n <- nrow(R)
    mu <- rep(1, m)
    sigma <- diag(1000000, m)
    ## fit model without adherence parameter (adherence fixed to 1)
    res <- sgdPL(R, mu, sigma, rate = 0.1, adherence = FALSE, maxiter = 8000,
                tol = 1e-12, start = rep(1, m), decay = 1.001)
    ###oscillating behaviour
    ###plot(res$value, type = "l")
    # Fit Plackett-Luce with standard maximum likelihood
    mod_PL <- PlackettLuce(rankings = R, npseudo = 0)
    ## lowish tolerance as stochastic gradient descent only approximate
    expect_equal(logLik(mod_PL)[1], -tail(res$value, 1),
                 tolerance = 1e-6)
})

# Triple comparisons, no ties
prior <- list(mu = c(0, -0.05, -2, -3),
              K = matrix(c(1, 0.5, 0.1, -0.5,
                           0.5, 1.1, 0.1, 0.1,
                           0.1, 0.1, 1.2, 0.2,
                           -0.5, 0.1, 0.2, 1.3), 4, 4, byrow = TRUE))

test_that("logLik matches agRank with prior [fake triple comparisons]", {
    # Fit model using sgdPL from AgRank
    res <- sgdPL(R, prior$mu, prior$K, rate = 0.1, adherence = FALSE, maxiter = 8000,
                 tol = 1e-12, start = prior$mu, decay = 1.001)
    ###oscillating behaviour
    ###plot(res$value, type = "l")
    # Fit Plackett-Luce with standard maximum likelihood (BFGS)
    mod_PL <- PlackettLuce(rankings = R, npseudo = 0, prior = prior,
                           start = exp(prior$mu))
    mod_PL2 <- PlackettLuce(rankings = R, npseudo = 0, prior = prior,
                           start = exp(prior$mu), method = "L-BFGS")
    ## lowish tolerance as stochastic gradient descent only approximate
    expect_equal(logLik(mod_PL)[1], -tail(res$value, 1),
                 tolerance = 1e-5)
    expect_equal(logLik(mod_PL2)[1], -tail(res$value, 1),
                 tolerance = 1e-5)
})
