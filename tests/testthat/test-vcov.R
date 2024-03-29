# Get alternative implementation based on optimHess
source(system.file(file.path("Reference_Implementations", "vcov_hessian.R"),
                   package = "PlackettLuce"))
# Mult masked by loading psychotree (-> partykit -> mvtnorm)
# needs update in gnm to fix, quick hack here to ensure correct Mult used
detach(package:psychotree)
detach(package:partykit)
detach(package:mvtnorm)

## The artificial example in ?PlackettLuce
R <- matrix(c(1, 2, 0, 0,
              4, 1, 2, 3,
              2, 1, 1, 1,
              1, 2, 3, 0,
              2, 1, 1, 0,
              1, 0, 3, 2), nrow = 6, byrow = TRUE)
colnames(R) <- c("apple", "banana", "orange", "pear")
R <- as.rankings(R)

# standard Plackett-Luce model
noprior <- PlackettLuce(rankings = R, npseudo = 0)

if (require("gnm")) {
    test_that("vcov.PlackettLuce matches vcov.gnm [partial + ties]", {
        dat <- poisson_rankings(R, aggregate = FALSE)
        dat$X <- as.matrix(dat$X)
        n <- nrow(dat$X)
        cf <- log(noprior$coefficients)
        # use Mult to force use of gnmFit
        # (allows zero iterations - )
        noprior_gnm <- gnm(y ~ Mult(offset(rep(1, n)), X[, 1:4]) + X[, 5:6],
                           eliminate = as.factor(z), data = dat,
                           family = poisson, constrain = 1,
                           start = c(cf[1:4] - cf[1], cf[5:6]),
                           iterMax = 0, verbose = FALSE)
        # should be the same to low tolerance for same coefficients
        expect_equal(vcov(noprior), unclass(vcov(noprior_gnm)),
                     ignore_attr = TRUE, tolerance = 1e-11)
        # fixed adherence
        a <- seq(0.75, 1.25, length.out = nrow(R))
        noprior <- PlackettLuce(rankings = R, npseudo = 0, method = "BFGS",
                                adherence = a)
        dat$a <- noprior$adherence[rep(1:6, c(3, 24, 14, 10, 7, 10))]
        cf <- log(noprior$coefficients)
        ## suppress warning - does not meet gnm convergence criteria
        noprior_gnm <- suppressWarnings(
            gnm(y ~ Mult(offset(a), X[, 1:4]) + X[, 5:6],
                eliminate = as.factor(z), data = dat,
                family = poisson, constrain = 1,
                start = c(cf[1:4] - cf[1], cf[5:6]),
                iterMax = 0, verbose = FALSE))
        # should be the same to low tolerance for same coefficients
        expect_equal(vcov(noprior), unclass(vcov(noprior_gnm)),
                     ignore_attr = TRUE, tolerance = 1e-11)
    })
}

test_that("qvcalc.PlackettLuce works with different ref [partial + ties]", {
    expect_equal(qvcalc(noprior)$qvframe[c("quasiSE", "quasiVar")],
                 qvcalc(noprior, ref = NULL)$qvframe[c("quasiSE", "quasiVar")])
})

test_that("vcov.PlackettLuce matches vcov_hessian [normal prior]", {
    # non-informative prior
    m <- ncol(R)
    n <- nrow(R)
    mu <- rep(1, m)
    sigma <- diag(1000000, m)
    normal_prior <- PlackettLuce(rankings = R, npseudo = 0, method = "BFGS",
                                 normal = list(mu = mu, Sigma = sigma))
    ## suppress warning due to non-convergence
    noprior <- suppressWarnings(PlackettLuce(rankings = R, npseudo = 0,
                                             start = coef(normal_prior),
                                             maxit = 0))
    # should be the same to medium tolerance since some effect of prior on vcov
    expect_equal(vcov(normal_prior), vcov(noprior),
                 ignore_attr = TRUE, tolerance = 1e-6)
    # informative prior
    prior <- list(mu = c(-0.05, -0.05, -2, -3),
                  Sigma = matrix(c(1, 0.5, 0.1, -0.5,
                                   0.5, 1.1, 0.1, 0.1,
                                   0.1, 0.1, 1.2, 0.2,
                                   -0.5, 0.1, 0.2, 1.3), 4, 4, byrow = TRUE))
    normal_prior <- PlackettLuce(rankings = R, npseudo = 0, normal = prior)
    # should be the same to medium tolerance since expectation of hessian is
    # equal to hessian for GLM, but optimHess gives numerical approximation
    expect_equal(vcov(normal_prior), vcov_hessian(normal_prior),
                 ignore_attr = TRUE, tolerance = 1e-6)
    # more data
    normal_prior <- PlackettLuce(rankings = R, npseudo = 0, normal = prior,
                                 weights = rep(50, nrow(R)))
    # should be equal to lower tolerance with more data
    expect_equal(vcov(normal_prior), vcov_hessian(normal_prior),
                 ignore_attr = TRUE, tolerance = 1e-7)
})

test_that("vcov.PlackettLuce approximated by vcov_hessian [gamma prior]", {
    gamma_prior <- PlackettLuce(rankings = R, npseudo = 0, method = "BFGS",
                                gamma = list(shape = 100, rate = 100))
    # expectation of hessian is not equal to hessian for GNM, so vcov_hessian
    # only gives an approximation - not that close for small sample
    expect_equal(vcov(gamma_prior), vcov_hessian(gamma_prior),
                 ignore_attr = TRUE, tolerance = 0.1)
    # should be equal to low tolerance if base on observed Fisher Info though
    expect_equal(vcov(gamma_prior, type = "observed"),
                 vcov_hessian(gamma_prior),
                 ignore_attr = TRUE, tolerance = 1e-6)
    # more data - N.B. this assumes each ranker gives exactly same ranking,
    # not likely in practice just checking statistical property here
    # (vcov_hessian slow with large number of adherence par, not practical to
    # check with larger number of rankings from different rankers)
    gamma_prior <- PlackettLuce(rankings = R, npseudo = 0, method = "BFGS",
                                weights = rep(50, nrow(R)),
                                gamma = list(shape = 100, rate = 100))
    # closer with moderate number of samples
    expect_equal(vcov(gamma_prior), vcov_hessian(gamma_prior),
                 ignore_attr = TRUE, tolerance = 1e-2)
    expect_equal(vcov(gamma_prior, type = "observed"),
                 vcov_hessian(gamma_prior),
                 ignore_attr = TRUE, tolerance = 1e-6)
})

test_that("vcov.PlackettLuce works, grouped rankings [normal + gamma prior]", {
    # informative prior
    prior <- list(mu = c(-0.05, -0.05, -2, -3),
                  Sigma = matrix(c(1, 0.5, 0.1, -0.5,
                                   0.5, 1.1, 0.1, 0.1,
                                   0.1, 0.1, 1.2, 0.2,
                                   -0.5, 0.1, 0.2, 1.3), 4, 4, byrow = TRUE))
    # use grouped rankings so no. of rankings != no.of adherence par
    G <- group(R, index = c(1, 2, 3, 3, 4, 4))
    both_priors <- PlackettLuce(rankings = G, npseudo = 0, method = "BFGS",
                                normal = prior,
                                gamma = list(shape = 100, rate = 100))
    # small sample so vcov based on expected info not that close to observed
    expect_equal(vcov(both_priors), vcov_hessian(both_priors),
                 ignore_attr = TRUE, tolerance = 1e-2)
    # but that based on observed info equals numerical hessian to medium tol
    expect_equal(vcov(both_priors, type = "observed"),
                 vcov_hessian(both_priors),
                 ignore_attr = TRUE, tolerance = 1e-6)
    # gamma prior only (different method for Info inversion)
    gamma_prior <- PlackettLuce(rankings = G, npseudo = 0, method = "BFGS",
                                gamma = list(shape = 100, rate = 100))
    # small sample so vcov based on expected info not that close to observed
    expect_equal(vcov(gamma_prior), vcov_hessian(gamma_prior),
                 ignore_attr = TRUE, tolerance = 1e-1)
    # but that based on observed info equals numerical hessian to medium tol
    expect_equal(vcov(gamma_prior, type = "observed"),
                 vcov_hessian(gamma_prior),
                 ignore_attr = TRUE, tolerance = 1e-6)
})

test_that("vcov.PlackettLuce works w/ different ref [pudding]", {
    # create orderings for each set of paired comparisons
    i_wins <- data.frame(Winner = pudding$i, Loser = pudding$j)
    j_wins <- data.frame(Winner = pudding$j, Loser = pudding$i)
    if (getRversion() < "3.6.0"){
        n <- nrow(pudding)
        ties <- data.frame(Winner = array(split(pudding[c("i", "j")], 1:n), n),
                           Loser = rep(NA, 15))
    } else {
        ties <- data.frame(Winner = asplit(pudding[c("i", "j")], 1),
                           Loser = rep(NA, 15))
    }
    # convert to rankings
    R <- as.rankings(rbind(i_wins, j_wins, ties),
                     input = "orderings")
    # define weights as frequencies of each ranking
    w <- unlist(pudding[c("w_ij", "w_ji", "t_ij")])
    # fit Plackett-Luce model: limit iterations to match Davidson 1970
    expect_warning(mod <- PlackettLuce(R, npseudo = 0, weights = w,
                                       maxit = 7),
                   "Iterations have not converged.")
    V <- vcov(mod)

    # different ref id
    D <- diag(nrow(V))
    D[1:6, 2] <- D[1:6, 2] - 1
    expect_equal(vcov(mod, ref = 2), D %*% V %*% t(D),
                 ignore_attr = TRUE)

    # mean of multiple items
    D <- diag(nrow(V))
    D[1:6, 2:3] <- D[1:6, 2:3] - 1/2
    expect_equal(vcov(mod, ref = 2:3), D %*% V %*% t(D),
                 ignore_attr = TRUE)

    # mean of all items
    D <- diag(nrow(V))
    D[1:6, 1:6] <- D[1:6, 1:6] - 1/6
    expect_equal(vcov(mod, ref = NULL), D %*% V %*% t(D),
                 ignore_attr = TRUE)
})
