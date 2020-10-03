context("implementation [estfun in PlackettLuce]")

## The artificial example in ?PlackettLuce
R <- matrix(c(1, 2, 0, 0,
              4, 1, 2, 3,
              2, 1, 1, 1,
              1, 2, 3, 0,
              2, 1, 1, 0,
              1, 0, 3, 2), nrow = 6, byrow = TRUE)
colnames(R) <- c("apple", "banana", "orange", "pear")

test_that("estfun matches agRank, fixed adherence [fake triple comparisons]", {
    m <- ncol(R)
    n <- nrow(R)
    adherence <- seq(0.75, 1.25, length.out = n)
    alpha <- seq(0.25, 0.75, length.out = m)
    delta <- seq(0.5, 1.5, length.out = 2)
    # Fit model using PlackettLuce
    mod_PL <- PlackettLuce(rankings = R, npseudo = 0,
                           adherence = adherence, start = c(alpha, delta))
    # sum over all sets st object i is in selected set adherence/size
    A <- c(sum(adherence[c(1, 4, 6)]),
           sum(adherence[c(2, 3, 4, 5)]*c(1, 1/3, 1, 1/2)),
           sum(adherence[c(2, 3, 5)]*c(1, 1/3, 1/2)),
           sum(adherence[c(2, 3, 6)]*c(1, 1/3, 1)))
    # number of sets with cardinality d
    B <- c(8, 1, 1)
    # expectations
    R2 <- matrix(c(2, 1, 3, 4,
                   1, 4, 3, 2,
                   1, 2, 3, 4,
                   3, 2, 1, 4,
                   1, 2, 3, 4,
                   3, 4, 1, 2), nrow = 6, byrow = TRUE)
    G <- list(NULL, c(1, 2, 4, 6), c(2, 4, 5, 6), c(2, 3))
    W <- list(NULL, c(1, 1, 1, 1), c(1, 1, 1, 1), c(1, 1))
    par <- mod_PL$coefficients
    fit <- expectation(c("alpha", "delta"), par[1:m], c(1.0, par[-(1:m)]),
                       adherence, m, 1:3, 2:4, R2, G, W)
    # score wrt log-parameters
    score <- score_common(par = par, N = m,
                          mu = NULL, Kinv = NULL, A = A, B = B, fit = fit)*par
    # score = c(A - fit$expA, B[-1] - fit$expB*par[-(1:m)])
    expect_equal(colSums(estfun(mod_PL, ref = NULL)), score)
    # model with weights
    w <- c(2, 7, 3, 4, 10, 5)
    mod_PL <- PlackettLuce(rankings = R, npseudo = 0, weights = w,
                           adherence = adherence, start = c(alpha, delta))
    A <- c(sum(w[c(1, 4, 6)] * adherence[c(1, 4, 6)]),
           sum(w[c(2, 3, 4, 5)] * adherence[c(2, 3, 4, 5)]*c(1, 1/3, 1, 1/2)),
           sum(w[c(2, 3, 5)] * adherence[c(2, 3, 5)]*c(1, 1/3, 1/2)),
           sum(w[c(2, 3, 6)] * adherence[c(2, 3, 6)]*c(1, 1/3, 1)))
    B <- c(sum(w[c(1, 2, 2, 2, 4, 4, 6, 6)]), w[5], w[3])
    W <- list(NULL, w[G[[2]]], w[G[[3]]], w[G[[4]]])
    par <- mod_PL$coefficients
    fit <- expectation(c("alpha", "delta"), par[1:m], c(1.0, par[-(1:m)]),
                       adherence, m, 1:3, 2:4, R2, G, W)
    score <- score_common(par = par, N = m,
                          mu = NULL, Kinv = NULL, A = A, B = B, fit = fit)*par

    expect_equal(colSums(estfun(mod_PL, ref = NULL)), score)
})
