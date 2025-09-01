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
R <- as.rankings(R)

test_that("logLik matches agRank, fixed adherence [fake triple comparisons]", {
    m <- ncol(R)
    n <- nrow(R)
    adherence <- seq(0.75, 1.25, length.out = n)
    alpha <- seq(0.25, 0.75, length.out = m)
    # Non-informative prior on log-worths (large variance)
    mu <- log(alpha)
    sigma <- diag(1000000, m)
    p <- 8 # switch for interactive testing (max = 8)
    # Fit model using sgdPL from AgRank with fixed adherence
    ## - no iterations, just checking log-likelihood calculations
    res <- sgdPL(as.matrix(R[seq(p),]), mu, sigma, rate = 0.1,
                 adherence = FALSE, maxiter = 0, tol = 1e-12,
                 start = c(mu, adherence[seq(p)]), decay = 1.001)
    # Fit model using PlackettLuce
    ## with normal prior to allow low p (BFGS by default)
    mod_PL1 <- PlackettLuce(rankings = R[seq(p),], npseudo = 0, maxit = 0,
                            adherence = adherence[seq(p)], start = alpha,
                            normal = list(mu = mu, Sigma = sigma))
    ## N.B. - can't test L-BFGS with zero iterations
    ##      - iterative scaling not implemented with adherence
    expect_equal(logLik(mod_PL1)[1], -res$value[1])
    # Same, now iterating to convergence
    res <- sgdPL(as.matrix(R[seq(p),]), mu, sigma, rate = 0.1,
                 adherence = FALSE, maxiter = 8000,
                 tol = 1e-12, start = c(mu, adherence[seq(p)]), decay = 1.001)
    mod_PL2 <- PlackettLuce(rankings = R[seq(p),], npseudo = 0,
                            adherence = adherence[seq(p)], start = alpha,
                            normal = list(mu = mu, Sigma = sigma))
    expect_equal(mod_PL2$logposterior, -tail(res$value, 1),
                 tolerance = 1e-5)
    if (require(lbfgs, quietly = TRUE)){
        mod_PL3 <- PlackettLuce(rankings = R[seq(p),], npseudo = 0,
                                method = "L-BFGS",
                                adherence = adherence[seq(p)],
                                start = alpha,
                                normal = list(mu = mu, Sigma = sigma))
        expect_equal(mod_PL3$logposterior, -tail(res$value, 1),
                     tolerance = 1e-5)
    }
})

# with grouped rankings

test_that('estimated adherence works for grouped_rankings [fake triples]', {
    # each ranking is a separate group
    G <- group(R, seq(nrow(R)))
    mod1 <- PlackettLuce(rankings = R, npseudo = 0,
                         gamma = list(shape = 10, rate = 10))
    mod2 <- PlackettLuce(G, npseudo = 0, gamma = list(shape = 10, rate = 10))
    # remove bits we expect to be different
    # iter can be different on some platform due to small difference in rowsums
    nm <- setdiff(names(mod1), c("call", "iter"))
    expect_equal(mod1[nm], mod2[nm])
    expect_equal(mod1$adherence[mod1$ranker], mod2$adherence[mod2$ranker])

    # results should be same when fix to returned adherence
    mod3 <- PlackettLuce(R, npseudo = 0, start = coef(mod1), method = "BFGS",
                         adherence = mod1$adherence)
    mod4 <- PlackettLuce(G, npseudo = 0, start = coef(mod1), method = "BFGS",
                         adherence = mod2$adherence)
    # BFGS always does another iteration to check converegence,
    # iterative scaling has different check so would also iterate a little more
    expect_equal(coef(mod1), coef(mod3), tolerance = 1e-4,
                 ignore_attr = TRUE)
    expect_equal(coef(mod2), coef(mod4), tolerance = 1e-4,
                 ignore_attr = TRUE)
    expect_equal(logLik(mod1), logLik(mod3), tolerance = 1e-8,
               ignore_attr = TRUE)
    expect_equal(logLik(mod2), logLik(mod4), tolerance = 1e-8,
                 ignore_attr = TRUE)
})

test_that('estimated adherence works w/ npseudo != 0 [fake triples]', {
    mod1 <- PlackettLuce(rankings = R,
                         gamma = list(shape = 100, rate = 100))
    expect_snapshot_value(mod1, style = "json2")
})

test_that('default prior for adherence works [fake triples]', {
    mod1 <- PlackettLuce(rankings = R,
                         gamma = list(shape = 10, rate = 10))
    mod2 <- PlackettLuce(rankings = R, gamma = TRUE)
    # remove bits we expect to be different
    nm <- setdiff(names(mod1), c("call"))
    expect_equal(mod1[nm], mod2[nm])
})

test_that('check on prior for adherence works [fake triples]', {
    expect_warning(mod1 <- PlackettLuce(rankings = R,
                                        gamma = list(shape = 100, rate = 10)))
})

data(beans, package = "PlackettLuce")

# Fill in the missing ranking
beans$middle <- complete(beans[c("best", "worst")],
                         items = c("A", "B", "C"))

# Use these names to decode the orderings of order 3
order3 <- decode(beans[c("best", "middle", "worst")],
                 items = beans[c("variety_a", "variety_b", "variety_c")],
                 code = c("A", "B", "C"))

# Convert these results to a vector and get the corresponding trial variety
outcome <- unlist(beans[c("var_a", "var_b", "var_c")])
trial_variety <- unlist(beans[c("variety_a", "variety_b", "variety_c")])

# Create a data frame of the implied orderings of order 2
order2 <- data.frame(Winner = ifelse(outcome == "Worse",
                                     "Local", trial_variety),
                     Loser = ifelse(outcome == "Worse",
                                    trial_variety, "Local"),
                     stringsAsFactors = FALSE, row.names = NULL)

# Finally combine the rankings of order 2 and order 3
R <- rbind(as.rankings(order3, input = "ordering"),
           as.rankings(order2, input = "ordering"))

# Group the rankings by the corresponding farm
G <- group(R, rep(seq_len(nrow(beans)), 4))

test_that('fixed adherence works for grouped_rankings [beans]', {
    # adherence = 1 is same as no adherence
    adherence <- rep(1L, nrow(beans))
    mod1 <- PlackettLuce(G)
    mod2 <- PlackettLuce(G, adherence = adherence)
    # remove bits we expect to be different
    # iter can change on same platforms as rankings not aggregated in mod2
    nm <- setdiff(names(mod1), c("call", "adherence", "iter"))
    expect_equal(mod1[nm], mod2[nm])
    # adherence != 1 for grouped same as ungrouped with replicated adherence
    ranker_adherence <- seq(0.75, 1.25, length.out = nrow(beans))
    ranking_adherence <- rep(ranker_adherence, 4)
    mod1 <- PlackettLuce(R, adherence = ranking_adherence)
    mod2 <- PlackettLuce(G, adherence = ranker_adherence)
    # remove bits we expect to be different
    # iter can be different on some platform due to small difference in rowsums
    nm <- setdiff(names(mod1), c("call", "adherence", "ranker", "iter"))
    expect_equal(mod1[nm], mod2[nm])
    expect_equal(mod1$adherence[mod1$ranker], mod2$adherence[mod2$ranker])
})


R <- matrix(c(1, 2, 0, 0,
              4, 1, 2, 3,
              2, 1, 1, 1,
              1, 2, 3, 0,
              2, 1, 1, 0,
              1, 0, 3, 2), nrow = 6, byrow = TRUE)
colnames(R) <- c("apple", "banana", "orange", "pear")
R <- as.rankings(R)

test_that('estimated adherence works for grouped_rankings [partial + ties]', {
    w <- c(3, 2, 5, 4, 3, 7)
    # replicates with exactly same adherence (does not converge)
    mod1 <-  suppressWarnings(PlackettLuce(R, npseudo = 0, method = "BFGS",
                                           weights = w,
                                           gamma = list(shape = 10, rate = 10)))
    # replicates grouped together by ranker
    G <- group(R[rep(seq(6), w),], index = rep(seq(6), w))
    mod2 <- suppressWarnings(PlackettLuce(rankings = G, npseudo = 0,
                                          method = "BFGS",
                                          gamma = list(shape = 10, rate = 10)))
    # remove bits we expect to be different
    # iter can be different on some platform due to small difference in rowsums
    nm <- setdiff(names(mod1),
                  c("call", "rankings", "ranker", "weights", "iter"))
    expect_equal(mod1[nm], mod2[nm])
})


