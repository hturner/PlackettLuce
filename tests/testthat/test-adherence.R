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
    res <- sgdPL(R[seq(p),], mu, sigma, rate = 0.1, adherence = FALSE,
                 maxiter = 0, tol = 1e-12, start = c(mu, adherence[seq(p)]),
                 decay = 1.001)
    # Fit model using PlackettLuce
    ## with normal prior to allow low p (BFGS by default)
    mod_PL1 <- PlackettLuce(rankings = R[seq(p),], npseudo = 0, maxit = 0,
                            adherence = adherence[seq(p)], start = alpha,
                            normal = list(mu = mu, Sigma = sigma))
    ## N.B. - can't test L-BFGS with zero iterations
    ##      - iterative scaling not implemented with adherence
    expect_equal(logLik(mod_PL1)[1], -res$value[1])
    # Same, now iterating to convergence
    res <- sgdPL(R[seq(p),], mu, sigma, rate = 0.1, adherence = FALSE,
                 maxiter = 8000,
                 tol = 1e-12, start = c(mu, adherence[seq(p)]), decay = 1.001)
    mod_PL2 <- PlackettLuce(rankings = R[seq(p),], npseudo = 0,
                            adherence = adherence[seq(p)], start = alpha,
                            normal = list(mu = mu, Sigma = sigma))
    mod_PL3 <- PlackettLuce(rankings = R[seq(p),], npseudo = 0,
                            method = "L-BFGS", adherence = adherence[seq(p)],
                            start = alpha,
                            normal = list(mu = mu, Sigma = sigma))
    expect_equal(mod_PL2$logposterior, -tail(res$value, 1),
                 tolerance = 1e-5)
    expect_equal(mod_PL3$logposterior, -tail(res$value, 1),
                 tolerance = 1e-5)
})

# with grouped rankings

test_that('estimated adherence works for grouped_rankings [fake triples]', {
    # each ranking is a separate group
    G <- grouped_rankings(R, seq(nrow(R)))
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
                 check.attributes = FALSE)
    expect_equal(coef(mod2), coef(mod4), tolerance = 1e-4,
                 check.attributes = FALSE)
    expect_equal(logLik(mod1), logLik(mod3), tolerance = 1e-8,
               check.attributes = FALSE)
    expect_equal(logLik(mod2), logLik(mod4), tolerance = 1e-8,
                 check.attributes = FALSE)
})

test_that('estimated adherence works w/ npseudo != 0 [fake triples]', {
    mod1 <- PlackettLuce(rankings = R,
                         gamma = list(shape = 100, rate = 100))
    expect_known_value(mod1,
                       file = test_path("outputs/pl_adherence_pseudo.rds"))
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
# Add middle ranked variety
beans <- within(beans, {
    best <- match(best, c("A", "B", "C"))
    worst <- match(worst, c("A", "B", "C"))
    middle <- 6 - best - worst
})

# Convert the variety IDs to the variety names
varieties <- as.matrix(beans[c("variety_a", "variety_b", "variety_c")])
n <- nrow(beans)
beans <- within(beans, {
    best <- varieties[cbind(seq_len(n), best)]
    worst <- varieties[cbind(seq_len(n), worst)]
    middle <- varieties[cbind(seq_len(n), middle)]
})

# Create a rankings object from the rankings of order three
## each ranking is a sub-ranking of three varieties from the full set
lab <- c("Local", sort(unique(as.vector(varieties))))
R <- as.rankings(beans[c("best", "middle", "worst")],
                 input = "ordering", labels = lab)

# Convert worse/better columns to ordered pairs
head(beans[c("var_a", "var_b", "var_c")], 2)
paired <- list()
for (id in c("a", "b", "c")){
    ordering <- matrix("Local", nrow = n, ncol = 2)
    worse <- beans[[paste0("var_", id)]] == "Worse"
    ## put trial variety in column 1 when it is not worse than local
    ordering[!worse, 1] <- beans[[paste0("variety_", id)]][!worse]
    ## put trial variety in column 2 when it is worse than local
    ordering[worse, 2] <- beans[[paste0("variety_", id)]][worse]
    paired[[id]] <- ordering
}

# Convert orderings to sub-rankings of full set and combine all rankings
paired <- lapply(paired, as.rankings, input = "ordering", labels = lab)
R <- rbind(R, paired[["a"]], paired[["b"]], paired[["c"]])
G <- grouped_rankings(R, rep(seq_len(nrow(beans)), 4))

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

test_that('estimated adherence works for grouped_rankings [partial + ties]', {
    w <- c(3, 2, 5, 4, 3, 7)
    # replicates with exactly same adherence (does not converge)
    mod1 <-  suppressWarnings(PlackettLuce(R, npseudo = 0, method = "BFGS",
                                           weights = w,
                                           gamma = list(shape = 10, rate = 10)))
    # replicates grouped together by ranker
    G <- grouped_rankings(R[rep(seq(6), w),], index = rep(seq(6), w))
    mod2 <- suppressWarnings(PlackettLuce(rankings = G, npseudo = 0,
                                          method = "BFGS",
                                          gamma = list(shape = 10, rate = 10)))
    # remove bits we expect to be different
    # iter can be different on some platform due to small difference in rowsums
    nm <- setdiff(names(mod1),
                  c("call", "rankings", "ranker", "weights", "iter"))
    expect_equal(mod1[nm], mod2[nm])
})


