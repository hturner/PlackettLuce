context("implementation [weight argument in PlackettLuce]")

coef_tol <- 1e-06
loglik_tol <- 1e-07

## Extractor for the log-likelihood of poisson gnm's adjusting for the -mu part
## which is fixed due to the parameter space restriction to match row totals
logLik_poisson.gnm <- function(x) {
    ll <- logLik(x) + sum(x$weights)
    attr(ll, "df") <- attr(ll, "df") - nlevels(x$eliminate)
    ll
}

## pudding data (Example 2, Davidson 1970) is in tests/pudding.rda
load("../pudding.rda")
## expand to one ranking per row
R <- with(pudding, {
    R <- list()
    n <- max(i, j)
    for (r in seq_len(nrow(pudding))){
        R[[r]] <- matrix(0, nrow = r_ij[r], ncol = n)
        a <- rep(c(1, 2, 1), c(w_ij[r], w_ji[r], t_ij[r]))
        b <- rep(c(2, 1, 1), c(w_ij[r], w_ji[r], t_ij[r]))
        R[[r]][, c(i[r], j[r])] <- c(a, b)
    }
    do.call("rbind", R)
})
## aggregate to unique rankings
rankings <- split(R, row(R))
uniq <- unique(rankings)
g <- match(rankings, uniq)
R2 <- R[!duplicated(g),]
w <- tabulate(g)
if (require(gnm)){
    ## fit PlackettLuce model
    ## - one ranking per row
    mod0 <- PlackettLuce(R, npseudo = 0)
    ## - aggregated to unique rankings
    mod1 <- PlackettLuce(R2, weights = w, npseudo = 0)
    ## fit log-linear model to aggregated rankings
    ## (do not aggregate different choices from same set of alternatives)
    dat <- PlackettLuce:::poisson_rankings(R2, weights = w, aggregate = FALSE,
                                           as.data.frame = TRUE)
    mod2 <- gnm(y ~ -1 + X, family = poisson, eliminate = z, data = dat,
                constrain = 1, weights = w)
    test_that("fit is the same for aggregated data [pudding]",
              {
                  expect_equal(coef(mod0), coef(mod1), tolerance = coef_tol)
                  expect_equal(logLik(mod0), logLik(mod1),
                               tolerance = loglik_tol)
                  expect_equal(vcov(mod0), vcov(mod1))
              })
    test_that("fit matches gnm for aggregated data [pudding]",
              {
                  expect_equal(as.vector(coef(mod1)),
                               as.vector(parameters(mod2)),
                               tolerance = coef_tol)
                  expect_equal(logLik(mod1), logLik_poisson.gnm(mod2),
                               check.attributes = FALSE, tolerance = loglik_tol)
                  expect_equal(as.vector(vcov(mod1)),
                               as.vector(vcov(mod2)))

              })
}

## netflix data (as in vignett) is in tests/netflix.rda
load("../netflix.rda")
r <- rep(seq_len(nrow(netflix)), netflix$n)
R <- as.rankings(netflix[r,-1], "ordering")
R2 <- as.rankings(netflix[,-1], "ordering")
w <- netflix$n
## fit PlackettLuce model
## - one ranking per row
mod0 <- PlackettLuce(R, npseudo = 0)
## - aggregated to unique rankings
mod1 <- PlackettLuce(R2, weights = w, npseudo = 0)
test_that("fit is the same for aggregated data [netflix]",
          {
              expect_equal(coef(mod0), coef(mod1), tolerance = coef_tol)
              expect_equal(logLik(mod0), logLik(mod1),
                           tolerance = loglik_tol)
              expect_equal(vcov(mod0), vcov(mod1))
          })

if (require(psychotree)){
    ## Germany's Next Topmodel 2007 data
    data("Topmodel2007", package = "psychotree")
    ## give some judges higher weight
    w <- rep.int(1, nrow(Topmodel2007))
    w[1:5] <- 2
    ## btmodel
    mod1 <- btmodel(Topmodel2007$preference)
    mod2 <- btmodel(Topmodel2007$preference, weights = w)
    ## PlackettLuce (should be grouped rankings as weight is per judge)
    R <- as.grouped_rankings(Topmodel2007$preference)
    mod3 <- PlackettLuce(R, npseudo = 0)
    mod4 <- PlackettLuce(R, weights = w, npseudo = 0)
    ## bttree can't take weights, so compare to tree with no split (alpha = 0)
    tm_tree <- pltree(R ~ ., data = Topmodel2007[, -1], minsize = 5,
                      weights = w, alpha = 0, npseudo = 0)
    test_that("fit correct for grouped rankings object [Topmodel2007]",
              {
                  expect_equal(as.vector(itempar(mod1, log = TRUE,
                                                 ref = "Barbara")),
                               as.vector(coef(mod3)), tolerance = coef_tol)
                  expect_equal(logLik(mod1), logLik(mod3),
                               tolerance = loglik_tol)
              })
    test_that("weights work for plfit [Topmodel2007]",
              {
                  expect_equal(as.vector(itempar(mod2, log = TRUE,
                                                 ref = "Barbara")),
                               as.vector(coef(mod4)), tolerance = coef_tol)
                  expect_equal(logLik(mod2), logLik(mod4),
                               tolerance = loglik_tol)
              })
    test_that("weights work for pltree [Topmodel2007]",
              {
                  expect_equal(as.vector(coef(tm_tree)), as.vector(coef(mod4)),
                               check.attributes = FALSE, tolerance = coef_tol)
                  expect_equal(logLik(tm_tree), logLik(mod4),
                               check.attributes = FALSE,
                               tolerance = loglik_tol)
              })
}
