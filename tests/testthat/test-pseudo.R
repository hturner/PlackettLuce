context("implementation [pseudo-data]")

## Get the legacy implementation
source_files <- c("PlackettLuce0.R", "coef0.R",
                  "fitted0.R", "logLik0.R",
                  "print.PlackettLuce0.R", "summary0.R",
                  "vcov0.R")

for (file0 in source_files) {
    source(system.file("PlackettLuce0", file0, package = "PlackettLuce"))
}


coef_tol <- 1e-06
loglik_tol <- 1e-07

## Extractor for the log-likelihood of poisson gnm's adjusting for the -mu part
## which is fixed due to the parameter space restriction to match row totals
logLik_poisson.gnm <- function(x) {
    n <- nlevels(x$eliminate)
    ll <- logLik(x) + n
    attr(ll, "df") <- attr(ll, "df") - n
    ll
}

## Weakly connected network
X <- matrix(c(1, 2, 0, 0,
              2, 1, 3, 0,
              0, 0, 1, 2,
              0, 0, 2, 1), ncol = 4, byrow = TRUE)

if (require(Matrix)){
    model0 <- PlackettLuce0(rankings = X, network = "pseudodata")
    model1 <- PlackettLuce(rankings = X, npseudo = 1)
    test_that("coef match legacy code [weakly connected network]", {
        # coefficients
        expect_equal(unname(unclass(coef(model0))),
                     unname(unclass(coef(model1))), tolerance = coef_tol)
    })
    test_that("logLik matches legacy code [weakly connected network]", {
        # log-likelihood
        expect_equal(logLik(model0), logLik(model1), tolerance = loglik_tol)
    })
}

G <- grouped_rankings(X, c(1, 1, 2, 2))
model2 <- PlackettLuce(rankings = G, npseudo = 1)
test_that("pseudodata works with grouped_rankings [weakly connected network]", {
    expect_equal(coef(model1), coef(model2), tolerance = coef_tol)
    expect_equal(logLik(model1), logLik(model2), tolerance = loglik_tol)
})

model3 <- PlackettLuce(rankings = G, npseudo = 0.5)
if (require(gnm) & require(sandwich)){
    ## add pseudodata
    N <- ncol(X)
    X2 <- cbind(X, "NULL" = 0)
    pseudo <- matrix(0, nrow = 2*N, ncol = N + 1)
    pseudo[, N + 1] <- 1:2
    pseudo[cbind(seq_len(nrow(pseudo)),
                 rep(seq_len(N), each = 2))] <- 2:1
    X2 <- rbind(pseudo, X2)
    w <- c(rep.int(0.5, nrow(pseudo)), rep.int(1, nrow(X2)))
    dat <- PlackettLuce:::poisson_rankings(X2, weights = w, aggregate = FALSE,
                                           as.data.frame = TRUE)
    ## fit log-linear model with added pseudodata
    model4 <- gnm(y ~ -1 + X, family = poisson, eliminate = z, data = dat,
                  constrain = 1, weights = w)
    test_that("non-integer pseudo data works [weakly connected network]",
              {
                  # coef
                  expect_equal(as.vector(coef(model3)),
                               as.vector(parameters(model4)[-(N + 1)]),
                               tolerance = coef_tol)
                  # likelihood contributions
                  keep <- !dat$z %in% seq_len(2*N)
                  expect_equal(unname(colSums(estfun(model3))),
                               unname(colSums(estfun(model4)[keep, - N])),
                               tolerance = loglik_tol)
                  # rank
                  expect_equal(model3$rank,
                               model4$rank - nlevels(dat$z) - 1)
                  # df.residual
                  # rank
                  expect_equal(model3$df.residual,
                               sum(keep) - 2*N)

              })
}
## TODO: subset as rankings etc to not bother checking
