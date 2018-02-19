context("implementation [pseudo-data]")

## Get the legacy implementation
source_files <- dir(system.file("PlackettLuce0", package = "PlackettLuce"),
                    full.names = TRUE)

## N.B. fitted0 requires tibble but unused in tests
for (file0 in source_files) source(file0)


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
        expect_equal(as.vector(coef(model0)),
                     as.vector(coef(model1)), tolerance = coef_tol)
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

model5 <- PlackettLuce(rankings = G, npseudo = 0.5, method = "BFGS",
                       control = list(reltol = 1e-10))
test_that("pseudo data works with BFGS [weakly connected network]",
          {
              # coef
              expect_equivalent(coef(model3), coef(model5),
                                tolerance = coef_tol)
              # log-likelihood
              expect_equal(logLik(model3), logLik(model5),
                           tolerance = loglik_tol)
          })

model6 <- PlackettLuce(rankings = G, npseudo = 0.5, method = "BFGS",
                       control = list(reltol = 1e-10))
test_that("pseudo data works with L-BFGS [weakly connected network]",
          {
              # coef
              expect_equivalent(coef(model3), coef(model6),
                                tolerance = coef_tol)
              # log-likelihood
              expect_equal(logLik(model3), logLik(model6),
                           tolerance = loglik_tol)
          })



## simple BT model
R <- matrix(c(1, 2, 0, 0,
              2, 0, 1, 0,
              1, 0, 0, 2,
              2, 1, 0, 0,
              0, 1, 2, 0,
              0, 0, 2, 0), byrow = TRUE, ncol = 4,
            dimnames = list(NULL, letters[1:4]))

test_that("disconnected network causes error [one always loses]",
          {
              # error for discinnected network
              expect_error(mod <- PlackettLuce(R, npseudo = 0))
              # no error for connected subset
              expect_error(mod <- PlackettLuce(R[, 1:3], npseudo = 0), NA)
           })

test_that("disconnected network works with pseudodata [one always loses]",
          {
              expect_error(mod <- PlackettLuce(R), NA)
          })

# weakly connected clusters
X <- matrix(c(1, 2, 0, 0,
              2, 1, 3, 0,
              0, 0, 1, 2,
              0, 0, 2, 1), ncol = 4, byrow = TRUE)
R <- as.rankings(X)

test_that("disconnected network causes error [weakly connected clusters]",
          {
              expect_error(mod <- PlackettLuce(R, npseudo = 0))
          })

# disconnected clusters
X <- matrix(c(1, 2, 0, 0,
              2, 1, 0, 0,
              0, 0, 1, 2,
              0, 0, 2, 1), ncol = 4, byrow = TRUE)
R <- as.rankings(X)

test_that("disconnected network causes error [disconnected clusters]",
          {
              expect_error(mod <- PlackettLuce(R, npseudo = 0))
          })

test_that("disconnected network works with pseudodata [disconnected clusters]",
          {
              expect_error(mod <- PlackettLuce(R), NA)
          })

# two weakly connected items:
# item 1 always loses; item 4 only wins against item 1
X <- matrix(c(4, 1, 2, 3,
              0, 2, 1, 3), nr = 2, byrow = TRUE)
R <- as.rankings(X)

test_that("disconnected network causes error [weakly connected items]",
          {
              expect_error(mod <- PlackettLuce(R, npseudo = 0))
          })

test_that("disconnected network works with pseudodata [weakly connected items]",
          {
              expect_error(mod <- PlackettLuce(R), NA)
          })

# item 1 always wins; item 4 always loses
X <- matrix(c(1, 2, 3, 4,
              1, 3, 2, 4), nr = 2, byrow = TRUE)
R <- as.rankings(X)

test_that("disconnected network causes error [1 wins; 4 loses]",
          {
              expect_error(mod <- PlackettLuce(R, npseudo = 0))
          })

test_that("disconnected network works with pseudodata [1 wins; 4 loses]",
          {
              expect_error(mod <- PlackettLuce(R), NA)
          })
