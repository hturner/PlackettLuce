context("implementation [ties]")

coef_tol <- 1e-06
loglik_tol <- 1e-12

## Extractor for the log-likelihood of poisson gnm's adjusting for the -mu part
## which is fixed due to the parameter space restriction to match row totals
logLik_poisson.gnm <- function(x) {
    n <- nlevels(x$eliminate)
    ll <- logLik(x) + n
    attr(ll, "df") <- attr(ll, "df") - n
    ll
}

## ties, with some orders of tie not observed
R <- matrix(c(1, 2, 0, 0,
              4, 1, 2, 3,
              2, 1, 1, 1,
              1, 2, 3, 0,
              2, 1, 3, 0,
              1, 0, 3, 2,
              1, 1, 1, 1), nrow = 7, byrow = TRUE)
colnames(R) <- c("apple", "banana", "orange", "pear")
R <- as.rankings(R)

R2 <- matrix(c(1, 2, 0, 0,
               4, 1, 2, 3,
               2, 1, 1, 0,
               1, 2, 3, 0,
               2, 1, 3, 0,
               1, 0, 3, 2,
               1, 1, 1, 1), nrow = 7, byrow = TRUE)
colnames(R2) <- c("apple", "banana", "orange", "pear")
R2 <- as.rankings(R2)

# compute components for R2 directly for low-level checks
## (sum over all sets st object i is in selected set)/size
A <- c(sum(1, 1, 1, 1, 1/4),
       sum(1, 1/2, 1, 1, 1/4),
       sum(1, 1/2, 1/4),
       sum(1, 1, 1/4))
## number of sets with cardinality d = c(1, 2, 4)
B <- c(10, 1, 1)
## item id for wins
item_id <- c(1, 2, 3, 4, 2, 3, 1, 2, 2, 1, 1, 4, 1, 2, 3, 4)
## 1/size of winning sets
S <- c(1, 1, 1, 1, 1/2, 1/2, 1, 1, 1, 1, 1, 1, 1/4, 1/4, 1/4, 1/4)
## ranking
ranker_id <- rep(1:7, c(1, 3, 2, 2, 2, 2, 4))
## reverse orderings (unranked used to fill rows)
R2orderings <- matrix(c(2, 1, 3, 4,
                        1, 4, 3, 2,
                        1, 2, 3, 4,
                        3, 2, 1, 4,
                        3, 1, 2, 4,
                        3, 4, 1, 2,
                        1, 2, 3, 4), nrow = 7, byrow = TRUE)
## rankings which select from sets of size 1:4
G <- list(NULL, c(1, 2, 4, 5, 6), c(2, 3, 4, 5, 6), c(2, 7))
## weights corresponding to G (not aggregated)
W <- list(NULL, c(1, 1, 1, 1, 1), c(1, 1, 1, 1, 1), c(1, 1))
## tie orders
d <- c(1, 2, 4)
## set sizes
P <- 2:4
N <- ncol(R2)


if (require("gnm")){
    test_that("works with missing tie orders", {
        # missing lowest tie order
        model1 <- PlackettLuce(rankings = R, npseudo = 0)
        ## fit model via gnm
        dat <- poisson_rankings(R, aggregate = FALSE, as.data.frame = TRUE)
        model2 <- gnm(y ~ X, eliminate = z, family = poisson,
                      data = dat, constrain = 1)
        ## coefficients
        expect_equal(as.vector(coef(model1)[-1]),
                     as.vector(coef(model2)[-1]), tolerance = coef_tol)
        ## loglik
        expect_equal(logLik(model1), logLik_poisson.gnm(model2),
                     check.attributes = FALSE, tolerance = 1e-12)
        expect_equal(attr(logLik(model1), "df"),
                     attr(logLik_poisson.gnm(model2), "df"))

        # missing intermediate tie order
        model1 <- PlackettLuce(rankings = R2, npseudo = 0)
        ## fit model via gnm
        dat <- poisson_rankings(R2, aggregate = FALSE,
                                as.data.frame = TRUE)
        model2 <- gnm(y ~ X, eliminate = z, family = poisson,
                      data = dat, constrain = 1)
        ## coefficients
        expect_equal(as.vector(coef(model1)[-1]),
                     as.vector(coef(model2)[-1]), tolerance = coef_tol)
        ## loglik
        expect_equal(logLik(model1), logLik_poisson.gnm(model2),
                     check.attributes = FALSE, tolerance = 1e-12)
        expect_equal(attr(logLik(model1), "df"),
                     attr(logLik_poisson.gnm(model2), "df"))
        ## fitted values
        expect_equal(fitted(model1)$fitted, fitted(model2)[dat$y == 1],
                     check.attributes = FALSE, tolerance = coef_tol)
        ## vcov
        expect_equal(vcov(model1), unclass(vcov(model2)),
                     check.attributes = FALSE, tolerance = coef_tol)
        ## estfun
        par <- model1$coefficients
        fit <- expectation(c("alpha", "delta"), par[1:N], c(1.0, par[-(1:N)]),
                           NULL, N, d, P, R2orderings, G, W)
        # score wrt log-parameters
        score <- score_common(par = par, N = N, mu = NULL, Kinv = NULL,
                              A = A, B = B, fit = fit)*par
        # score = c(A - fit$expA, B[-1] - fit$expB*par[-(1:m)])
        expect_equal(colSums(estfun(model1, ref = NULL)), score)
    })
}

test_that("works with adherence missing tie orders", {
    # missing intermediate tie order
    gamma <- list(shape = 100, rate = 100)
    model1 <- PlackettLuce(rankings = R2, npseudo = 0,
                           gamma = gamma)
    alpha <- exp(coef(model1)[1:4])
    delta <- exp(coef(model1)[5:6])
    a <- model1$adherence
    wa <- model1$weights
    ## compute log-posterior using expectation (tested above)
    A <- unname(rowsum(a[ranker_id]*S, item_id)[,1L])
    fit <- expectation("all", alpha, c(1.0, delta),
                       a, N, d, P, R2orderings, G, W)
    res <- -loglik_common(c(alpha, delta), N, NULL, NULL, A, B, fit)
    logp <- -res + sum(wa*((gamma$shape - 1L)*log(a) - gamma$rate*a))
    ## compute log-likelihood using normalization
    # (only used when estimating adherence)
    Z <- unname(rowsum(S*log(alpha[item_id]), ranker_id)[,1L])
    fit2 <- normalization(alpha, c(1.0, delta),
                          a, d, P, R2orderings, G, W)
    res2 <- -loglik_adherence(a, gamma$shape, gamma$rate, wa, Z, fit2)
    logp2 <- -res2 + sum(B[-1]*log(delta))
    expect_equal(logp, logp2)
})
