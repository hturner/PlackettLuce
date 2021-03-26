context("implementation [ADMM]")

library(prefmod) # salad data
library(survival) # coxph for rank-ordered logit

## artificial example in ?PlackettLuce
M <- matrix(c(1, 2, 0, 0,
              3, 1, 4, 0,
              1, 4, 0, 0,
              2, 1, 4, 3,
              2, 3, 4, 0,
              1, 2, 3, 0), nrow = 6, byrow = TRUE)
R <- as.rankings(M, "ordering")
colnames(R) <- c("apple", "banana", "orange", "pear")

# analysis of salad data from Critchlow, D. E. & Fligner, M. A. (1991).
## salad is a data.frame of rankings for items A B C D
## 1 = most tart, 4 = least tart
## convert to rankings object
salad_rankings <- as.rankings(salad)
## create data frame of corresponding features
## (acetic and gluconic acid concentrations in salad dressings)
features <- data.frame(salad = LETTERS[1:4],
                       acetic = c(0.5, 0.5, 1, 0),
                       gluconic = c(0, 10, 0, 10))
## convert rankings to long-form (explode rankings)
salad_long_rankings <-
    data.frame(features[t(col(salad_rankings)),],
               ranking = c(t(salad_rankings)),
               chid = c(t(row(salad_rankings))))

coef_tol <- 1e-4

test_that("PLADMM works for partial rankings [fruits]", {
    ## create design matrix for separate worths for each fruit
    dat <- data.frame(fruit = factor(colnames(R), levels = colnames(R)))
    ## setting rho ~ 10% log-lik gives good results (not extensively tested!)
    partial_PLADMM <- pladmm(R, ~ fruit, dat, rho = 1, rtol = 1e-5)
    partial_PL <- PlackettLuce(rankings = R, npseudo = 0)
    ## expect that log-worths are equal, PLADMM and PlackettLuce
    expect_equal(log(partial_PLADMM[["pi"]]),
                 c(log(coef(partial_PL, log = FALSE))),
                 tol = coef_tol)
    ## expect log-worths predicted by linear predictor equal, PLADMM and PlackettLuce
    expect_equal(log(partial_PLADMM[["tilde_pi"]]),
                 c(log(coef(partial_PL, log = FALSE))),
                 tol = coef_tol)
    ## expect beta coef from PLADMM equal non-zero (differences in) log-worth from PlackettLuce
    expect_equivalent(coef(partial_PLADMM)[-1],
                      as.vector(coef(partial_PL)[-1]),
                      tol = coef_tol)
    ## expect log-likelihood equal, PLADMM andPlackettLuce
    expect_equal(logLik(partial_PLADMM),
                 logLik(partial_PL),
                 tol = coef_tol)
})

test_that("PLADMM worth estimates match PlackettLuce and rologit [salad]", {
    ## model with separate worths for each dressing
    ## setting rho ~ 10% log-lik gives good results (not extensively tested!)
    res0_PLADMM <- pladmm(salad_rankings, ~ salad, data = features, rho = 8)
    res0_PL <- PlackettLuce(salad_rankings, npseudo = 0)
    res0_RO <- coxph(Surv(ranking, status) ~ salad + strata(chid),
                     data = cbind(salad_long_rankings, status = 1))
    ## expect that log-worths are equal, PLADMM and PlackettLuce
    expect_equal(log(res0_PLADMM[["pi"]]),
                 c(log(coef(res0_PL, log = FALSE))),
                 tol = coef_tol)
    ## expect that log-worths are equal, PLADMM and rank-ordered logit
    lambda <- c(itemA = 0, coef(res0_RO))
    log_worth <- log(exp(lambda)/sum(exp(lambda))) # log of worths normalized to sum to 1
    expect_equal(unname(log(res0_PLADMM[["pi"]])),
                 unname(log_worth),
                 tol = coef_tol)
    ## expect log-worths predicted by linear predictor equal, PLADMM and PlackettLuce
    expect_equal(c(res0_PLADMM$x %*% coef(res0_PLADMM)),
                 unname(as.vector(log(coef(res0_PL, log = FALSE)))),
                 tol = coef_tol)
    ## expect beta coef from PLADMM equal non-zero (differences in) log-worth from PlackettLuce
    ## TRUE to lower tolerance
    expect_equivalent(coef(res0_PLADMM)[-1],
                      as.vector(coef(res0_PL)[-1]),
                      tol = 10*coef_tol)
    ## expect log-likelihood equal, PLADMM andPlackettLuce
    expect_equal(logLik(res0_PLADMM),
                 logLik(res0_PL),
                 tol = coef_tol)
})

test_that("PLADMM worth estimates match rank ordered logit model [salad]", {
    ## setting rho ~ 10% log-lik gives good results (not extensively tested!)
    res_PLADMM <- pladmm(salad_rankings, ~ acetic + gluconic, data = features,
                         rho = 8)
    ## expect fitted log-worths equal to those predicted by linear predictor
    lambda <- c(res_PLADMM$x %*% matrix(coef(res_PLADMM)))
    expect_equal(unname(log(res_PLADMM[["pi"]])),
                 lambda,
                 tol = coef_tol)
    ## rank-ordered logit
    res_RO <- coxph(Surv(ranking, status) ~ acetic + gluconic + strata(chid),
                    data = cbind(salad_long_rankings, status = 1))
    beta <- c(0, coef(res_RO))
    lambda <- as.vector(res_PLADMM$x %*% matrix(beta))
    log_worth <- log(exp(lambda)/sum(exp(lambda))) # log of worths normalized to sum to 1
    ## expect log-worths predicted by linear predictor from PLADMM
    ## equal to log-worths based on rank-orded logit.
    expect_equal(unname(log(res_PLADMM[["pi"]])),
                 log_worth,
                 tol = coef_tol)
    ## expect two approaches to give same coefficients (different intercept)
    expect_equal(coef(res_PLADMM)[-1],
                 beta[-1],
                 tol = coef_tol)
    ## expect log-likelihood equal (survival returns extra `nobs` attribute)
    expect_equivalent(logLik(res_PLADMM),
                      logLik(res_RO),
                      tol = coef_tol)
    ## expect vcov equal
    expect_equal(vcov(res_PLADMM),
                 vcov(res_RO),
                 tol = coef_tol)
})
