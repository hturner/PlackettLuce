context("implementation [ADMM]")

library(prefmod) # salad data
library(survival) # coxph for rank-ordered logit

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
## create design matrix to predict worth by acetic and gluconic acid conc
## intercept not needed as unidentifiable
## (gives same ranking probabilities regardless of value)
salad_X <- model.matrix(~ acetic + gluconic, data = features)
## convert rankings to long-form (explode rankings)
salad_long_rankings <-
    data.frame(salad_X[t(col(salad_rankings)),],
               item = rep(LETTERS[1:4], nrow(salad_rankings)),
               ranking = c(t(salad_rankings)),
               chid = c(t(row(salad_rankings))))

coef_tol <- 1e-4

test_that("PLADMM worth estimates match PlackettLuce and rologit [salad]", {
    ## create design matrix for separate worths for each dressing
    ## intercept not needed as unidentifiable
    ## (gives same ranking probabilities regardless of value)
    salad_X0 <- model.matrix(~ salad, data = features)
    ## setting rho ~ 10% log-lik gives good results (not extensively tested!)
    res0_PLADMM <- pladmm(salad_rankings, salad_X0, rho = 8)
    res0_PL <- PlackettLuce(salad_rankings, npseudo = 0)
    res0_RO <- coxph(Surv(ranking, status) ~ item + strata(chid),
                     data = cbind(salad_long_rankings, status = 1))
    ## expect that log-worths are equal, PLADMM and PlackettLuce
    expect_equal(log(res0_PLADMM[["pi"]]),
                 unname(as.vector(log(coef(res0_PL, log = FALSE)))),
                 tol = coef_tol)
    ## expect that log-worths are equal, PLADMM and rank-ordered logit
    lambda <- c(itemA = 0, coef(res0_RO))
    log_worth <- log(exp(lambda)/sum(exp(lambda))) # log of worths normalized to sum to 1
    expect_equal(log(res0_PLADMM[["pi"]]),
                 unname(log_worth),
                 tol = coef_tol)
    ## expect log-worths predicted by linear predictor equal, PLADMM and PlackettLuce
    expect_equal(c(salad_X0 %*% res0_PLADMM[["beta"]]),
                 unname(as.vector(log(coef(res0_PL, log = FALSE)))),
                 tol = coef_tol)
    ## expect beta coef from PLADMM equal non-zero (differences in) log-worth from PlackettLuce
    ## TRUE to lower tolerance
    expect_equivalent(res0_PLADMM[["beta"]][-1],
                      as.vector(coef(res0_PL)[-1]),
                      tol = 10*coef_tol)
})

test_that("PLADMM worth estimates match rank ordered logit model [salad]", {
    ## setting rho ~ 10% log-lik gives good results (not extensively tested!)
    res_PLADMM <- pladmm(salad_rankings, salad_X, rho = 8)
    ## expect fitted log-worths equal to those predicted by linear predictor
    lambda <- as.vector(salad_X %*% matrix(res_PLADMM[["beta"]]))
    expect_equal(log(res_PLADMM[["pi"]]),
                 lambda,
                 tol = coef_tol)
    ## rank-ordered logit
    res_RO <- coxph(Surv(ranking, status) ~ acetic + gluconic + strata(chid),
                    data = cbind(salad_long_rankings, status = 1))
    beta <- c(0, coef(res_RO))
    lambda <- as.vector(salad_X %*% matrix(beta))
    log_worth <- log(exp(lambda)/sum(exp(lambda))) # log of worths normalized to sum to 1
    ## expect log-worths predicted by linear predictor from PLADMM
    ## equal to log-worths based on rank-orded logit.
    expect_equal(log(res_PLADMM[["pi"]]),
                 unname(log_worth),
                 tol = coef_tol)
    ## expect two approaches to give same coefficients (different intercept)
    expect_equal(res_PLADMM[["beta"]][-1],
                 beta[-1],
                 tol = coef_tol)
})
