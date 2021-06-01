context("implementation [estfun ADMM]")

library(prefmod) # salad data
library(survival)

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

test_that("PLADMM estfun matches PlackettLuce [salad]", {
    ## model with separate worths for each dressing
    ## setting rho ~ 10% log-lik gives good results (not extensively tested!)
    res0_PLADMM <- pladmm(salad_rankings, ~ salad, data = features, rho = 8,
                          rtol = 1e-5)
    res0_PL <- PlackettLuce(salad_rankings, npseudo = 0)
    ## expect that estfun results are equal
    expect_equivalent(estfun(res0_PLADMM),
                      estfun(res0_PL),
                      tol = coef_tol)
})

test_that("PLADMM estfun matches rank ordered logit model [salad]", {
    ## setting rho ~ 10% log-lik gives good results (not extensively tested!)
    res_PLADMM <- pladmm(salad_rankings, ~ acetic + gluconic, data = features,
                         rho = 8, rtol = 1e-5)
    res_RO <- coxph(Surv(ranking, status) ~ acetic + gluconic + strata(chid),
                    data = cbind(salad_long_rankings, status = 1))
    ## expect that estfun results are equal
    expect_equivalent(estfun(res_PLADMM),
                      rowsum(estfun(res_RO), salad_long_rankings$chid),
                      tol = coef_tol)
})
