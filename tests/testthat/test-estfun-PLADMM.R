context("implementation [estfun ADMM]")

coef_tol <- 1e-4

if (requireNamespace("prefmod", quietly = TRUE) &
    require("survival")) {
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
        res_PLADMM <- pladmm(salad_rankings, ~ acetic + gluconic,
                             data = features, rho = 8, rtol = 1e-5)
        res_RO <- coxph(Surv(ranking, status) ~
                            acetic + gluconic + strata(chid),
                        data = cbind(salad_long_rankings, status = 1))
        ## expect that estfun results are equal
        expect_equivalent(estfun(res_PLADMM),
                          rowsum(estfun(res_RO), salad_long_rankings$chid),
                          tol = coef_tol)
    })
}
