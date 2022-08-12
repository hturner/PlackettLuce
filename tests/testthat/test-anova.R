context("implementation [anova ADMM]")

coef_tol <- 1e-4

if (requireNamespace("prefmod", quietly = TRUE) &
    require("survival")) {
    test_that("PLADMM anova matches rank ordered logit model [salad]", {
        ## setting rho ~ 10% log-lik gives good results (not extensively tested!)
        res0_PLADMM <- pladmm(salad_rankings, ~ acetic + gluconic,
                             data = features, rho = 8, rtol = 1e-5)
        res1_PLADMM <- pladmm(salad_rankings, ~ acetic,
                              data = features, rho = 8, rtol = 1e-5)
        res0_RO <- coxph(Surv(ranking, status) ~
                             acetic + gluconic + strata(chid),
                         data = cbind(salad_long_rankings, status = 1))
        res1_RO <- coxph(Surv(ranking, status) ~
                             acetic + strata(chid),
                         data = cbind(salad_long_rankings, status = 1))
        ## expect that anova equivalent for list of models
        anova1 <- anova(res0_PLADMM, res1_PLADMM)
        anova2 <- anova(res0_RO, res1_RO) # tables log-likelihood vs deviance
        expect_equal(anova1$Deviance, #difference in residual deviance
                     anova2$Chisq, #Chi-squared statistic
                     tol = coef_tol)
        expect_equal(anova1$Df, anova2$Df)
        expect_equal(anova1$`Pr(>Chi)`, #difference in residual deviance
                     anova2$`Pr(>|Chi|)`, #Chi-squared statistic
                     tol = coef_tol)
        ## expect that anova equivalent for single model
        anova1 <- anova(res0_PLADMM)
        anova2 <- anova(res0_RO) # tables log-likelihood vs deviance
        expect_equal(anova1$Deviance, #difference in residual deviance
                     anova2$Chisq, #Chi-squared statistic
                     tol = coef_tol)
        expect_equal(anova1$Df, anova2$Df)
        expect_equal(anova1$`Pr(>Chi)`, #difference in residual deviance
                     anova2$`Pr(>|Chi|)`, #Chi-squared statistic
                     tol = coef_tol)
    })
}
