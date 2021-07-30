context("implementation [ADMM]")

coef_tol <- 1e-4

if (requireNamespace("prefmod", quietly = TRUE)) {
    test_that("PLADMM predict works for standard PL [salad]", {
        ## setting rho ~ 10% log-lik gives good results (not extensively tested!)
        ## reduce rtol a little so vcov more accurate
        res0_PLADMM <- pladmm(salad_rankings, ~ salad, data = features, rho = 8,
                              rtol = 1e-5)
        ## linear predictor, no new data
        expect_equal(predict(res0_PLADMM),
                     log(res0_PLADMM[["tilde_pi"]]),
                     tol = coef_tol)
        ## same returned by fitted
        expect_equal(predict(res0_PLADMM),
                     fitted(res0_PLADMM),
                     tol = coef_tol)
        ## linear predictor, new data
        expect_equal(unname(predict(res0_PLADMM, newdata = features[1:2,])),
                     unname(predict(res0_PLADMM)[1:2]),
                     tol = coef_tol)
        ## itempar, no new data
        expect_equal(predict(res0_PLADMM, type = "itempar"),
                     c(itempar(res0_PLADMM)),
                     tol = coef_tol)
        ## itempar, new data
        worth <- itempar(res0_PLADMM)[1:2]
        expect_equal(unname(predict(res0_PLADMM, type = "itempar",
                                    newdata = features[1:2,])),
                     unname(worth/sum(worth)),
                     tol = coef_tol)
    })

    if (requireNamespace("BradleyTerry2", quietly = TRUE)) {
        test_that("PLADMM predict works for PL with covariates [salad pairs]", {
            ## setting rho ~ 10% log-lik gives good results (not extensively tested!)
            res_PLADMM <- pladmm(salad_pairs, ~ acetic + gluconic,
                                 data = features, rho = 2)
            winner <- apply(salad_pairs == 1, 1, which)
            loser <- apply(salad_pairs == 2, 1, which)
            lev <- colnames(salad_pairs)
            contests <- data.frame(winner = factor(lev[winner], levels = lev),
                                   loser = factor(lev[loser]), levels = lev)
            ## warns that no random effect in predictor
            res_BTm <- suppressWarnings(BTm(outcome = rep(1, nrow(salad_pairs)),
                                            winner, loser,
                                            ~ acetic[..] + gluconic[..],
                                            data = list(contests = contests,
                                                        predictors = features)))
            # expect log-worth the same up to constant
            ability <- c(BTabilities(res_BTm)[, "ability"])
            expect_equal(predict(res_PLADMM) - coef(res_PLADMM)[1],
                         ability,
                         coef_tol)
            # expect s.e. of log-worth to be the same
            se <- c(BTabilities(res_BTm)[, "s.e."])
            expect_equal(predict(res_PLADMM, se.fit = TRUE)$se.fit,
                         se,
                         coef_tol)
            # expect predicted item worth correct for new data
            pred1 <- predict(res_PLADMM, type = "itempar", ref = 2:3,
                             se.fit = TRUE)
            pred2 <- predict(res_PLADMM, type = "itempar",
                             newdata = features[2:3,],
                             se.fit = TRUE)
            expect_equal(unname(pred1$fit[2:3]),
                         unname(pred2$fit),
                         coef_tol)
            expect_equal(unname(pred1$se.fit[2:3]),
                         unname(pred2$se.fit),
                         coef_tol)
        })

        test_that("PLADMM predict works for PL with other contrasts [salad pairs]", {
            ## setting rho ~ 10% log-lik gives good results (not extensively tested!)
            res_PLADMM <- pladmm(salad_pairs, ~ salad, data = features,
                                 rho = 1, contrasts = list(salad = "contr.sum"),
                                 rtol = 1e-5)
            winner <- apply(salad_pairs == 1, 1, which)
            loser <- apply(salad_pairs == 2, 1, which)
            lev <- colnames(salad_pairs)
            contests <- data.frame(winner = factor(lev[winner], levels = lev),
                                   loser = factor(lev[loser]), levels = lev)
            ## warns that no random effect in predictor
            res_BTm <- BTm(outcome = rep(1, nrow(salad_pairs)),
                           winner, loser,
                           ~ salad, id = "salad", x = FALSE,
                           contrasts = list(salad = "contr.sum"),
                           data = list(contests = contests,
                                       predictors = features))
            # expect log-worth the same up to constant
            ability <- c(BTabilities(res_BTm)[, "ability"])
            expect_equal(predict(res_PLADMM) - coef(res_PLADMM)[1],
                         ability,
                         coef_tol)
            # expect s.e. of log-worth to be the same
            se <- c(BTabilities(res_BTm)[, "s.e."])
            expect_equal(predict(res_PLADMM, se.fit = TRUE)$se.fit,
                         se,
                         coef_tol)
            # expect predicted item worth correct for new data
            pred1 <- predict(res_PLADMM, type = "itempar", ref = 2:3,
                             se.fit = TRUE)
            pred2 <- predict(res_PLADMM, type = "itempar",
                             newdata = features[2:3,],
                             se.fit = TRUE)
            expect_equal(unname(pred1$fit[2:3]),
                         unname(pred2$fit),
                         coef_tol)
            expect_equal(unname(pred1$se.fit[2:3]),
                         unname(pred2$se.fit),
                         coef_tol)
        })
    }
}
