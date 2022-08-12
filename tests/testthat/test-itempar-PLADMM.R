coef_tol <- 1e-4

if (requireNamespace("prefmod", quietly = TRUE)) {
    test_that("PLADMM itempar works for standard PL [salad]", {
        ## setting rho ~ 10% log-lik gives good results (not extensively tested!)
        ## reduce rtol a little so vcov more accurate
        res0_PLADMM <- pladmm(salad_rankings, ~ salad, data = features, rho = 8,
                              rtol = 1e-5)
        res0_PL <- PlackettLuce(salad_rankings, npseudo = 0)
        expect_ok <- function(itemparPLADMM, itemparPL, tol){
            attr(itemparPL, "model") <- "PLADMM"
            expect_equal(itemparPL, itemparPLADMM, tolerance = tol)
        }
        ## expect that itempar equal, default settings (ref = NULL, log = FALSE)
        expect_ok(itempar(res0_PLADMM),
                  itempar(res0_PL),
                  tol = coef_tol)
        ## expect that itempar equal, log worth s.t. sum log worth is zero
        expect_ok(itempar(res0_PLADMM, ref = NULL, log = TRUE),
                  itempar(res0_PL, ref = NULL, log = TRUE),
                  tol = coef_tol)
        ## expect that itempar equal, worth s.t. first log worth is zero
        expect_ok(itempar(res0_PLADMM, ref = 1, log = FALSE),
                  itempar(res0_PL, ref = 1, log = FALSE),
                  tol = coef_tol)
        ## expect that itempar equal, log worth s.t. first log worth is zero
        expect_ok(itempar(res0_PLADMM, ref = 1, log = TRUE),
                  itempar(res0_PL, ref = 1, log = TRUE),
                  tol = coef_tol)
        ## expect that itempar equal,
        ## log worth s.t. sum first two log worths = zero
        expect_ok(itempar(res0_PLADMM, ref = 1:2, log = TRUE),
                  itempar(res0_PL, ref = 1:2, log = TRUE),
                  tol = coef_tol)
        ## expact that works with alias = FALSE
        expect_ok(itempar(res0_PLADMM, ref = 2, alias = FALSE, log = TRUE),
                  itempar(res0_PL, ref = 2, alias = FALSE, log = TRUE),
                  tol = coef_tol)
    })

    test_that("PLADMM itempar works for standard PL [salad pairs]", {
        ## setting rho ~ 10% log-lik gives good results (not extensively tested!)
        ## reduce rtol a little so vcov more accurate
        res0_PLADMM <- pladmm(salad_pairs, ~ salad, data = features, rho = 1,
                              rtol = 1e-6)
        res0_PL <- PlackettLuce(salad_pairs, npseudo = 0)
        expect_ok <- function(itemparPLADMM, itemparPL, tol){
            attr(itemparPL, "model") <- "PLADMM"
            expect_equal(itemparPL, itemparPLADMM, tolerance = tol)
        }
        ## expect that itempar equal, default settings (ref = NULL, log = FALSE)
        expect_ok(itempar(res0_PLADMM),
                  itempar(res0_PL),
                  tol = coef_tol)
        ## expect that itempar equal, log worth s.t. sum log worth is zero
        expect_ok(itempar(res0_PLADMM, ref = NULL, log = TRUE),
                  itempar(res0_PL, ref = NULL, log = TRUE),
                  tol = coef_tol)
        ## expect that itempar equal, worth s.t. first log worth is zero
        expect_ok(itempar(res0_PLADMM, ref = 1, log = FALSE),
                  itempar(res0_PL, ref = 1, log = FALSE),
                  tol = coef_tol)
        ## expect that itempar equal, log worth s.t. first log worth is zero
        expect_ok(itempar(res0_PLADMM, ref = 1, log = TRUE),
                  itempar(res0_PL, ref = 1, log = TRUE),
                  tol = coef_tol)
        ## expect that itempar equal,
        ## log worth s.t. sum first two log worths = zero
        expect_ok(itempar(res0_PLADMM, ref = 1:2, log = TRUE),
                  itempar(res0_PL, ref = 1:2, log = TRUE),
                  tol = coef_tol)
        ## expect that works with alias = FALSE
        expect_ok(itempar(res0_PLADMM, ref = 2, alias = FALSE, log = TRUE),
                  itempar(res0_PL, ref = 2, alias = FALSE, log = TRUE),
                  tol = coef_tol)
        if (require("BradleyTerry2", quietly = TRUE)){
            winner <- apply(salad_pairs == 1, 1, which)
            loser <- apply(salad_pairs == 2, 1, which)
            lev <- colnames(salad_pairs)
            contests <- data.frame(winner = factor(lev[winner], levels = lev),
                                   loser = factor(lev[loser]), levels = lev)
            res0_BTm <- BTm(outcome = rep(1, nrow(salad_pairs)),
                            winner, loser,
                            data = contests)
            # expect log-worth the same (first log worth set to zero)
            expect_equal(c(itempar(res0_PLADMM, ref = 1, log = TRUE)),
                         c(BTabilities(res0_BTm)[, "ability"]),
                         tolerance = coef_tol)
            # expect s.e. of log-worth the same
            expect_equal(sqrt(diag(attr(itempar(res0_PLADMM, ref = 1,
                                                log = TRUE),
                                        "vcov"))),
                         c(BTabilities(res0_BTm)[, "s.e."]),
                         tolerance = coef_tol)
        }
    })

    if (require("BradleyTerry2", quietly = TRUE)){
        test_that("PLADMM itempar works for PL with covariates [salad pairs]", {
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
                                            family = binomial,
                                            data = list(contests = contests,
                                                        predictors = features)))
            # expect log-worth the same up to constant
            ability <- c(BTabilities(res_BTm)[, "ability"])
            expect_equal(c(itempar(res_PLADMM, ref = 1, log = TRUE)),
                         ability - ability[1],
                         tolerance = coef_tol)
            # expect s.e. of contrasts of log-worth to be the same
            # get full vcov for BT abilities
            salad_X <- res_PLADMM$x
            V <- salad_X[,-1] %*% vcov(res_BTm) %*% t(salad_X[,-1])
            expect_equal(unname(diag(sqrt(V))),
                         unname(c(BTabilities(res_BTm)[, "s.e."])),
                         tolerance = coef_tol)
            # hence get vcov of contrast
            D <- diag(nrow(salad_X))
            D[,1] <- D[,1] - 1
            # D %*% BTabilities(res_BTm)[, "ability"] #contrast
            V2 <- D %*% V %*% t(D)
            expect_equal(unname(sqrt(diag(attr(itempar(res_PLADMM, ref = 1,
                                                       log = TRUE),
                                               "vcov")))),
                         unname(sqrt(diag(V2))),
                         tolerance = coef_tol)
            # expect fitted probability of ranking first to be the same
            pi <- itempar(res_PLADMM)
            alpha <- exp(BTabilities(res_BTm)[, "ability"])
            pi2 <- alpha/sum(alpha)
            expect_equal(pi2,
                         c(pi),
                         tolerance = coef_tol)
            # derivatives of exp(a_i)/sum_j(exp(a_j))
            D <- -outer(pi2, pi2)
            diag(D) <- diag(D) + pi2
            expect_equal(sqrt(diag(D %*% V %*% t(D))),
                         sqrt(diag(attr(pi, "vcov"))),
                         tolerance = coef_tol)
        })
    }
}
