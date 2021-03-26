context("implementation [ADMM]")

library(prefmod) # salad data

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

## salad pairs (treat 3rd and 4th as unranked)
salad_pairs <- as.matrix(salad)
salad_pairs[salad_pairs %in% c(3, 4)] <- 0
salad_pairs <- as.rankings(salad_pairs)

coef_tol <- 1e-4

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

test_that("PLADMM predict works for PL with covariates [salad pairs]", {
    ## setting rho ~ 10% log-lik gives good results (not extensively tested!)
    res_PLADMM <- pladmm(salad_pairs, ~ acetic + gluconic, data = features,
                         rho = 2)
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
    expect_equal(predict(res_PLADMM) - coef(res_PLADMM)[1],
                 ability,
                 coef_tol)
    # expect s.e. of log-worth to be the same
    se <- c(BTabilities(res_BTm)[, "s.e."])
    expect_equal(predict(res_PLADMM, se.fit = TRUE)$se.fit,
                 se,
                 coef_tol)
})

