context("PlackettLuce methods")

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
## create design matrix to predict worth by acetic and gluconic acid conc
## intercept not needed as unidentifiable
## (gives same ranking probabilities regardless of value)
salad_X <- model.matrix(~ acetic + gluconic, data = features)
res_PLADMM <- pladmm(salad_rankings, salad_X, rho = 8)

test_that("output of print.PlackettLuce is correct", {
    expect_known_output(print(res_PLADMM),
                        file = test_path("outputs/print_PLADMM.txt"))
})

test_that("output of print.PlackettLuce is correct", {
    expect_known_output(print(summary(res_PLADMM)),
                        file = test_path("outputs/print_PLADMM.txt"))
})
