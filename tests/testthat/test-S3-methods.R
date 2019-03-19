context("PlackettLuce methods")

## Simple example with few coefficients
R <- matrix(c(1, 2, 0,
              3, 1, 2,
              2, 1, 1,
              1, 2, 3,
              2, 1, 1,
              1, 0, 2), nrow = 6, byrow = TRUE)
colnames(R) <- c("apple", "banana", "orange")

tol <- 1e-6

model_fruits1 <- PlackettLuce(rankings = R)

model_fruits2 <- PlackettLuce(rankings = R[-5,],
                              weights = c(1, 1, 2, 1, 1))

test_that("output of print.PlackettLuce is correct", {
    expect_known_output(print(model_fruits1),
                        file = test_path("outputs/print.rds"))
})

test_that("output of print.coef.PlackettLuce is correct", {
    expect_known_output(print(coef(model_fruits1)),
                        file = test_path("outputs/print.coef.rds"))
})

test_that("output of print.summary.PlackettLuce is correct", {
    expect_known_output(print(summary(model_fruits1)),
                        file = test_path("outputs/print.summary.rds"))
})

test_that("output of fitted.PlackettLuce is correct", {
    expect_known_value(fitted(model_fruits1),
                       file = test_path("outputs/fitted.rds"),
                       tol = tol)
    expect_known_value(fitted(model_fruits1, aggregate = FALSE),
                       file = test_path("outputs/fitted_individual.rds"),
                       tol = tol)
    expect_known_value(fitted(model_fruits1, free = FALSE),
                       file = test_path("outputs/fitted_all.rds"),
                       tol = tol)
})

test_that("output of fitted.PlackettLuce is correct [weights]", {
    # fitted the same apart from ranking ID(s) when aggregate
    expect_equal(fitted(model_fruits1, aggregate = TRUE)[-3],
                 fitted(model_fruits2, aggregate = TRUE)[-3])
    # non-aggregated for weighted same as aggregate for unweighted
    expect_equal(fitted(model_fruits1, aggregate = TRUE)[-3],
                 fitted(model_fruits2, aggregate = FALSE)[-3])
})

