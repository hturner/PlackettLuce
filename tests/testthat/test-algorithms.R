context("implementation [algorithms]")

## Using the artificial example in ?PlackettLuce
R <- matrix(c(1, 2, 0, 0,
              4, 1, 2, 3,
              2, 1, 1, 1,
              1, 2, 3, 0,
              2, 1, 1, 0,
              1, 0, 3, 2), nrow = 6, byrow = TRUE)
colnames(R) <- c("apple", "banana", "orange", "pear")

tol <- 1e-6

model_fruits1 <- PlackettLuce(rankings = R)
model_fruits2 <- PlackettLuce(rankings = R, method = "BFGS",
                              control = list(reltol = 1e-10))

test_that("BFGS gives same log-likelihood as iterative scaling", {
    expect_equal(logLik(model_fruits1), logLik(model_fruits2),
                 tolerance = tol)
})

test_that("BFGS gives same coef as iterative scaling", {
    expect_equal(coef(model_fruits1), coef(model_fruits2),
                 tolerance = tol, check.attributes = FALSE)
})

if (require(lbfgs)){
    model_fruits3 <- PlackettLuce(rankings = R, method = "L-BFGS",
                                  gtol = 0.2)

    test_that("L-BFGS gives same log-likelihood as iterative scaling", {
        expect_equal(logLik(model_fruits1), logLik(model_fruits3),
                     tolerance = tol)
    })

    test_that("L-BFGS gives same coef as iterative scaling", {
        expect_equal(coef(model_fruits1), coef(model_fruits3),
                     tolerance = tol, check.attributes = FALSE)
    })
}
