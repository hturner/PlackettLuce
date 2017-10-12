context("implementation [ref argument in PlackettLuce]")

## Using the artificial example in ?PlackettLuce
R <- matrix(c(1, 2, 0, 0,
              4, 1, 2, 3,
              2, 1, 1, 1,
              1, 2, 3, 0,
              2, 1, 1, 0,
              1, 0, 3, 2), nrow = 6, byrow = TRUE)
colnames(R) <- c("apple", "banana", "orange", "pear")

model_fruits1 <- PlackettLuce(rankings = R)

test_that("ref implementation in coef.PlackettLuce is correct", {
    cc <- log(model_fruits1$coefficients)
    cc[1:4] <- cc[1:4] - cc[2]
    expect_equal(unclass(coef(model_fruits1, ref = as.integer(2))), cc,
                 tolerance = 1e-12, check.attributes = FALSE)
    expect_equal(unclass(coef(model_fruits1, ref = "banana")), cc,
                 tolerance = 1e-12, check.attributes = FALSE)
})

