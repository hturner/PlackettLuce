context("ref argument in PlackettLuce")

## Using the artificial example in ?PlackettLuce
R <- matrix(c(1, 2, 0, 0,
              4, 1, 2, 3,
              2, 1, 1, 1,
              1, 2, 3, 0,
              2, 1, 1, 0,
              1, 0, 3, 2), nrow = 6, byrow = TRUE)
colnames(R) <- c("apple", "banana", "orange", "pear")

model_fruits1 <- PlackettLuce(rankings = R, ref = "orange")
## Use as.integer here to match "id" type that PlackettLuce used internally
model_fruits2 <- PlackettLuce(rankings = R, ref = as.integer(3))

test_that("ref works with both numeric and character baseline specification", {
    expect_identical(coef(model_fruits1), coef(model_fruits2))
})

test_that("ref implementation in coef.PlackettLuce is correct", {
    cc <- log(model_fruits1$coefficients)
    cc[1:4] <- cc[1:4] - cc[1]
    expect_equal(unclass(coef(model_fruits1, ref = as.integer(1))), cc,
                 tolerance = 1e-12, check.attributes = FALSE)
})

