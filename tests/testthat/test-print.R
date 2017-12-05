context("implementation [print methods]")

## Simple example with few coefficients
R <- matrix(c(1, 2, 0,
              3, 1, 2,
              2, 1, 1,
              1, 2, 3,
              2, 1, 1,
              1, 0, 2), nrow = 6, byrow = TRUE)
colnames(R) <- c("apple", "banana", "orange")

model_fruits1 <- PlackettLuce(rankings = R)

test_that("output of print.coef.PlackettLuce is correct", {
    expect_output(print(coef(model_fruits1)),
                  paste0("     apple     banana     orange       tie2 \n",
                         " 0.0000000  0.2867987 -0.4636687 -1.0701129 "))
})

