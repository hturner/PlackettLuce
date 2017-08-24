context("implementation of the log-likelihood")

## Get the legacy implementation
source_files <- c("PlackettLuce0.R", "coef0.R",
                  "fitted0.R", "logLik0.R",
                  "print.PlackettLuce0.R", "summary0.R",
                  "vcov0.R")

for (file0 in source_files) {
    source(system.file("PlackettLuce0", file0, package = "PlackettLuce"))
}


## Using the artificial example in ?PlackettLuce
R <- matrix(c(1, 2, 0, 0,
              4, 1, 2, 3,
              2, 1, 1, 1,
              1, 2, 3, 0,
              2, 1, 1, 0,
              1, 0, 3, 2), nrow = 6, byrow = TRUE)
colnames(R) <- c("apple", "banana", "orange", "pear")

if (require("Matrix") & require("igraph") & require("rARPACK")) {
    model0 <- PlackettLuce0(rankings = R, ref = "orange")
    model1 <- PlackettLuce(rankings = R, ref = "orange")
    test_that("Current and legacy implementations return the same log-likelihood", {
        expect_equal(logLik(model0), logLik(model1), tolerance = 1e-12)
    })
}




