context("PlackettLuce methods")


if (requireNamespace("prefmod", quietly = TRUE)){
    ## model that predicts salad worth by acetic and gluconic acid conc
    res_PLADMM <- pladmm(salad_rankings, ~ acetic + gluconic, data = features,
                         rho = 8)

    test_that("output of print.PLADMM is correct", {
        expect_known_output(print(res_PLADMM),
                            file = test_path("outputs/print_PLADMM.txt"))
    })

    test_that("output of print.summary.PLADMM is correct", {
        expect_known_output(print(summary(res_PLADMM)),
                            file = test_path(
                                "outputs/print_summary_PLADMM.txt"))
    })
}
