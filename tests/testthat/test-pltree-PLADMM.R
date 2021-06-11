context("implementation [pladmmfit and pladmmtree]")

test_that('pladmmfit works [salad]',  {
    ## convert salad to rankings object
    salad_rankings <- as.rankings(salad)

    # create a grouped rankings object, grouping the rankings into 2 groups
    G <- group(salad_rankings, rep(1:2, nrow(salad_rankings)/2))

    # pladmmfit() gives the same results as pladmm()
    mod1 <- pladmmfit(G, model.matrix(~ acetic + gluconic, data = features),
                      rho = 8)
    mod2 <- pladmm(salad, ~ acetic + gluconic, data = features, rho = 8)

    expect_equivalent(coef(mod1), coef(mod2)) # names differ
    expect_equal(mod1$objfun, -logLik(mod2))
})
