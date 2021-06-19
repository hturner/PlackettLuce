context("implementation [pladmm_mob_fit and pladmmtree]")

test_that('pladmm_mob_fit works [salad]',  {
    # convert salad to rankings object
    salad_rankings <- as.rankings(salad)

    # create a grouped rankings with 2 arbitrary groups
    G <- group(salad_rankings, rep(1:2, nrow(salad_rankings)/2))

    # replace rankings with orderings
    ord <- PlackettLuce:::convert_to_orderings(attr(G, "rankings"))
    attr(G, "rankings")[] <- ord[]

    # pladmm_mob_fit() gives the same results as pladmm()
    mod1 <- pladmm_mob_fit(G,
                           model.matrix(~ acetic + gluconic, data = features),
                           rho = 8)
    mod2 <- pladmm(salad, ~ acetic + gluconic, data = features, rho = 8)

    expect_equal(coef(mod1), coef(mod2))
    expect_equal(mod1$objfun, -logLik(mod2)[[1]])
})

test_that('pltree works with worth formula, 1 node [salad]',  {
    # convert salad to rankings object
    salad_rankings <- as.rankings(salad)

    # create a grouped rankings with 2 arbitrary groups
    G <- group(salad_rankings, rep(1:2, nrow(salad_rankings)/2))

    # create some random covariates
    # (expect 1 node as min node size is 10, so seed doesn't matter!)
    zvar <- data.frame(z1 = rnorm(length(G)),
                       z2 = rpois(length(G), lambda = 2))

    # pltree gives equivalent results to pladmm()
    mod1 <- pltree(G ~ .,
                   worth = ~ acetic + gluconic,
                   data = list(zvar, features),
                   rho = 8)
    mod2 <- pladmm(salad, ~ acetic + gluconic, data = features, rho = 8)

    expect_equal(coef(mod1), coef(mod2))
    expect_equal(as.vector(itempar(mod1)), as.vector(itempar(mod2)))
    expect_equal(vcov(mod1)$`1`, vcov(mod2))
    expect_equal(AIC(mod1), AIC(mod2))
})

# TODO: predict method

# need proper test here: redo pltree or bttree so have something to
# test against
test_that('pltree works with worth formula, 2 node [salad]',  {
    # convert salad to rankings object
    salad_rankings <- as.rankings(salad)

    # create a grouped rankings with 2 arbitrary groups
    G <- group(salad_rankings, rep(1:8, nrow(salad_rankings)/8))

    # create some random covariates
    set.seed(1)
    zvar <- data.frame(z1 = rnorm(length(G)),
                       z2 = rpois(length(G), lambda = 2))

    # set high alpha to force two nodes
    mod1 <- pltree(G ~ .,
                   worth = ~ acetic + gluconic,
                   data = list(zvar, features),
                   rho = 1, minsize = 4, alpha = 0.8)
})
