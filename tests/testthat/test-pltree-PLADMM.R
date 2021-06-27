context("implementation [pladmm_mob_fit and pladmmtree]")

coef_tol <- 1e-4

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

if (require(psychotree) & require(sandwich)){
    # pltree + PLADMM works, but currently very slow
    data("Topmodel2007", package = "psychotree")
    preference <- Topmodel2007$preference

    G <- as.grouped_rankings(preference)
    models <- colnames(attr(G, "rankings"))
    models <- factor(models, levels = models)
    pl_tree <- pltree(G ~ ., worth = ~ models,
                      data = list(Topmodel2007[, -1],
                                  data.frame(features = models)),
                      minsize = 30, rho = 1)
    bt_tree <- bttree(preference ~ ., data = Topmodel2007,
                      minsize = 5, ref = "Anja")

    test_that("pltree with worth formula and bttree agree [Topmodel2007]", {
        expect_equal(itempar(pl_tree),
                     itempar(bt_tree),
                     tol = coef_tol)

    })

    # predict pltree
    newdata <- Topmodel2007[1:3,]
    test_that('predict.pltree works for type = "itempar" [Topmodel2007]',
              {
                  # probabilities
                  expect_equal(itempar(bt_tree)[
                      as.character(predict(bt_tree, type = "node")),],
                      predict(pl_tree, type = "itempar"),
                      check.attributes = FALSE, tol = coef_tol)
                  expect_equal(itempar(bt_tree)[
                      as.character(predict(bt_tree, newdata = newdata,
                                           type = "node")),],
                      predict(pl_tree,  newdata = newdata, type = "itempar"),
                      check.attributes = FALSE, tol = coef_tol)
                  # log-abilities - okay to lower tol
                  expect_equal(itempar(bt_tree, log = TRUE)[
                      as.character(predict(bt_tree, type = "node")),],
                      predict(pl_tree, type = "itempar", log = TRUE),
                      check.attributes = FALSE, tol = coef_tol*10)
                  expect_equal(itempar(bt_tree, log = TRUE)[
                      as.character(predict(bt_tree, newdata = newdata,
                                           type = "node")),],
                      predict(pl_tree, newdata = newdata, type = "itempar",
                              log = TRUE),
                      check.attributes = FALSE, tol = coef_tol*10)
              })
    test_that('predict.pltree works for type = "rank" [Topmodel2007]',
              {
                  rank <- t(apply(-itempar(bt_tree), 1, rank))
                  expect_equal(rank[
                      as.character(predict(bt_tree, newdata = newdata,
                                           type = "node")),],
                      predict(pl_tree, newdata = newdata, type = "rank"),
                      check.attributes = FALSE, tol = coef_tol)
                  expect_equal(rank[
                      as.character(predict(bt_tree, newdata = newdata,
                                           type = "node")),],
                      predict(pl_tree, newdata = newdata, type = "rank"),
                      check.attributes = FALSE, tol = coef_tol)
              })
    test_that('predict.pltree works for type = "best" [Topmodel2007]',
              {
                  expect_equal(names(predict(bt_tree, type = "best")),
                               names(predict(pl_tree, type = "best")))
                  expect_equal(as.character(predict(bt_tree, type = "best")),
                               as.character(predict(pl_tree, type = "best")))
                  expect_equal(names(predict(bt_tree, newdata = newdata,
                                             type = "best")),
                               names(predict(pl_tree,  newdata = newdata,
                                             type = "best")))
                  expect_equal(as.character(predict(bt_tree, newdata = newdata,
                                                    type = "best")),
                               as.character(predict(pl_tree,  newdata = newdata,
                                                    type = "best")))
              })
    test_that('predict.pltree works for type = "node" [Topmodel2007]',
              {
                  tmp <- predict(pl_tree, type = "node")
                  mode(tmp) <- "numeric"
                  expect_equal(predict(bt_tree, type = "node"), tmp)
                  tmp <- predict(pl_tree,  newdata = newdata, type = "node")
                  mode(tmp) <- "numeric"
                  expect_equal(predict(bt_tree, newdata = newdata,
                                       type = "node"), tmp)
              })
    test_that('AIC.pltree works [Topmodel2007]',
              {
                  Topmodel2007$G <- G
                  aic1 <- AIC(pl_tree)
                  aic2 <- AIC(pl_tree, newdata = Topmodel2007)
                  expect_equal(aic1, aic2)

                  node <- predict(pl_tree, type = "node")
                  aic1 <- AIC(pl_tree, newdata = Topmodel2007[node == "3", -1])
                  aic2 <- -2*as.numeric(logLik(pl_tree[[3]])) +
                      2*attr(logLik(pl_tree), "df")
                  expect_equal(aic1, aic2)
              })
}


test_that('pltree works with worth formula, 2 node [salad]', {
    # use rankings just of 3 salads to begin with
    salad_rankings <- as.rankings(salad[, -4])

    # create a grouped rankings object with 2 arbitrary groups
    G <- group(salad_rankings, rep(1:8, nrow(salad_rankings)/8))

    # create some random covariates
    set.seed(1)
    zvar <- data.frame(z1 = rnorm(length(G)),
                       z2 = rpois(length(G), lambda = 2))

    # set high alpha to force two nodes
    mod1 <- pltree(G ~ .,
                   worth = ~acetic + gluconic,
                   data = list(zvar, features[1:3,]),
                   rho = 1, minsize = 4, alpha = 0.8)

    # predictions with "new" data
    # - two rankings by judge 1 in node 3 (z2 > 2)
    # - two rankings of 4 salads by a new judge in node 2 (z2 <= 2)
    salad_rankings_new <- as.rankings(rbind(cbind(salad[2:3, -4], D = 0),
                                            salad[5:6,]))
    G_new <- group(salad_rankings_new, c(1, 1, 2, 2))
    zvar_new <- rbind(zvar[1,],
                      data.frame(z1 = 0,
                                 z2 = 1))
    newdata <- list(cbind(data.frame(G = G_new),
                          zvar_new),
                    features)

    test_that('predict.pltree works for type = "node" [salad]',
              {
                  expect_equal(predict(mod1, newdata = newdata, type = "node"),
                               c("1" = "3", "2" = "2"))
              })
    test_that('predict.pltree works for type = "itempar" [salad]',
              {
                  # worth
                  pred <- predict(mod1,  newdata = newdata, type = "itempar")
                  node <- predict(mod1, newdata = newdata, type = "node")
                  X <- model.matrix(~ acetic + gluconic, data = features)
                  alpha <- exp(coef(mod1) %*% t(X))
                  expect_equal(pred,
                               (alpha/rowSums(alpha))[node,],
                               check.attributes = FALSE)
                  # log-abilities
                  # ...
              })
})

