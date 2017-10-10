context("implementation [plfit and pltree]")

coef_tol <- 1e-06
loglik_tol <- 1e-07

if (require(psychotree) & require(sandwich)){
    data("Topmodel2007", package = "psychotree")
    preference <- Topmodel2007$preference

    # plfit vs btmodel
    btmod <- btmodel(preference, ref = "Anja")

    G <- as.grouped_rankings(preference)
    plmod <- plfit(G, npseudo = 0, ref= "Anja", estfun = TRUE, object = TRUE)

    # some conditions with psychotree 0.15.1 and psychotools 0.4.2
    test_that("plfit and bt model agree [Topmodel2007]",
              {
                  # coef
                  expect_equal(coef(btmod), coef(plmod)[-6])
                  # estfun - only agree if last level taken as ref in btmodel
                  E1 <- estfun(btmod)
                  E2 <- plmod$estfun
                  expect_equal(unname(E1), unname(E2))
                  # worth and vcov
                  # - vcov only agrees if first level taken as ref in btmodel
                  btmod <- btmodel(preference, ref = "Barbara")
                  w1 <- worth(btmod, log = FALSE, ref = 1)
                  w2 <- worth(plmod$object, log = FALSE, ref = 1)
                  expect_equal(coef(w1), coef(w2))
                  expect_equal(attr(w1, "vcov"), attr(w2, "vcov"), 1e-6)
              })
    # pltree vs brtree
    bt_tree <- bttree(preference ~ ., data = Topmodel2007, minsize = 5,
                      ref = "Anja")
    pl_tree <- pltree(G ~ ., npseudo = 0, data = Topmodel2007[, -1],
                      minsize = 5, ref = "Anja")

    test_that("pltree and bttree agree [Topmodel2007]",
              {
                 expect_equal(itempar(bt_tree), itempar(pl_tree))
              })
    # predict pltree
    # work round some bugs in psychotree 0.15.1
    newdata <- Topmodel2007[1:3,]
    test_that('predict.pltree works for type = "itempar" [Topmodel2007]',
              {
                  # probabilities
                  expect_equal(itempar(bt_tree)[
                      as.character(predict(bt_tree, type = "node")),],
                      predict(pl_tree, type = "itempar"),
                      check.attributes = FALSE)
                  expect_equal(itempar(bt_tree)[
                      as.character(predict(bt_tree, newdata = newdata,
                                           type = "node")),],
                      predict(pl_tree,  newdata = newdata, type = "itempar"),
                      check.attributes = FALSE)
                  # log-abilities
                  expect_equal(itempar(bt_tree, log = TRUE)[
                      as.character(predict(bt_tree, type = "node")),],
                      predict(pl_tree, type = "itempar", log = TRUE),
                      check.attributes = FALSE)
                  expect_equal(itempar(bt_tree, log = TRUE)[
                      as.character(predict(bt_tree, newdata = newdata,
                                           type = "node")),],
                      predict(pl_tree, newdata = newdata, type = "itempar",
                              log = TRUE),
                      check.attributes = FALSE)
              })
    test_that('predict.pltree works for type = "rank" [Topmodel2007]',
              {
                  rank <- t(apply(-itempar(bt_tree), 1, rank))
                  expect_equal(rank[
                      as.character(predict(bt_tree, newdata = newdata,
                                           type = "node")),],
                      predict(pl_tree, newdata = newdata, type = "rank"),
                      check.attributes = FALSE)
                  expect_equal(rank[
                      as.character(predict(bt_tree, newdata = newdata,
                                           type = "node")),],
                      predict(pl_tree, newdata = newdata, type = "rank"),
                      check.attributes = FALSE)
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
}
