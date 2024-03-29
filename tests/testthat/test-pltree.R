coef_tol <- 1e-06
loglik_tol <- 1e-07

if (require(psychotree) & require(sandwich)){
    data("Topmodel2007", package = "psychotree")
    preference <- Topmodel2007$preference

    # plfit vs btmodel
    btmod <- btmodel(preference, ref = "Anja")

    G <- as.grouped_rankings(preference)
    plmod <- plfit(G, npseudo = 0, ref = "Anja", estfun = TRUE, object = TRUE)

    # some conditions with psychotree 0.15.1 and psychotools 0.4.2
    test_that("plfit and bt model agree [Topmodel2007]",
              {
                  # coef
                  expect_equal(coef(btmod), coef(plmod)[-6])
                  # estfun - only agree if last level taken as ref in btmodel
                  E1 <- psychotools::estfun.btmodel(btmod)
                  E2 <- plmod$estfun
                  expect_equal(unname(E1), unname(E2))
                  # worth and vcov
                  # - vcov only agrees if first level taken as ref in btmodel
                  btmod <- btmodel(preference, ref = "Barbara")
                  w1 <- worth(btmod, log = FALSE, ref = 1)
                  w2 <- worth(plmod$object, log = FALSE, ref = 1)
                  expect_equal(coef(w1), coef(w2))
                  expect_equal(attr(w1, "vcov"), attr(w2, "vcov"),
                               tolerance = coef_tol)
              })
    # pltree vs bttree
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
                      ignore_attr = TRUE)
                  expect_equal(itempar(bt_tree)[
                      as.character(predict(bt_tree, newdata = newdata,
                                           type = "node")),],
                      predict(pl_tree,  newdata = newdata, type = "itempar"),
                      ignore_attr = TRUE)
                  # log-abilities
                  expect_equal(itempar(bt_tree, log = TRUE)[
                      as.character(predict(bt_tree, type = "node")),],
                      predict(pl_tree, type = "itempar", log = TRUE),
                      ignore_attr = TRUE)
                  expect_equal(itempar(bt_tree, log = TRUE)[
                      as.character(predict(bt_tree, newdata = newdata,
                                           type = "node")),],
                      predict(pl_tree, newdata = newdata, type = "itempar",
                              log = TRUE),
                      ignore_attr = TRUE)
              })
    test_that('predict.pltree works for type = "rank" [Topmodel2007]',
              {
                  rank <- t(apply(-itempar(bt_tree), 1, rank))
                  expect_equal(rank[
                      as.character(predict(bt_tree, newdata = newdata,
                                           type = "node")),],
                      predict(pl_tree, newdata = newdata, type = "rank"),
                      ignore_attr = TRUE)
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
    # coef pltree
    test_that('coef pltree works with log = FALSE [Topmodel2007]',
              {
                  expect_equal(coef(pl_tree, log = FALSE),
                               itempar(pl_tree, log = FALSE, vcov = FALSE))
                  expect_equal(coef(pl_tree)[, -6],
                               coef(bt_tree))
              })
}

# example with weights

data(beans, package = "PlackettLuce")

# Fill in the missing ranking
beans$middle <- complete(beans[c("best", "worst")],
                         items = c("A", "B", "C"))

# Use these names to decode the orderings of order 3
order3 <- decode(beans[c("best", "middle", "worst")],
                 items = beans[c("variety_a", "variety_b", "variety_c")],
                 code = c("A", "B", "C"))

# Convert these results to a vector and get the corresponding trial variety
outcome <- unlist(beans[c("var_a", "var_b", "var_c")])
trial_variety <- unlist(beans[c("variety_a", "variety_b", "variety_c")])

# Create a data frame of the implied orderings of order 2
order2 <- data.frame(Winner = ifelse(outcome == "Worse",
                                     "Local", trial_variety),
                     Loser = ifelse(outcome == "Worse",
                                    trial_variety, "Local"),
                     stringsAsFactors = FALSE, row.names = NULL)

# Finally combine the rankings of order 2 and order 3
R <- rbind(as.rankings(order3, input = "ordering"),
           as.rankings(order2, input = "ordering"))

# Group the rankings by the corresponding farm
G <- group(R, rep(seq_len(nrow(beans)), 4))

weights <- c(rep(0.3, 177), rep(2, 481), rep(0.3, 184))

pl_tree <- pltree(G ~ season,
                  data = beans, alpha = 0.05, weights = weights )

test_that('grouped_rankings work w/ weights [beans]', {
    mod1 <- PlackettLuce(G, weights = weights)
    mod2 <- PlackettLuce(R, weights =rep(weights, 4))
    # iter can be different on some platform due to small difference in rowsums
    nm <- setdiff(names(mod1), c("call", "ranker", "iter"))
    expect_equal(mod1[nm], mod2[nm])
})

test_that('itempar.pltree works w/ weights [beans]',
          {
              # same results with newdata as original data
              pred1 <- predict(pl_tree, newdata = beans)
              pred2 <- predict(pl_tree)
              expect_equal(pred1, pred2)

              itempar1 <- unique(pred2)
              itempar2 <- itempar(pl_tree)
              # make sure nodes ordered the same way around
              expect_equal(itempar1[order(itempar1[,1]),],
                           itempar2[order(itempar2[,1]),],
                           ignore_attr = TRUE)
          })

test_that('AIC.pltree works w/ weights [beans]',
          {
              beans$G <- G
              aic1 <- AIC(pl_tree)
              aic2 <- AIC(pl_tree, newdata = beans, weights = weights)
              expect_equal(aic1, aic2)

              node <- predict(pl_tree, type = "node")
              id <- node == "3"
              aic1 <- AIC(pl_tree, newdata = beans[id, -1],
                          weights = weights[id])
              aic2 <- -2*as.numeric(logLik(pl_tree[[3]])) +
                  2*attr(logLik(pl_tree), "df")
              expect_equal(aic1, aic2)
          })

pl_tree1 <- pltree(G ~ maxTN, data = beans, alpha = 0)

test_that('itempar.pltree works w/ single node [beans]',  {
    # same results with newdata as original data
    pred1 <- predict(pl_tree1, newdata = beans)
    pred2 <- predict(pl_tree1)
    expect_equal(pred1, pred2)

    itempar1 <- unique(pred2)
    itempar2 <- itempar(pl_tree1)
    expect_equal(itempar1, itempar2, ignore_attr = TRUE)
})

test_that('AIC.pltree works w/ single node [beans]',
          {
              beans$G <- G
              aic1 <- AIC(pl_tree1)
              aic2 <- AIC(pl_tree1, newdata = beans)
              expect_equal(aic1, aic2)
          })

test_that('pltree works w/ estimated adherence [beans]', {
    pl_tree <- pltree(G ~ season,
                      data = beans, alpha = 0.05,
                      gamma = list(shape = 10, rate = 10))
    expect_snapshot_output(print(pl_tree))
})

test_that('pltree fails w/ fixed adherence [beans]', {
    expect_error(pltree(G ~ season,
                        data = beans, alpha = 0.05,
                        adherence = seq(0.75, 1.25, length.out = length(G))))
})
