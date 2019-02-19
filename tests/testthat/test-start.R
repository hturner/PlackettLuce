context("implementation [start argument in PlackettLuce]")

coef_tol <- 1e-06
loglik_tol <- 1e-07

## artificial data
R <- matrix(c(1, 2, 0, 0,
              4, 1, 2, 3,
              2, 1, 1, 1,
              2, 1, 0, 0,
              1, 2, 3, 0,
              2, 1, 1, 0,
              1, 0, 3, 2), ncol = 4, byrow = TRUE)
colnames(R) <- c("apple", "banana", "orange", "pear")

mod <- PlackettLuce(R, npseudo = 0)
mod2 <- PlackettLuce(R, start = coef(mod, log = FALSE), npseudo = 0)
mod3 <- PlackettLuce(R, start = coef(mod), npseudo = 0)
mod4 <- PlackettLuce(R, start = unclass(coef(mod, log = FALSE)), npseudo = 0)

test_that("starting at solution returns same solution [npseudo = 0]",
          {
              expect_equal(mod2$iter, 0)
              expect_equal(coef(mod), coef(mod2))

              expect_equal(mod3$iter, 0)
              expect_equal(coef(mod), coef(mod3))

              # in this case const will change as starting values not rescaled
              expect_equal(mod4$iter, 0)
              expect_equivalent(coef(mod), coef(mod4), tol = coef_tol)
          })

mod <- PlackettLuce(R, npseudo = 0.5)
mod2 <- PlackettLuce(R, start = coef(mod, log = FALSE), npseudo = 0.5)
mod3 <- PlackettLuce(R, start = coef(mod), npseudo = 0.5)
mod4 <- PlackettLuce(R, start = unclass(coef(mod, log = FALSE)), npseudo = 0.5)
# suppress warning that iterations have not converged
mod5 <- suppressWarnings(PlackettLuce(R,
                                      start = unclass(coef(mod, log = FALSE)),
                                      npseudo = 0.5,
                                      maxit = 0))

test_that("starting at solution returns same solution [npseudo = 0.5]",
          {
              expect_equal(mod2$iter, 0)
              expect_equal(coef(mod), coef(mod2))

              expect_equal(mod3$iter, 0)
              expect_equal(coef(mod), coef(mod3))

              # in this case generic start for hypothetical item
              # => will iterate a little, to equivalent answer
              expect_gt(mod4$iter, 0)
              expect_equivalent(coef(mod), coef(mod4), tol = coef_tol)

              # here prevent further iterations so fix at starting val
              expect_equal(mod5$iter, 0)
              expect_equivalent(coef(mod), coef(mod5))
          })

if (require(psychotree) & require(sandwich)){
    data("Topmodel2007", package = "psychotree")
    preference <- Topmodel2007$preference

    G <- as.grouped_rankings(preference)
    plmod <- plfit(G, npseudo = 0, ref = "Anja", estfun = TRUE, object = TRUE)

    plmod2 <- plfit(G, npseudo = 0, ref = "Anja", estfun = TRUE, object = TRUE,
                    start = coef(plmod))

    test_that("plfit works with start [Topmodel2007]",
              {
                  expect_equal(plmod2$object$iter, 0)
                  expect_equal(coef(plmod), coef(plmod2))
              })

    plmod <- plfit(G, npseudo = 0.5, ref = "Anja", estfun = TRUE, object = TRUE)

    plmod2 <- plfit(G, npseudo = 0.5, ref = "Anja", estfun = TRUE,
                    object = TRUE, start = coef(plmod))

    test_that("plfit works with start, npseudo > 0 [Topmodel2007]",
              {
                  expect_equal(plmod2$object$iter, 0)
                  expect_equal(coef(plmod), coef(plmod2))
              })
}

