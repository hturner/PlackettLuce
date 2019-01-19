context("implementation [log-likelihood and coefficients]")

## Get the legacy implementation
source_files <- dir(system.file("PlackettLuce0", package = "PlackettLuce"),
                    full.names = TRUE)

## N.B. fitted0 requires tibble but unused in tests
for (file0 in source_files) source(file0)

## Get reference implementations (Davidson, MM algorithms)
source_files <- dir(system.file("Reference_Implementations",
                                package = "PlackettLuce"),
                    full.names = TRUE)

for (file0 in source_files) source(file0)

coef_tol <- 1e-06
loglik_tol <- 1e-12

## Extractor for the log-likelihood of poisson gnm's adjusting for the -mu part
## which is fixed due to the parameter space restriction to match row totals
logLik_poisson.gnm <- function(x) {
    n <- nlevels(x$eliminate)
    ll <- logLik(x) + n
    attr(ll, "df") <- attr(ll, "df") - n
    ll
}


## The artificial example in ?PlackettLuce
R <- matrix(c(1, 2, 0, 0,
              4, 1, 2, 3,
              2, 1, 1, 1,
              1, 2, 3, 0,
              2, 1, 1, 0,
              1, 0, 3, 2), nrow = 6, byrow = TRUE)
colnames(R) <- c("apple", "banana", "orange", "pear")
R <- as.rankings(R)
if (require("Matrix") & requireNamespace("igraph") &
    requireNamespace("rARPACK")) {
    model0 <- PlackettLuce0(rankings = R)
    model1 <- PlackettLuce(rankings = R, npseudo = 0)
    test_that("coef match legacy code [fake partial rankings with ties]", {
        # coefficients
        expect_equal(as.vector(coef(model0)),
                     as.vector(coef(model1)), tolerance = coef_tol)
    })
    test_that("logLik matches legacy code [fake partial rankings with ties]", {
        # log-likelihood
        expect_equal(logLik(model0), logLik(model1), tolerance = loglik_tol)
    })
    ## fit null model via glm
    dat <- poisson_rankings(R, aggregate = FALSE, as.data.frame = TRUE)
    test_that("null deviance matches glm [fake partial rankings with ties]", {
        model2 <- glm(y ~ -1 + z, family = poisson, data = dat)
        ## null deviance
        expect_equal(-2*model1$null.loglik, model2$deviance)
        ## null df
        expect_equal(model1$df.null, model2$df.residual)
    })
}


## simple BT model
M <- matrix(c(1, 2,
              3, 1,
              1, 4,
              2, 1,
              2, 3,
              4, 3), nrow = 6, byrow = TRUE)
R <- as.rankings(M, "ordering")
mod1 <- PlackettLuce(R, npseudo = 0)
dat <- poisson_rankings(R, aggregate = FALSE, as.data.frame = TRUE)
if (require(gnm) & require(BradleyTerry2) &
    requireNamespace("BradleyTerryScalable")){
    ## fit loglinear model
    mod2 <- gnm(y ~ -1 + X, family = poisson, eliminate = z, data = dat,
                constrain = 1)
    BT_data <- data.frame(p1 = factor(M[,1]), p2 = factor(M[,2]))
    mod3 <- BTm(rep(1, 6), p1, p2, data = BT_data)
    cdat <- BradleyTerryScalable::btdata(as.data.frame(cbind(M, 1)))
    mod4 <- BradleyTerryScalable::btfit(cdat, 1, epsilon = 1e-7)
    pp <- mod4$pi[[1]]
    pp <- (pp/sum(pp))[cdat$components$`1`]
    pp <- log(pp) - log(pp)[1]
    test_that("estimates match gnm, BTm, btfit [fake paired comparisons]", {
        ## coefficients
        expect_equal(unname(coef(mod1)[-1]), unname(coef(mod2)[-1]),
                     tolerance = coef_tol)
        expect_equal(unname(coef(mod2)[-1]), unname(coef(mod3)),
                     tolerance = coef_tol)
        expect_equal(unname(coef(mod3)), unname(pp[-1]),
                     tolerance = coef_tol)
    })
    test_that("logLik matches gnm, BTm, btfit [fake paired comparisons]", {
        ## log-likelihood
        expect_equal(logLik(mod1), logLik(mod3), check.attributes = FALSE,
                     tolerance = 1e-12)
        expect_equal(attr(logLik(mod3), "df"),
                     attr(logLik_poisson.gnm(mod2), "df"))
        expect_equal(logLik(mod3), logLik_poisson.gnm(mod2),
                     check.attributes = FALSE,tolerance = 1e-12)
    })
    ## fit null model via glm
    mod5 <- glm(y ~ -1 + z, family = poisson, data = dat)
    test_that("null deviance matches glm, BTm [fake paired comparisons]", {
        ## null deviance
        expect_equal(-2*mod1$null.loglik, mod3$null.deviance)
        expect_equal(-2*mod1$null.loglik, mod5$deviance)
        ## null df
        expect_equal(mod1$df.null, mod3$df.null)
        expect_equal(mod1$df.null, mod5$df.residual)
    })
}


## Icehockey data
if (require(BradleyTerry2)){
    ## loglinear model with BTm
    icehockey2 <- subset(icehockey, result != 0.5) #remove ties
    mod_BT <- BTm(outcome = result,
                  player1 = visitor, player2 = opponent,
                  id = "team", data = icehockey2)
    ## compare to PlackettLuce
    R <- matrix(0, nrow = nrow(icehockey2),
                ncol = nlevels(icehockey2$visitor))
    R[cbind(seq_len(nrow(icehockey2)), icehockey2$visitor)] <-
        2 - icehockey2$result
    R[cbind(seq_len(nrow(icehockey2)), icehockey2$opponent)] <-
        icehockey2$result + 1
    ## needs 170 iterations
    mod_PL <- PlackettLuce(R, npseudo = 0)
    test_that("estimates match BTm [icehockey]", {
        expect_equal(unname(coef(mod_BT)), unname(coef(mod_PL))[-1],
                     tolerance = coef_tol)
    })
    test_that("log-likelihood matches BTm [icehockey]", {
        expect_equal(logLik(mod_BT), logLik(mod_PL), check.attributes = FALSE,
                     tolerance = loglik_tol)
        expect_equal(attr(logLik(mod_BT), "df"), attr(logLik(mod_PL), "df"))
    })
}


## partial rankings, no ties
M <- matrix(c(1, 2, 0, 0,
              3, 1, 4, 0,
              1, 4, 0, 0,
              2, 1, 4, 3,
              2, 3, 4, 0,
              1, 2, 3, 0), nrow = 6, byrow = TRUE)
## via Hunter's MM (reference implementation)
gamma <- PL(M)
lambda <- log(c(gamma/sum(gamma)))
lambda <- lambda - lambda[1]
## via PlackettLuce
R <- as.rankings(M, "ordering")
mod1 <- PlackettLuce(R, npseudo = 0)
if (require(gnm)){
    ## fit loglinear model
    dat <- dat <- poisson_rankings(R, aggregate = FALSE, as.data.frame = TRUE)
    mod2 <- gnm(y ~ -1 + X, family = poisson, eliminate = z, data = dat,
                constrain = 1)
    test_that("coef match Hunter's MM, gnm [fake partial rankings no ties]", {
        expect_equal(unname(coef(mod1))[-1], unname(coef(mod2))[-1],
                     tolerance = coef_tol)
        expect_equal(unname(c(coef(mod1))), lambda, tolerance = coef_tol)
    })
    test_that("logLik matches gnm [fake partial rankings no ties]",
              {
                  expect_equal(logLik(mod1), logLik_poisson.gnm(mod2),
                               check.attributes = FALSE, tolerance = loglik_tol)
              })
}


## Nascar example from Hunter
if (require(StatRank)){
    ## 36 races. Partial rankings of length (42 or 43), ranking 83 drivers in
    ## 1st to 83rd place (puts zero for 43rd or 44th to last place). No ties.
    data(Data.Nascar)
    ## StatRank PL function takes ~10min; not sure how to compare coef
    ## a <- Estimation.PL.MLE(Data.Nascar)$Mean
    ## via Hunter's MM (reference implementation)
    gamma <- PL(Data.Nascar)
    lambda <- log(c(gamma/sum(gamma)))
    lambda <- lambda - lambda[1]
    ## via PlackettLuce
    R <- as.rankings(Data.Nascar, input = "ordering")
    mod1 <- PlackettLuce(R, npseudo = 0)
    ## fairly quick - A. Cameron (driver with ID 1) is fixed at zero
    dat <- PlackettLuce:::poisson_rankings(R, aggregate = FALSE,
                                           as.data.frame = TRUE)
    mod2 <- gnm(y ~ -1 + X, family = poisson, eliminate = z, data = dat,
                constrain = 1)
    test_that("coef match Hunter's MM, gnm [nascar]", {
        expect_equal(unname(coef(mod1))[-1], unname(coef(mod2))[-1],
                     tolerance = coef_tol)
        expect_equal(unname(c(coef(mod1))), lambda, tolerance = coef_tol,
                     check.attributes = FALSE)
    })
    test_that("logLik matches gnm [nascar]",
              {
                  expect_equal(logLik(mod1), logLik_poisson.gnm(mod2),
                               check.attributes = FALSE, tolerance = loglik_tol)
              })
}

