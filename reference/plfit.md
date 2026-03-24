# PlackettLuce Wrapper for Model-based Recursive Partitioning

This is a wrapper around `PlackettLuce` as required by
[`mob`](https://rdrr.io/pkg/partykit/man/mob.html) for model-based
recursive partitioning. It is not intended for general use.

## Usage

``` r
plfit(
  y,
  x = NULL,
  ref = 1L,
  start = NULL,
  weights = NULL,
  offset = NULL,
  ...,
  estfun = FALSE,
  object = FALSE
)
```

## Arguments

- y:

  a
  `"`[`grouped_rankings`](https://hturner.github.io/PlackettLuce/reference/PlackettLuce-deprecated.md)`"`
  object giving the rankings to model.

- x:

  unused.

- ref:

  An integer or character string specifying the reference item (for
  which log worth will be set to zero). If `NULL` the sum of the log
  worth parameters is set to zero.

- start:

  starting values for the worth parameters and the tie parameters on the
  raw scale (worth parameters need not be scaled to sum to 1). If
  `normal` is specified, `exp(normal$mu)` is used as starting values for
  the worth parameters. Coefficients from a previous fit can be passed
  as the result of a call to `coef.PlackettLuce`, or the `coefficients`
  element of a `"PlackettLuce"` object.

- weights:

  an optional vector of weights for each ranking.

- offset:

  unused.

- ...:

  additional arguments passed to `PlackettLuce`.

- estfun:

  logical. If `TRUE` the empirical estimating functions (score/gradient
  contributions) are returned.

- object:

  logical. If `TRUE` the fitted model is returned.

## Value

a list with elements

- coefficients:

  model coefficients.

- objfun:

  the negative log-likelihood.

- estfun:

  if `estfun` the empirical estimating functions.

- object:

  if `object` the fitted model.

## Examples

``` r
# rankings
R <- matrix(c(1, 2, 0, 0,
              4, 1, 2, 3,
              2, 1, 1, 1,
              1, 2, 3, 0,
              2, 1, 1, 0,
              1, 0, 3, 2), nrow = 6, byrow = TRUE)
colnames(R) <- c("apple", "banana", "orange", "pear")
R <- as.rankings(R)

# group rankings into two groups
G <- group(R, rep(1:2, 3))

# plfit() gives the same results as PlackettLuce()
pl <- plfit(G)
pl$coefficients
#>       apple      banana      orange        pear        tie2        tie3 
#>  0.00000000  0.25287379 -0.61350684 -0.08688475 -2.15068111 -0.79245358 
-pl$objfun
#> [1] -14.61069

mod <- PlackettLuce(R)
coef(mod)
#>       apple      banana      orange        pear        tie2        tie3 
#>  0.00000000  0.25287379 -0.61350684 -0.08688475 -2.15068111 -0.79245358 
logLik(mod)
#> 'log Lik.' -14.61069 (df=5)
```
