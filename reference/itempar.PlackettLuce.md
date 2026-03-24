# Extract Item Parameters of Plackett-Luce Models

Methods for
[`itempar`](https://rdrr.io/pkg/psychotools/man/itempar.html) to extract
the item parameters (worth or log-worth) from a Plackett-Luce model or
tree. In the case of a tree, item parameters are extracted for each
terminal node.

## Usage

``` r
# S3 method for class 'PlackettLuce'
itempar(object, ref = NULL, alias = TRUE, vcov = TRUE, log = FALSE, ...)

# S3 method for class 'pltree'
itempar(object, ...)

# S3 method for class 'PLADMM'
itempar(object, ref = NULL, alias = TRUE, vcov = TRUE, log = FALSE, ...)
```

## Arguments

- object:

  a fitted model object as returned by
  [`PlackettLuce`](https://hturner.github.io/PlackettLuce/reference/PlackettLuce.md),
  [`pladmm`](https://hturner.github.io/PlackettLuce/reference/pladmm.md),
  or
  [`pltree`](https://hturner.github.io/PlackettLuce/reference/pltree.md).

- ref:

  a vector of labels or position indices of item parameters which should
  be used as restriction/for normalization. If `NULL` (the default), all
  items are used with a zero sum (`log = TRUE`) or unit sum
  (`log = FALSE`) constraint.

- alias:

  logical. If `TRUE` (the default), the aliased parameter is included in
  the return vector (and in the variance-covariance matrix if
  `vcov = TRUE`). If `FALSE`, it is removed. If the restriction given in
  ref depends on several parameters, the first parameter of the
  restriction specified is (arbitrarily) chosen to be removed if alias
  is `FALSE`.

- vcov:

  logical. If `TRUE` (the default), the (transformed)
  variance-covariance matrix of the item parameters is attached as
  attribute `vcov`. If `FALSE`, a `NA`-matrix is attached.

- log:

  logical. Whether to return log-abilities (`TRUE`) or abilities
  (`FALSE`).

- ...:

  further arguments which are currently not used.

## Value

An object of class `"itempar"`, see
[`itempar`](https://rdrr.io/pkg/psychotools/man/itempar.html).

## Examples

``` r
R <- matrix(c(1, 2, 0, 0,
              4, 1, 2, 3,
              2, 1, 1, 1,
              1, 2, 3, 0,
              2, 1, 1, 0,
              1, 0, 3, 2), nrow = 6, byrow = TRUE)
colnames(R) <- c("apple", "banana", "orange", "pear")

mod <- PlackettLuce(R)
coef(mod)
#>       apple      banana      orange        pear        tie2        tie3 
#>  0.00000000  0.25287379 -0.61350684 -0.08688475 -2.15068111 -0.79245358 

# equivalent to default coefficients, i.e. log abilities
itempar(mod, ref= 1, log = TRUE)
#> Item response item parameters (PlackettLuce):
#>    apple   banana   orange     pear 
#>  0.00000  0.25287 -0.61351 -0.08688 

# abilities, normalized so abilities for apple and pear sum to 1
itempar(mod, ref = 1:2)
#> Item response item parameters (PlackettLuce):
#>  apple banana orange   pear 
#> 0.4371 0.5629 0.2367 0.4007 
```
