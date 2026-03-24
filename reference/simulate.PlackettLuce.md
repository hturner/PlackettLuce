# Simulate from `PlackettLuce` fitted objects

Simulate from `PlackettLuce` fitted objects

## Usage

``` r
# S3 method for class 'PlackettLuce'
simulate(
  object,
  nsim = 1L,
  seed = NULL,
  multinomial = FALSE,
  max_combinations = 20000,
  ...
)
```

## Arguments

- object:

  an object representing a fitted model.

- nsim:

  number of response vectors to simulate. Defaults to `1`.

- seed:

  an object specifying if and how the random number generator should be
  initialised. Either `NULL` or an integer that will be used in a call
  to `set.seed` before simulating the rankings. If set, the value is
  saved as the `seed` attribute of the returned value. The default,
  `NULL`, will not change the random generator state, and return
  `.Random.seed` as the `seed` attribute.

- multinomial:

  use multinomial sampling anyway? Default is `FALSE`. see Details.

- max_combinations:

  a positive number. Default is `20000`. See Details.

- ...:

  additional optional arguments.

## Value

A `data.frame` of
[`rankings`](https://hturner.github.io/PlackettLuce/reference/rankings.md)
objects of the same dimension as `object$rankings`.

## Details

If `multinomial` is `FALSE` (default) and there are no tie parameters in
the object (i.e. `length(object$ties) == 1`), then rankings are sampled
by ordering exponential random variates with rate 1 scaled by the
estimated item-worth parameters `object$coefficients` (see, Diaconis,
1988, Chapter 9D for details).

In all other cases, the current implementation uses direct multinomial
sampling, and will throw an error if there are more than
`max_combinations` combinations of items that the sampler has to decide
from. This is a hard-coded exit to prevent issues relating to the
creation of massive objects in memory.

If `length(object$ties) > 1` the user's setting for `multinomial` is
ignored and `simulate.PlackettLuce` operates as if `multinomial` is
`TRUE`.

## References

Diaconis (1988). *Group Representations in Probability and Statistics*.
Institute of Mathematical Statistics Lecture Notes 11. Hayward, CA.

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
simulate(mod, 5)
#>                            sim_1                          sim_2
#> 1                 banana > apple                 banana > apple
#> 2 orange > banana > apple > pear apple = orange > banana > pear
#> 3 pear > banana > orange > apple apple = banana = orange > pear
#> 4        apple = banana = orange        apple = orange > banana
#> 5        banana > apple > orange        banana > apple > orange
#> 6          pear > apple > orange          orange > apple > pear
#>                            sim_3                          sim_4
#> 1                 banana > apple                 banana > apple
#> 2 pear > apple > orange > banana banana > pear > apple > orange
#> 3 banana > apple > orange > pear banana > orange > apple > pear
#> 4        apple > banana > orange        apple = banana = orange
#> 5        orange > apple > banana        banana > apple > orange
#> 6          apple = orange > pear          pear > apple > orange
#>                            sim_5
#> 1                 apple > banana
#> 2 apple > pear > banana > orange
#> 3 pear > apple > banana > orange
#> 4        apple > orange > banana
#> 5        apple > banana > orange
#> 6          apple > pear > orange

s1 <- simulate(mod, 3, seed = 112)
s2 <- simulate(mod, 2, seed = 112)

identical(s1[1:2], s2[1:2])
#> [1] TRUE
```
