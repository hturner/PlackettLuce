# Fitted Probabilities for PlackettLuce Objects

Fitted probabilities for all choice/alternative combinations in the
data.

## Usage

``` r
# S3 method for class 'PlackettLuce'
fitted(object, aggregate = TRUE, free = TRUE, ...)

# S3 method for class 'pltree'
fitted(object, aggregate = TRUE, free = TRUE, ...)
```

## Arguments

- object:

  an object as returned by
  [`PlackettLuce`](https://hturner.github.io/PlackettLuce/reference/PlackettLuce.md)
  or
  [`pltree`](https://hturner.github.io/PlackettLuce/reference/pltree.md).

- aggregate:

  logical; if `TRUE` observations of the same choice from the same set
  of alternatives are aggregated.

- free:

  logical; if `TRUE` only free choices are included, i.e. choices of one
  item from a set of one item are excluded.

- ...:

  further arguments, currently ignored.

## Value

A list with the following components

- choices:

  The selected item(s).

- alternatives:

  The set of item(s) that the choice was made from.

- ranking:

  The ranking(s) including this choice.

- n:

  The weighted count of rankings including this choice (equal to the
  ranking weight if `aggregate = FALSE`.

- fitted:

  The fitted probability of making this choice.

If `object` was a `"pltree"` object, the list has an additional element,
`node`, specifying which node the ranking corresponds to.

## See also

[`choices`](https://hturner.github.io/PlackettLuce/reference/choices.md)

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
fit <- fitted(mod)
fit
#>    choices alternatives ranking     fitted n
#> 1        1         1, 2       1 0.41325484 1
#> 2        2   1, 2, 3, 4       2 0.21428983 1
#> 3        3      1, 3, 4       2 0.17489657 1
#> 4        4         1, 4       2 0.45200924 1
#> 5  2, 3, 4   1, 2, 3, 4       3 0.06489882 1
#> 6        1      1, 2, 3       4 0.28204215 1
#> 7        2         2, 3       4 0.66847071 1
#> 8     2, 3      1, 2, 3       5 0.02741414 1
#> 9        1      1, 3, 4       6 0.32301592 1
#> 10       4         3, 4       6 0.59521895 1
```
