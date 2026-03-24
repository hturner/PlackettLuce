# Aggregate Rankings

Aggregate rankings, returning an `"aggregated_rankings"` object of the
unique rankings and their frequencies. The frequencies can be extracted
via the function `freq()`.

## Usage

``` r
# S3 method for class 'rankings'
aggregate(x, freq = NULL, ...)

as.aggregated_rankings(x, ...)

# S3 method for class 'aggregated_rankings'
x[i, j, ..., drop = FALSE, as.aggregated_rankings = TRUE]

freq(x)
```

## Arguments

- x:

  A
  [`"rankings"`](https://hturner.github.io/PlackettLuce/reference/rankings.md)
  object for `aggregate()`; an object that can be coerced to a
  `"aggregated_rankings"` object for `as.aggregated_rankings()`,
  otherwise an `"aggregated_rankings"` object.

- freq:

  A vector of frequencies for rankings that have been previously
  aggregated.

- ...:

  Additional arguments, currently unused.

- i:

  indices specifying rankings to extract, as for
  [`[`](https://rdrr.io/r/base/Extract.html).

- j:

  indices specifying items to extract, as for
  [`[`](https://rdrr.io/r/base/Extract.html).

- drop:

  if `TRUE` return single row/column matrices as a vector.

- as.aggregated_rankings:

  if `TRUE` create an `"aggregated_rankings"` object from the indexed
  rankings. Otherwise index the underlying matrix of ranks and return in
  a data frame with the corresponding frequencies.

## Value

A data frame of class `"aggregated_rankings"`, with columns

- ranking:

  A
  [`"rankings"`](https://hturner.github.io/PlackettLuce/reference/rankings.md)
  object of the unique rankings.

- freq:

  The corresponding frequencies.

Methods are available for [`rbind()`](https://rdrr.io/r/base/cbind.html)
and [`as.matrix()`](https://rdrr.io/r/base/matrix.html).

## See also

[`preflib()`](https://hturner.github.io/PlackettLuce/reference/preflib.md)
for an object that can be coerced to an `"aggregated_rankings"` object.

## Examples

``` r
# create a rankings object with duplicated rankings
R <- matrix(c(1, 2, 0, 0,
              0, 1, 2, 3,
              2, 1, 1, 0,
              1, 2, 0, 0,
              2, 1, 1, 0,
              1, 0, 3, 2), nrow = 6, byrow = TRUE)
colnames(R) <- c("apple", "banana", "orange", "pear")
R <- as.rankings(R)

# aggregate the rankings
A <- aggregate(R)

# subsetting applies to the rankings, e.g. first two unique rankings
A[1:2]
#>                  ranking freq
#> 1         apple > banana    2
#> 2 banana > orange > pear    1

# (partial) rankings of items 2 to 4 only
A[, 2:4]
#> Recoded rankings that are not in dense form
#> Rankings with only 1 item set to `NA`
#>                  ranking freq
#> 1                   <NA>    2
#> 2 banana > orange > pear    1
#> 3        banana = orange    2
#> 4          pear > orange    1

# convert to a matrix
as.matrix(A)
#>      apple banana orange pear freq
#> [1,]     1      2      0    0    2
#> [2,]     0      1      2    3    1
#> [3,]     2      1      1    0    2
#> [4,]     1      0      3    2    1

# frequencies are automatically used as weights by PlackettLuce()
mod <- PlackettLuce(A)
mod$weights
#> [1] 2 1 2 1
```
