# Create an Adjacency Matrix for a set of Rankings

Convert a set of rankings to an adjacency matrix summarising wins and
losses between pairs of items.

## Usage

``` r
adjacency(object, weights = NULL, ...)
```

## Arguments

- object:

  a
  [`rankings`](https://hturner.github.io/PlackettLuce/reference/rankings.md)
  object, or an object that can be coerced by `as.rankings`.

- weights:

  an optional vector of weights for the rankings.

- ...:

  further arguments passed to/from methods.

## Value

An N by N matrix, where N is the number of items that can be ranked.

## Details

For a `"rankings"` object based on N items, the adjacency matrix is an N
by N matrix, with element (i, j) being the number of times item i wins
over item j. For example, in the ranking {1} \> {3, 4} \> {2}, item 1
wins over items 2, 3, and 4, and items 3 and 4 win over item 2.

If `weights` is specified, the values in the adjacency matrix are the
weighted counts.

## Examples

``` r
X <- matrix(c(2, 1, 2, 1, 2,
              3, 2, 0, 0, 1,
              1, 0, 2, 2, 3), nrow = 3, byrow = TRUE)
X <- as.rankings(X)
adjacency(X)
#>   1 2 3 4 5
#> 1 0 0 1 1 1
#> 2 2 0 1 0 1
#> 3 0 0 0 0 1
#> 4 1 0 1 0 2
#> 5 1 1 0 0 0
#> attr(,"class")
#> [1] "adjacency" "matrix"   

adjacency(X, weights = c(1, 1, 2))
#>   1 2 3 4 5
#> 1 0 0 2 2 2
#> 2 2 0 1 0 1
#> 3 0 0 0 0 2
#> 4 1 0 1 0 3
#> 5 1 1 0 0 0
#> attr(,"class")
#> [1] "adjacency" "matrix"   
```
