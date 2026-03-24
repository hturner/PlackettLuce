# Group Rankings

Create an object of class `"grouped_rankings"` which associates a group
index with an object of class `"rankings"`. This allows the rankings to
be linked to covariates with group-specific values as the basis for
model-based recursive partitioning, see
[`pltree`](https://hturner.github.io/PlackettLuce/reference/pltree.md).

## Usage

``` r
group(x, index, ...)

as.grouped_rankings(x, ...)

# S3 method for class 'paircomp'
as.grouped_rankings(x, ...)

# S3 method for class 'grouped_rankings'
x[i, j, ..., drop = TRUE, as.grouped_rankings = TRUE]

# S3 method for class 'grouped_rankings'
format(x, max = 2L, width = 20L, ...)
```

## Arguments

- x:

  a
  [`"rankings"`](https://hturner.github.io/PlackettLuce/reference/rankings.md)
  object for `group()`; an object that can be coerced to a
  `"grouped_rankings"` object for `as.grouped_rankings()`, otherwise a
  `"grouped_rankings"` object.

- index:

  a numeric vector of length equal to the number of rankings specifying
  the subject for each ranking.

- ...:

  additional arguments passed on to
  [`as.rankings`](https://hturner.github.io/PlackettLuce/reference/rankings.md)
  by `grouped_rankings` or `as.grouped_rankings`; unused by `format`.

- i:

  indices specifying groups to extract, may be any data type accepted by
  [`[`](https://rdrr.io/r/base/Extract.html).

- j:

  indices specifying items to extract, as for
  [`[`](https://rdrr.io/r/base/Extract.html).

- drop:

  if `TRUE` return single row/column matrices as a vector.

- as.grouped_rankings:

  if `TRUE` return a grouped_rankings object, otherwise return a
  matrix/vector.

- max:

  the maximum number of rankings to format per subject.

- width:

  the maximum width in number of characters to format each ranking.

## Value

An object of class `"grouped_rankings"`, which is a vector of of group
IDs with the following attributes:

- rankings:

  The `"rankings"` object.

- index:

  An index match each ranking to each group ID.

- R:

  A matrix with items ordered from last to first place, for each
  ranking.

- S:

  The rankings matrix with the ranks replaced by the size of the chosen
  set for free choices and zero for forced choices.

- id:

  A list with elements of the adjacency matrix that are incremented by
  each ranking.

## See also

[`pltree`](https://hturner.github.io/PlackettLuce/reference/pltree.md)

## Examples

``` r
# ungrouped rankings (5 rankings, 4 items)
R <- as.rankings(matrix(c(1, 2, 0, 0,
                          0, 2, 1, 0,
                          0, 0, 1, 2,
                          2, 1, 0, 0,
                          0, 1, 2, 3), ncol = 4, byrow = TRUE))
length(R)
#> [1] 5
R
#> [1] "1 > 2"     "3 > 2"     "3 > 4"     "2 > 1"     "2 > 3 > 4"

# group rankings (first three in group 1, next two in group 2)
G <- group(R, c(1, 1, 1, 2, 2))
length(G)
#> [1] 2

## by default up to 2 rankings are shown per group, "..." indicates if
## there are further rankings
G
#>                   1                   2 
#> "1 > 2, 3 > 2, ..."  "2 > 1, 2 > 3 > 4" 
print(G, max = 1)
#>            1            2 
#> "1 > 2, ..." "2 > 1, ..." 

## select rankings from group 1
G[1,]
#>                   1 
#> "1 > 2, 3 > 2, ..." 

## exclude item 3 from ranking
G[, -3]
#> Recoded rankings that are not in dense form
#> Rankings with only 1 item set to `NA`
#>                1                2 
#> "1 > 2, NA, ..."   "2 > 1, 2 > 4" 

## rankings from group 2, excluding item 3
## - note group 2 becomes the first group
G[2, -3]
#> Recoded rankings that are not in dense form
#>              1 
#> "2 > 1, 2 > 4" 

## index underlying rankings without creating new grouped_rankings object
G[2, -3, as.grouped_rankings = FALSE]
#>      1 2 4
#> [1,] 2 1 0
#> [2,] 0 1 3
```
