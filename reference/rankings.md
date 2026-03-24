# Rankings Object

Create a `"rankings"` object from data or convert a matrix of rankings
or ordered items to a `"rankings"` object.

## Usage

``` r
rankings(data, id, item, rank, aggregate = FALSE, verbose = TRUE, ...)

as.rankings(x, ..., verbose = TRUE)

# Default S3 method
as.rankings(
  x,
  input = c("rankings", "orderings"),
  freq = NULL,
  index = NULL,
  aggregate = FALSE,
  items = NULL,
  labels = NULL,
  ...,
  verbose = TRUE
)

# S3 method for class 'grouped_rankings'
as.rankings(x, ..., verbose = TRUE)

# S3 method for class 'matrix'
as.rankings(
  x,
  input = c("rankings", "orderings"),
  freq = NULL,
  index = NULL,
  aggregate = FALSE,
  items = NULL,
  labels = NULL,
  ...,
  verbose = TRUE
)

# S3 method for class 'rankings'
x[i, j, ..., drop = TRUE, as.rankings = TRUE]

# S3 method for class 'rankings'
format(x, width = 40L, ...)
```

## Arguments

- data:

  a data frame with columns specified by `id`, `item` and `rank`.

- id:

  an index of `data` specifying the column containing ranking IDs.

- item:

  an index of `data` specifying the column containing item IDs,

- rank:

  an index of `data` specifying the column containing item ranks.

- aggregate:

  if `TRUE`, aggregate the rankings via
  [`aggregate()`](https://hturner.github.io/PlackettLuce/reference/aggregate.md)
  before returning.

- verbose:

  logical; if `TRUE` print messages when changes are made to rankings
  data.

- ...:

  further arguments passed to/from methods.

- x:

  for `as.rankings`, a matrix with one column per item and one row per
  ranking, or an object that can be coerced to such as matrix; for `[`
  and `format`, a `"rankings"` object.

- input:

  for `as.rankings`, whether rows in the input matrix contain numeric
  `"rankings"` (dense, standard/modified competition or fractional
  rankings) or `"orderings"`, i.e. the items ordered by rank.

- freq:

  an optional column index (number, character or logical) specifying a
  column of `x` that holds ranking frequencies, or a vector of ranking
  frequencies. If provided, an `"aggregated_rankings"` object will be
  returned.

- index:

  an optional column index (number, character or logical) specifying a
  column of `x` that holds a grouping index, or a numeric vector to for
  grouping. If provided, the rankings will be grouped by
  [`group()`](https://hturner.github.io/PlackettLuce/reference/group.md)
  before returning.

- items:

  for `input = "orderings"`, a character vector specifying the full set
  of items. Values in `x` are matched to this by value (if character) or
  position (if numeric). Use
  [`decode()`](https://hturner.github.io/PlackettLuce/reference/decode.md)
  for orderings requiring more complex decoding.

- labels:

  for `input = "orderings"` an optional vector of labels for the items,
  corresponding to the sorted unique values of `x`.

- i:

  indices specifying rankings to extract, as for
  [`[`](https://rdrr.io/r/base/Extract.html).

- j:

  indices specifying items to extract, as for
  [`[`](https://rdrr.io/r/base/Extract.html).

- drop:

  if `TRUE` return single row/column matrices as a vector.

- as.rankings:

  if `TRUE` return a rankings object, otherwise return a matrix/vector.

- width:

  the width in number of characters to format each ranking - rankings
  that are too wide will be truncated.

## Value

By default, a `"rankings"` object, which is a matrix of dense rankings
with methods for several generics including
[`aggregate`](https://hturner.github.io/PlackettLuce/reference/aggregate.md),
`[`, `format`, [`rbind()`](https://rdrr.io/r/base/cbind.html) and
[`as.matrix()`](https://rdrr.io/r/base/matrix.html).

If the object is created with `aggregate = TRUE`, or ranking frequencies
are specified via `freq`, the rankings are post-processed to create an
`"aggregated_rankings"` object.

If a group index is specified via `index`, the (possibly aggregated)
rankings are post-processed to create a `"grouped_rankings"` object.

## Details

Each ranking in the input data will be converted to a dense ranking,
which rank items from 1 (first place) to \\n_r\\ (last place). Items not
ranked should have a rank of 0 or `NA`. Tied items are given the same
rank with no rank skipped. For example {1, 0, 2, 1}, ranks the first and
fourth items in first place and the third item in second place; the
second item is unranked.

Records in `data` with missing `id` or `item` are dropped. Duplicated
items in the rankings are resolved if possible: redundant or
inconsistent ranks are set to `NA`. Rankings with only 1 item are set to
`NA` (rankings with zero items are automatically treated as `NA`). Any
issues causing records to be removed or recoded produce a message if
`verbose = TRUE`.

For `as.rankings` with `input = "orderings"`, unused ranks may be filled
with zeroes for numeric `x` or `NA`. It is only necessary to have as
many columns as ranks that are used.

The method for `[` will return a reduced rankings object by default,
recoding as dense rankings and setting invalid rankings to `NA` as
necessary. To extract rows and/or columns of the rankings as a matrix or
vector, set `as.rankings = FALSE`, see examples.

## Examples

``` r
# create rankings from data in long form

# example long form data
x <- data.frame(ranking = c(rep(1:4, each = 4), 5, 5, 5),
                letter = c(LETTERS[c(1:3, 3, 1:4, 2:5, 1:2, 1)], NA,
                           LETTERS[3:5]),
                rank = c(4:1, rep(NA, 4), 3:4, NA, NA, 1, 3, 4, 2, 2, 2, 3))

# ranking 1 has different rank for same item, but order of items unambiguous
# all ranks are missing in ranking 2
# some ranks are missing in ranking 3
# ranking 4 has inconsistent ranks for two items and a rank with missing item
# ranking 5 is fine - an example of a tie
split(x, x$ranking)
#> $`1`
#>   ranking letter rank
#> 1       1      A    4
#> 2       1      B    3
#> 3       1      C    2
#> 4       1      C    1
#> 
#> $`2`
#>   ranking letter rank
#> 5       2      A   NA
#> 6       2      B   NA
#> 7       2      C   NA
#> 8       2      D   NA
#> 
#> $`3`
#>    ranking letter rank
#> 9        3      B    3
#> 10       3      C    4
#> 11       3      D   NA
#> 12       3      E   NA
#> 
#> $`4`
#>    ranking letter rank
#> 13       4      A    1
#> 14       4      B    3
#> 15       4      A    4
#> 16       4   <NA>    2
#> 
#> $`5`
#>    ranking letter rank
#> 17       5      C    2
#> 18       5      D    2
#> 19       5      E    3
#> 

# fix issues when creating rankings object
rankings(x, id = "ranking", item = "letter", rank = "rank")
#> Removed records with unknown id or item
#> Duplicated items within rankings: set redundant/inconsistent to `NA`.
#> Recoded rankings that are not in dense form
#> Rankings with only 1 item set to `NA`
#>           1           2           3           4           5 
#>     "B > A"          NA     "B > C"          NA "C = D > E" 

# convert existing matrix of rankings

R <- matrix(c(1, 2, 0, 0,
              4, 1, 2, 3,
              2, 1, 1, 1,
              1, 2, 3, 0,
              2, 1, 1, 0,
              1, 0, 3, 2), nrow = 6, byrow = TRUE)
colnames(R) <- c("apple", "banana", "orange", "pear")
R <- as.rankings(R)

# first three rankings
R[1:3,]
#> [1] "apple > banana"                 "banana > orange > pear > apple"
#> [3] "banana = orange = pear > apple"

# exclude pear from the rankings
R[, -4]
#> Recoded rankings that are not in dense form
#> [1] "apple > banana"          "banana > orange > apple"
#> [3] "banana = orange > apple" "apple > banana > orange"
#> [5] "banana = orange > apple" "apple > orange"         

# extract rankings 2 and 3 as numeric matrix
R[2:3, , as.rankings = FALSE]
#>      apple banana orange pear
#> [1,]     4      1      2    3
#> [2,]     2      1      1    1

# same as
as.matrix(R)[2:3,]
#>      apple banana orange pear
#> [1,]     4      1      2    3
#> [2,]     2      1      1    1

# extract rankings for item 1 as a vector
R[,1, as.rankings = FALSE]
#> [1] 1 4 2 1 2 1
```
