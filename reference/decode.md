# Decode Orderings using a Key to Item Names

Decode orderings by replacing numeric or character coded values with
item names.

## Usage

``` r
decode(orderings, items, code = NULL)
```

## Arguments

- orderings:

  A data frame of coded orderings.

- items:

  A data frame of the items in each ranking, or a vector of common
  items.

- code:

  (Optional) a vector giving the key to the code. If missing,
  `names(items)` is used for a character code, while `seq(items)` is
  used for a numeric code.

## Value

A data frame with the coded values replaced by the item names.

## Examples

``` r
# orderings of up to 3 items coded as A, B, C
orderings <- data.frame(Rank1 = c("A", "B"),
                        Rank2 = c("C", "A"),
                        Rank3 = c("B", NA),
                        stringsAsFactors = FALSE)
items <- data.frame(A = c("banana", "apple"),
                    B = c("orange", "pear"),
                    C = c("apple", NA),
                    stringsAsFactors = FALSE)
decode(orderings, items)
#>    Rank1 Rank2  Rank3
#> 1 banana apple orange
#> 2   pear apple   <NA>

# orderings with ties of up to 3 items, coded 1:3
orderings <- data.frame(Rank1 = c(1, 3),
                        Rank2 = I(list(c(2, 3), 2)),
                        Rank3 = c(NA, 1),
                        stringsAsFactors = FALSE)
items <- data.frame(A = c("banana", "apple"),
                    B = c("orange", "pear"),
                    C = c("apple", "orange"),
                    stringsAsFactors = FALSE)
decode(orderings, items)
#>    Rank1         Rank2 Rank3
#> 1 banana orange, apple  <NA>
#> 2 orange          pear apple

# same items in each comparison
items <- c(A = "banana", B = "orange", C = "pear")
decode(orderings, items)
#>    Rank1        Rank2  Rank3
#> 1 banana orange, pear   <NA>
#> 2   pear       orange banana
```
