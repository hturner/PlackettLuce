# Complete Orderings with the Missing Redundant Rank

Given orderings with one rank missing, complete the ordering by
assigning the remaining item(s) to the final rank.

## Usage

``` r
complete(orderings, items)
```

## Arguments

- orderings:

  A data frame of orderings with one rank missing.

- items:

  A vector of item names.

## Value

A vector of the missing items, which will be a list if there are any
ties.

## Examples

``` r
# Orderings of 3 items, when only the best and worst are recorded
orderings <- data.frame(best = c("A", "B", "A"),
                        worst = c("C", "C", NA))
orderings$middle <- complete(orderings, items = c("A", "B", "C"))
```
