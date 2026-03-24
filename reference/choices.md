# Choices Object

Convert a set of rankings to a list of choices, alternatives, and
rankings. The choices and the corresponding alternatives make up the
exchangeable part of the Plackett-Luce with ties.

## Usage

``` r
choices(rankings, names = FALSE)
```

## Arguments

- rankings:

  a
  `"`[`rankings`](https://hturner.github.io/PlackettLuce/reference/rankings.md)`"`
  object, or an object that can be coerced by `as.rankings`.

- names:

  logical: if `TRUE` use the object names in the returned `"choices"`
  object, else use object indices.

## Value

A data frame of class `"choices"` with elements:

- choices:

  A list where each element represents the set of items chosen for a
  single rank in the ranking.

- alternatives:

  A list where each element represents the set of items to choose from
  for a single rank in the ranking.

- ranking:

  A list where each element represents the ranking that the choice
  belongs to.

The list stores the number of choices and the names of the objects as
the attributes `"nchoices"` and `"objects"` respectively.

## Examples

``` r
R <- matrix(c(1, 2, 0, 0,
              4, 1, 2, 3,
              2, 1, 1, 1,
              1, 2, 3, 0,
              2, 1, 1, 0,
              1, 0, 3, 2), nrow = 6, byrow = TRUE)
colnames(R) <- c("apple", "banana", "orange", "pear")

actual_choices <- choices(R, names = TRUE)
actual_choices[1:6,]
#> Ranking: 1 
#> -------------- 
#> {apple} from {apple, banana} 
#> {banana} from {banana} 
#> ============== 
#> Ranking: 2 
#> -------------- 
#> {banana} from {apple, banana, orange, pear} 
#> {orange} from {apple, orange, pear} 
#> {pear} from {apple, pear} 
#> {apple} from {apple} 
#> ============== 

coded_choices <- choices(R, names = FALSE)
coded_choices[1:2,]
#> Ranking: 1 
#> -------------- 
#> {1} from {1, 2} 
#> {2} from {2} 
#> ============== 
as.data.frame(coded_choices)[1:2,]
#>   choices alternatives ranking
#> 1       1         1, 2       1
#> 2       2            2       1
attr(coded_choices, "objects")
#> [1] "apple"  "banana" "orange" "pear"  
```
