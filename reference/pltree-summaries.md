# Plackett-Luce Tree Summaries

Obtain the coefficients, variance-covariance matrix, AIC, or predictions
from a Plackett-Luce tree fitted by
[`pltree()`](https://hturner.github.io/PlackettLuce/reference/pltree.md).

## Usage

``` r
# S3 method for class 'pltree'
coef(object, node = NULL, drop = TRUE, ...)

# S3 method for class 'pltree'
vcov(object, node = nodeids(object, terminal = TRUE), ...)

# S3 method for class 'pltree'
AIC(object, newdata = NULL, ...)

# S3 method for class 'pltree'
predict(
  object,
  newdata = NULL,
  type = c("itempar", "rank", "best", "node"),
  ...
)
```

## Arguments

- object:

  a fitted model object of class `"pltree"`.

- node:

  a vector of node ids specifying the nodes to summarise, by default the
  ids of the terminal nodes.

- drop:

  if `TRUE` return the coefficients as a vector when only one node is
  selected.

- ...:

  additional arguments passed to
  [`itempar`](https://hturner.github.io/PlackettLuce/reference/itempar.PlackettLuce.md)
  by `predict`, and to
  [`model.frame`](https://rdrr.io/r/stats/model.frame.html) by `AIC`.

- newdata:

  an optional data frame to use instead of the original data. For `AIC`
  this must include the response variable.

- type:

  the type of prediction to return for each group, one of: `"itempar"`
  to give the result of
  [`itempar`](https://rdrr.io/pkg/psychotools/man/itempar.html) (by
  default the fitted probability of each item being ranked first out of
  all objects), `"rank"` the corresponding rank, `"best"` the topped
  ranked item, or `"node"` the node of the tree the group belongs to.

## Details

`AIC` computes \\-2L + 2p\\ where \\L\\ is the joint likelihood of the
observed rankings under the tree model and \\p\\ is the degrees of
freedom used to fit the tree model.

## Examples

``` r
data(beans)
# fit tree based on pairwise comparisons with variety B
pairB <- data.frame(Winner = ifelse(beans$var_b == "Worse",
                                    "Local", beans$variety_b),
                    Loser = ifelse(beans$var_b == "Worse",
                                   beans$variety_b, "Local"),
                    stringsAsFactors = FALSE, row.names = NULL)
beans$G <- as.rankings(pairB, input = "orderings",
                       index = rep(seq(nrow(beans)), 1))

mod <- pltree(G ~ ., data = beans[c("G", "maxTN")])

coef(mod, node = 3)
#>      ALS 0532-6     BRT 103-182 INTA Centro Sur    INTA Ferroso  INTA Matagalpa 
#>      0.00000000     -0.07161137      0.14990568     -0.16583159     -0.48272277 
#>     INTA Precoz       INTA Rojo     INTA Sequia           Local     PM2 Don Rey 
#>     -0.03697466     -0.68305444     -0.43665252     -0.31973562      0.54722786 
#>      SJC 730-79 
#>     -0.37766352 
AIC(mod)
#> [1] 1123.72

# treat first row from each year as new data
newdata <- beans[!duplicated(beans$year),]

## fitted probabilities
predict(mod, newdata)
#>     ALS 0532-6 BRT 103-182 INTA Centro Sur INTA Ferroso INTA Matagalpa
#> 1   0.10191465  0.09487159      0.11839676   0.08634096     0.06289162
#> 482 0.09663466  0.09072365      0.04497423   0.09188975     0.09950195
#>     INTA Precoz  INTA Rojo INTA Sequia      Local PM2 Don Rey SJC 730-79
#> 1    0.09821520 0.05147423  0.06585683 0.07402479  0.17615486 0.06985853
#> 482  0.08239922 0.08743950  0.13071479 0.16470103  0.04214223 0.06887900

## fitted log-abilities, with Local as reference
predict(mod, newdata, log = TRUE, ref = "Local")
#>     ALS 0532-6 BRT 103-182 INTA Centro Sur INTA Ferroso INTA Matagalpa
#> 1    0.3197356   0.2481243       0.4696413    0.1539040     -0.1629871
#> 482 -0.5331945  -0.5963139      -1.2980422   -0.5835425     -0.5039546
#>     INTA Precoz  INTA Rojo INTA Sequia Local PM2 Don Rey SJC 730-79
#> 1     0.2827610 -0.3633188  -0.1169169     0   0.8669635 -0.0579279
#> 482  -0.6925559 -0.6331848  -0.2311141     0  -1.3630816 -0.8717806

## variety ranks
predict(mod, newdata, type = "rank")
#>     ALS 0532-6 BRT 103-182 INTA Centro Sur INTA Ferroso INTA Matagalpa
#> 1            3           5               2            6             10
#> 482          4           6              10            5              3
#>     INTA Precoz INTA Rojo INTA Sequia Local PM2 Don Rey SJC 730-79
#> 1             4        11           9     7           1          8
#> 482           8         7           2     1          11          9

## top ranked variety
predict(mod, newdata, type = "best")
#>             1           482 
#> "PM2 Don Rey"       "Local" 

## node the trial belongs to
predict(mod, newdata, type = "node")
#>   1   2 
#> "3" "2" 
```
