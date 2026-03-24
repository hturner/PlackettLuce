# Results from 2002 NASCAR Season

This is an example dataset from Hunter 2004 recording the results of 36
car races in the 2002 NASCAR season in the United States. Each record is
an ordering of the drivers according to their finishing position.

## Usage

``` r
nascar
```

## Format

A matrix with 36 rows corresponding to the races and 43 columns
corresponding to the positions. The columns contain the ID for the
driver that came first to last place respectively. The `"drivers"`
attribute contains the names of the 87 drivers.

## References

Hunter, D. R. (2004) MM algorithms for generalized Bradley-Terry models.
*The Annals of Statistics*, **32(1)**, 384–406.

## Examples

``` r
# convert orderings to rankings
nascar[1:2, ]
#>      rank1 rank2 rank3 rank4 rank5 rank6 rank7 rank8 rank9 rank10 rank11 rank12
#> [1,]    83    18    20    48    53    51    67    72    32     42      2     31
#> [2,]    52    72     4    82    60    31    32    66     3     44      2     48
#>      rank13 rank14 rank15 rank16 rank17 rank18 rank19 rank20 rank21 rank22
#> [1,]     62     13     37      6     60     66     33     77     56     63
#> [2,]     83     67     41     77     33     61     45     38     51     14
#>      rank23 rank24 rank25 rank26 rank27 rank28 rank29 rank30 rank31 rank32
#> [1,]     55     70     14     43     71     35     12     44     79      3
#> [2,]     42     62     35     12     25     37     34      6     18     79
#>      rank33 rank34 rank35 rank36 rank37 rank38 rank39 rank40 rank41 rank42
#> [1,]     52      4      9     45     41     61     34     39     49     15
#> [2,]     39     59     43     55     49     56      9     53      7     13
#>      rank43
#> [1,]     82
#> [2,]     71
R <- as.rankings(nascar, input = "orderings",
                 items = attr(nascar, "drivers"))
R[1:2, 1:4, as.rankings = FALSE]
#>      Austin Cameron Bill Elliott Bobby Hamilton Bobby Labonte
#> [1,]              0           11             32            34
#> [2,]              0           11              9             3
format(R[1:2], width = 60)
#> [1] "Ward Burton > Elliott Sadler > Geoffrey Bodine > Kurt Bu ..."
#> [2] "Matt Kenseth > Sterling Marlin > Bobby Labonte > Tony St ..."

# fit model as in Hunter 2004, excluding drivers that only lose
keep <- seq_len(83)
R2 <- R[, keep]
mod <- PlackettLuce(R2, npseudo = 0)

# show coefficients as in Table 2 of Hunter 2004
avRank <- apply(R, 2, function(x) mean(x[x > 0]))
coefs <- round(coef(mod)[order(avRank[keep])], 2)
head(coefs, 3)
#>     PJ Jones Scott Pruett  Mark Martin 
#>         4.15         3.62         2.08 
tail(coefs, 3)
#>  Dave Marcis Dick Trickle    Joe Varde 
#>         0.03        -0.31        -0.15 
```
