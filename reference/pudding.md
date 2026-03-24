# Paired Comparisons of Chocolate Pudding

This is an example dataset from Davidson (1970) comprising paired
comparisons of chocolate pudding, with six brands in total. The
responses include tied outcomes, i.e. no preference.

## Usage

``` r
pudding
```

## Format

A data frame with 15 records and 6 variables:

- `i`:

  The first brand in the comparison.

- `j`:

  The second brand in the comparison.

- `r_ij`:

  The frequency of paired comparisons of brand i and brand j.

- `w_ij`:

  The frequency of preferences for i over j.

- `w_ji`:

  The frequency of preferences for j over i.

- `t_ij`:

  The frequency of no preference between i and j.

## References

Davidson, R. R. (1970). On extending the Bradley-Terry model to
accommodate ties in paired comparison experiments. *Journal of the
American Statistical Association*, **65**, 317–328.

## Examples

``` r
# create orderings for each set of paired comparisons

# wins for brand i and wins for brand j
i_wins <- data.frame(Winner = pudding$i, Loser = pudding$j)
j_wins <- data.frame(Winner = pudding$j, Loser = pudding$i)

# ties: use an array list (easier with R >= 3.6.0)
if (getRversion() < "3.6.0"){
  n <- nrow(pudding)
  ties <- data.frame(Winner = array(split(pudding[c("i", "j")], 1:n), n),
                     Loser = rep(NA, 15))
} else {
  ties <- data.frame(Winner = asplit(pudding[c("i", "j")], 1),
                     Loser = rep(NA, 15))
}
head(ties, 2)
#>   Winner Loser
#> 1   1, 2    NA
#> 2   1, 3    NA

# convert to rankings
R <- as.rankings(rbind(i_wins, j_wins, ties),
                 input = "orderings")
head(R, 2)
#> [1] "1 > 2" "1 > 3"
tail(R, 2)
#> [1] "4 = 6" "5 = 6"

# define weights as frequencies of each ranking
w <- unlist(pudding[c("w_ij", "w_ji", "t_ij")])

# fit Plackett-Luce model: limit iterations to match paper
mod <- PlackettLuce(R, npseudo = 0, weights = w, maxit = 7)
#> Warning: Iterations have not converged.
```
