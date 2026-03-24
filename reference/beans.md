# Preferred Bean Varieties in Nicaragua

This is a subset of data from trials of bean varieties (*Phaseolus
vulgaris* L.) in Nicaragua over five growing seasons. Farmers were asked
to try three varieties of bean from a total of ten varieties and to rank
them in order of preference. In addition, for each variety the farmers
were asked to compare each trial variety to the local variety and state
whether they considered it to be better or worse.

## Usage

``` r
beans
```

## Format

A data frame with 842 records and 14 variables:

- `variety_a`:

  The name of variety A in the comparison.

- `variety_b`:

  The name of variety B in the comparison.

- `variety_c`:

  The name of variety C in the comparison.

- `best`:

  The variety the farmer ranked in first place ("A", "B" or "C").

- `worst`:

  The variety the farmer ranked in last place ("A", "B" or "C").

- `var_a`:

  How the farmer ranked variety A compared to the local variety ("Worse"
  or "Better").

- `var_b`:

  How the farmer ranked variety B compared to the local variety ("Worse"
  or "Better").

- `var_c`:

  How the farmer ranked variety C compared to the local variety ("Worse"
  or "Better").

- `season`:

  A factor specifying the growing season ("Po - 15", "Ap - 15", "Pr -
  16", "Po - 16", "Ap - 16".

- `year`:

  The year of planting.

- `maxTN`:

  The maximum temperature at night during the vegetative cycle (degrees
  Celsius).

- `lon`:

  The geographic coordinate longitude (X axis) for where the plot was
  established.

- `lat`:

  The geographic coordinate latitude (Y axis) for where the plot was
  established.

- `planting_date`:

  A Date, specifying the start date of planting the trial.

## Source

van Etten, J. et al. (2019) *PNAS*, **116** (10), 4194–4199,
[doi:10.1073/pnas.1813720116](https://doi.org/10.1073/pnas.1813720116) .

## Details

There are three crop seasons in Central America:

- Primera:

  May - August.

- Postrera:

  September - October.

- Apante:

  November - January.

Beans can be planted near the beginning of each season, though are most
commonly planted in the Postrera or Apante seasons.

## Examples

``` r
# Consider the best and worst rankings. These give the variety the
# farmer thought was best or worst, coded as A, B or C for the
# first, second or third variety assigned to the farmer
# respectively.
data(beans)
head(beans[c("best", "worst")], 2)
#>   best worst
#> 1    C     A
#> 2    B     A

# Fill in the missing item
beans$middle <- complete(beans[c("best", "worst")],
                         items = c("A", "B", "C"))
head(beans[c("best", "middle", "worst")], 2)
#>   best middle worst
#> 1    C      B     A
#> 2    B      C     A

# This gives an ordering of the three varieties the farmer was
# given. The names of these varieties are stored in separate
# columns
varieties <- beans[c("variety_a", "variety_b", "variety_c")]
head(varieties, 2)
#>     variety_a       variety_b   variety_c
#> 1 BRT 103-182      SJC 730-79 PM2 Don Rey
#> 2   INTA Rojo INTA Centro Sur INTA Sequia

# Use these names to decode the orderings of order 3
order3 <- decode(beans[c("best", "middle", "worst")],
                 items = beans[c("variety_a", "variety_b", "variety_c")],
                 code = c("A", "B", "C"))

# Now consider the paired comparisons agains the local variety
head(beans[c("var_a", "var_b", "var_c")], 2)
#>   var_a  var_b  var_c
#> 1 Worse  Worse Better
#> 2 Worse Better Better

# Convert these results to a vector and get the corresponding trial variety
outcome <- unlist(beans[c("var_a", "var_b", "var_c")])
trial_variety <- unlist(beans[c("variety_a", "variety_b", "variety_c")])

# Create a data frame of the implied orderings of order 2
order2 <- data.frame(Winner = ifelse(outcome == "Worse",
                                     "Local", trial_variety),
                     Loser = ifelse(outcome == "Worse",
                                    trial_variety, "Local"),
                     stringsAsFactors = FALSE, row.names = NULL)
head(order2, 2)
#>   Winner       Loser
#> 1  Local BRT 103-182
#> 2  Local   INTA Rojo

# Finally combine the rankings of order 2 and order 3
R <- rbind(as.rankings(order3, input = "orderings"),
           as.rankings(order2, input = "orderings"))
head(R)
#> [1] "PM2 Don Rey > SJC 730-79 > BRT 103-182"  
#> [2] "INTA Centro Sur > INTA Sequia > INTA ..."
#> [3] "INTA Ferroso > INTA Matagalpa > BRT  ..."
#> [4] "INTA Rojo > INTA Centro Sur > ALS 0532-6"
#> [5] "PM2 Don Rey > INTA Sequia > SJC 730-79"  
#> [6] "ALS 0532-6 > INTA Matagalpa > INTA Rojo" 
tail(R)
#> [1] "INTA Sequia > Local"    "INTA Sequia > Local"    "BRT 103-182 > Local"   
#> [4] "Local > INTA Matagalpa" "Local > INTA Rojo"      "Local > SJC 730-79"    
```
