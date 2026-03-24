# Read Preflib Election Data Files

Read orderings from `.soc`, `.soi`, `.toc` or `.toi` file types storing
election data as defined by [{PrefLib}: A Library for
Preferences](https://www.preflib.org/).

## Usage

``` r
read.soc(file)

read.soi(file)

read.toc(file)

read.toi(file)

# S3 method for class 'preflib'
as.aggregated_rankings(x, ...)
```

## Arguments

- file:

  An election data file, conventionally with extension `.soc`, `.soi`,
  `.toc` or `.toi` according to data type.

- x:

  An object of class `"preflib"`.

- ...:

  Additional arguments passed to
  [`as.rankings()`](https://hturner.github.io/PlackettLuce/reference/rankings.md):
  `freq`, `input` or `items` will be ignored with a warning as they are
  set automatically.

## Value

A data frame of class `"preflib"` with first column `Freq`, giving the
frequency of the ranking in that row, and remaining columns `Rank 1`,
..., `Rank r` giving the items ranked from first to last place in that
ranking. Ties are represented by vector elements in list columns. The
data frame has an attribute `"items"` giving the labels corresponding to
each item number.

## Details

The file types supported are

- .soc:

  Strict Orders - Complete List

- .soi:

  Strict Orders - Incomplete List

- .toc:

  Orders with Ties - Complete List

- .toi:

  Orders with Ties - Incomplete List

Note that the file types do not distinguish between types of incomplete
orderings, i.e. whether they are a complete ranking of a subset of items
(as supported by
[`PlackettLuce()`](https://hturner.github.io/PlackettLuce/reference/PlackettLuce.md))
or top-\\n\\ rankings of \\n\\ items from the full set of items (not
currently supported by
[`PlackettLuce()`](https://hturner.github.io/PlackettLuce/reference/PlackettLuce.md)).

The numerically coded orderings and their frequencies are read into a
data frame, storing the item names as an attribute. The
`as.aggregated_rankings` method converts these to an
[`"aggregated_rankings"`](https://hturner.github.io/PlackettLuce/reference/aggregate.md)
object with the items labelled by the item names.

A Preflib file may be corrupt, in the sense that the ordered items do
not match the named items. In this case, the file can be read in as a
data frame (with a warning) using the corresponding `read.*` function,
but `as.aggregated_rankings` will throw an error.

## Note

The Netflix and cities datasets used in the examples are from Bennet and
Lanning (2007) and Caragiannis et al (2017) respectively. These data
sets require a citation for re-use.

## References

Mattei, N. and Walsh, T. (2013) PrefLib: A Library of Preference Data.
*Proceedings of Third International Conference on Algorithmic Decision
Theory (ADT 2013)*. Lecture Notes in Artificial Intelligence, Springer.

Caragiannis, I., Chatzigeorgiou, X, Krimpas, G. A., and Voudouris, A. A.
(2017) Optimizing positional scoring rules for rank aggregation. In
*Proceedings of the 31st AAAI Conference on Artificial Intelligence*.

Bennett, J. and Lanning, S. (2007) The Netflix Prize. *Proceedings of
The KDD Cup and Workshops*.

## Examples

``` r
# strict complete orderings of four films on Netflix
netflix <- read.soc(system.file("extdata", "netflix.soc",
                                package = "PlackettLuce"))
head(netflix)
#>   Freq Rank 1 Rank 2 Rank 3 Rank 4
#> 1   68      2      1      4      3
#> 2   53      1      2      4      3
#> 3   49      2      1      3      4
#> 4   44      1      2      3      4
#> 5   39      2      4      1      3
#> 6   37      3      2      1      4
attr(netflix, "items")
#>                        1                        2                        3 
#>             "Mean Girls"      "Beverly Hills Cop"      "The Mummy Returns" 
#>                        4 
#> "Mission: Impossible II" 

head(as.aggregated_rankings(netflix))
#>                                    ranking freq
#> 1 Beverly Hills Cop > Mean Girls > Mis ...   68
#> 2 Mean Girls > Beverly Hills Cop > Mis ...   53
#> 3 Beverly Hills Cop > Mean Girls > The ...   49
#> 4 Mean Girls > Beverly Hills Cop > The ...   44
#> 5 Beverly Hills Cop > Mission: Impossi ...   39
#> 6 The Mummy Returns > Beverly Hills Co ...   37

# strict incomplete orderings of 6 random cities from 36 in total
cities <- read.soi(system.file("extdata", "cities.soi",
                                package = "PlackettLuce"))

# complete orderings with ties of 30 skaters
skaters <- read.toc(system.file("extdata", "skaters.toc",
                                package = "PlackettLuce"))

# incomplete orderings with ties: most important qualities for success
# from 20 in total
qualities <- read.toi(system.file("extdata", "education_qualities.toi",
                      package = "PlackettLuce"))

# alternatively read from a url
# - can take a little while depending on speed of internet connection

if (FALSE) { # \dontrun{
# incomplete orderings with ties: most important qualities for success
# from 20 in total
preflib <- "https://raw.githubusercontent.com/PrefLib/PrefLib-Data/main/datasets"
qualities2 <- read.toi(file.path(preflib, "00032%20-%20education/00032-00000007.toi"))
all.equal(qualities, qualities2)
} # }
```
