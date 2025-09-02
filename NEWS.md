# PlackettLuce 0.4.4

* Fix minor typesetting issues in the docs
* Make tests (and hence R CMD check) pass without suggested packages

# PlackettLuce 0.4.3

* Update functions to read Preflib Election Data Files (`read.soc` etc) so that they work for updated Preflib file formats.
* Update Preflib URLs.

# PlackettLuce 0.4.2

* Fix test broken by survival update.

# PlackettLuce 0.4.1

## New features

* Extend `pltree` to allow option of modelling log-worth with a linear predictor (via `pladmm()`).
* Improved handling of model formula in `pladmm()`, including possibility to specify contrasts for any factors in the formula.
* Add anova method for PLADMM models.
* Add `weights` argument to `pladmm()`, allowing aggregated rankings to be modelled, optionally using an `aggregate_rankings` object to specify rankings and weights together.

## Bug fixes

* Avoid computing variance-covariance matrix in `predict.PLADMM(vcov = FALSE)` and `AIC` when new data specified (partial fix to #50).
* Correct residual df for PLADMM models based on partial rankings (previously assumed all rankings had equal number of items).
* Update URL for Preflib data sets.

# PlackettLuce 0.4.0

* New `pladmm` function to fit the Plackett-Luce model with log-worth modelled by item covariates.
* Add three new variables to the `beans` data. The planting date, and geographical coordinates (@kauedesousa, #41).

# PlackettLuce 0.3.2

* Fix test that fails with new behaviour of `all.equal()`.
* Update citation to use Computational Statistics paper (#44).

# PlackettLuce 0.3.1

* Fix Preflib URL.

# PlackettLuce 0.3.0

* Now correctly handles cases where intermediate tie orders are not observed, fixing corresponding tie parameters to zero (#42).

# PlackettLuce 0.2-9

* `vcov.PlackettLuce()` works again for `ref = NULL` (bug introduced with vcov method in version 0.2-4)
* avoid dependency on R >= 3.6.0 by providing alternatives to `asplit()`
* `read.soi()` and `read.toi()` now handle incomplete rankings with very irregular lengths correctly.
* `read.*()` functions for Preflib formats now give a meaningful error when the file or URL does not exist, and a warning if the file is corrupt.
* `as.rankings` with `input = "orderings"` now checks coded values can be matched to item names, if provided.
* `PlackettLuce()` now works with `nspeudo > 0` when there are no observed paired comparisons.
* `?PlackettLuce` now gives advice on analysing data with higher order ties.

# PlackettLuce 0.2-8

* Fix bug in `as.rankings.matrix()` introduced in version 0.2-7.
* Import `eigs` from RSpectra vs rARPACK.

# PlackettLuce 0.2-7

## New Features

* New `"aggregated_rankings"` object to store aggregated rankings with the corresponding frequencies. Objects of class `"rankings"` can be aggregated via the `aggregate` method; alternatively `rankings()` and `as.rankings()` will create an `"aggregated_rankings"` object when `aggregate = TRUE`. `as.rankings()` also handles pre-aggregated data, accepting frequencies via the `freq` argument.
* New `freq()` function to extract frequencies from aggregated rankings.
* `as.rankings()` can now create a `"grouped_rankings"` object, if a grouping index is passed via the `index` argument.
* New `as.matrix()` methods for rankings and aggregated rankings to extract the underlying matrix of rankings, with frequencies in the final column if relevant. This means rankings can be saved easily with `write.table()`.
* New `complete()` and `decode()` functions to help pre-process orderings before converting to rankings, `complete()` infers the item(s) in r'th rank given the items in the other (r - 1) ranks. `decode()` converts coded (partial) orderings to orderings of the items in each ordering.
* New `read.soi()`, `read.toc()` and `read.toi()` to read the corresponding PrefLib file formats (for data types "Strict Orders - Incomplete List", "Orders with Ties - Complete List" and "Orders with Ties - Incomplete List"). An `as.aggregated_rankings()` method is provided to convert the data frame of aggregated orderings to an `"aggregated_rankings"` object.

## Improvements

* `pltree()` now respects `na.action` and will pad predictions and fitted values for `na.action = "na.exclude"` if the rankings are missing for a whole group or one of the model covariates has a missing value.
* `PlackettLuce()` now has an `na.action` argument for handling of missing rankings.
* `fitted()` and `choices()` now return data frames, with list columns as necessary.

## Changes in behaviour

* `rankings()` now sets redundant/inconsistent ranks to `NA` rather than dropping them. This does not affect the final ranking, unless it is completely `NA`.
* The frequencies column in the data frame returned by `read.soc()` is now named `Freq` rather than `n`.
* The `"item"` attribute of the data frame returned by `read.soc()` is now named `"items"`.
* The `labels` argument in `as.rankings()` has been deprecated and replaced by `items`.
* `grouped_ranking()` has been deprecated and replaced by `group()`.
* The redundant columns in the `nascar` data have been dropped.

# PlackettLuce 0.2-6

* Avoid using `isFALSE()` for compatibility with R < 3.5.
* Don't test number of iterations when comparing models on grouped and ungrouped rankings.

# PlackettLuce 0.2-5

* Higher tolerance in tests of `vcov()` for CRAN Windows test machine.

# PlackettLuce 0.2-4

## New Features

* `PlackettLuce()` now supports MAP estimation with a multivariate normal prior on log-worths and/or a gamma prior on ranker adherence.
* `PlackettLuce()` now returns log-likelihood and degrees of freedom for the null model (where all outcomes, including ties, have equal probability).
* There is now a `vcov` method for Plackett-Luce trees.

## Changes in Behaviour

* `itempar.PlackettLuce()` now always returns a matrix, even for a single node tree.

## Bug Fixes

* `pltree()` or `PlackettLuce()` with grouped rankings now work correctly with weights.

# PlackettLuce 0.2-3

## Improvements

* Print methods for `"PlackettLuce"` and `"summary.PlacketLuce"` objects now respect `options("width")`.

## Changes in Behaviour

* `fitted` always returns `n` which is now weighted count of rankings (previously only returned unweighted count with argument `aggregate = TRUE`).

## Bug fixes

* Correct vcov for weighted rankings of more than two items.
* Enable `AIC.pltree` to work on `"pltree"` object with one node.

# PlackettLuce 0.2-2

## New features

* Add `AIC.pltree` to enable computation of AIC on new observations (e.g. data held out in cross-validation).
* Add `fitted.pltree` to return combined fitted probabilities for each choice within each ranking, for each node in a Plackett-Luce tree.

## Bug fixes

* `vcov.PlackettLuce` now works for models with non-integer weights (fixes #25).
* `plot.pltree` now works for `worth = TRUE` with psychotree version 0.15-2 (currently pre-release on https://r-forge.r-project.org/R/?group_id=330)
* `PlackettLuce` and `plfit` now work when `start` argument is set.
* `itempar.PlackettLuce` now works with `alias = FALSE`

# PlackettLuce 0.2-1

## New features

* Add **pkgdown** site.
* Add content to README (fixes #5).
* Add `plot.PlackettLuce` method so that plotting works for a saved 
`"PlackettLuce"` object

## Improvements

* Improved vignette, particularly example based on `beans` data (which has been
updated).
* Improved help files particularly `?PlackettLuce` and new 
`package?PlackettLuce`. (Fixes #14 and #21).

## Changes in behaviour

* `maxit` defaults to 500 in `PlackettLuce`.
* Steffensen acceleration only applied in iterations where it will increase the 
log-likelihood (still only attempted once iterations have reached a solution 
that is "close enough" as specified by `steffensen` argument).

## Bug fixes

* `coef.pltree()` now respects `log = TRUE` argument (fixes #19).
* Fix bug causes lack of convergence with iterative scaling plus 
pseudo-rankings.
* `[.grouped_rankings]` now works for replicated indices.

# PlackettLuce 0.2-0

## New Features

* Add vignette.
* Add data sets `pudding`, `nascar` and `beans`.
* Add `pltree()` function for use with `partykit::mob()`. Requires new 
objects of type `"grouped_rankings"` that add a grouping index to a `"rankings"`
object and store other derived objects used by `PlackettLuce`. Methods to print,
plot and predict from Plackett-Luce tree are provided.
* Add `connectivity()` function to check connectivity of a network given 
adjacency matrix. New `adjacency()` function computes adjacency matrix without
creating edgelist, so remove `as.edgelist` generic and method for 
`"PlackettLuce" objects.
* Add `as.data.frame` methods so that rankings and grouped rankings can be added
to model frames.
* Add `format` methods for rankings and grouped_rankings, for pretty printing.
* Add `[` methods for rankings and grouped_rankings, to create valid rankings
from selected rankings and/or items.
* Add method argument to offer choices of iterative scaling (default), or 
direct maximisation of the likelihood via BFGS or L-BFGS.
* Add `itempar` method for "PlackettLuce" objects to obtain different 
parameterizations of the worth parameters.
* Add `read.soc` function to read Strict Orders - Complete List (.soc) files 
from https://www.preflib.org.

## Changes in behaviour

Old behaviour should be reproducible with arguments
    
    npseudo = 0, steffensen = 0, start = c(rep(1/N, N), rep(0.1, D))

where `N` is number of items and `D` is maximum order of ties.

* Implement pseudo-data approach - now used by default. 
* Improve starting values for ability parameters
* Add Steffensen acceleration to iterative scaling algorithm
* Dropped `ref` argument from `PlackettLuce`; should be specified instead when 
calling `coef`, `summary`, `vcov` or `itempar`.
* `qvcalc` generic now imported from **qvcalc**

## Improvements

* Refactor code to speed up model fitting and computation of fitted values and 
vcov.
* Implement ranking weights and starting values in `PlackettLuce`.
* Add package tests
* Add `log` argument to `coef` so that worth parameters (probability of coming
first in strict ranking of all items) can be obtained easily.


# PlackettLuce 0.1-0

* GitHub-only release of prototype package.
