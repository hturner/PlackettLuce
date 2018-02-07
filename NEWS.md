# PlackettLuce dev version

## Bug fixes

* `vcov.PlackettLuce` now works for models with non-integer weights (fixes #25).
* `plot.pltree` now works for `worth = TRUE` with psychotree version 0.15-2 (currently pre-release on https://r-forge.r-project.org/R/?group_id=330)

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
from http://www.preflib.org.

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
