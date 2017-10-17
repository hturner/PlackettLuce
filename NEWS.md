# PlackettLuce 0.2

## New Features

* Add vignette.
* Add data sets `pudding`, `nascar` and `beans`.
* Add `pltree()` function for use with `partykit::mob()`. Requires new 
objects of type `"grouped_rankings"` that add a grouping index to a `"rankings"`
object and store other derived objects used by `PlackettLuce`. Methods to print,
plot and predict from Plackett-Luce tree are provided.
* Add `connectivity()` function to check connectivity of a network given 
adjancency matrix. New `adjacency()` function comuptes adjacency matrix without
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


# PlackettLuce 0.1

* GitHub-only release of prototype package.
