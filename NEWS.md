# PlackettLuce 0.2

* Add method argument to offer choices of iterative scaling (default), or 
direct maximisation of the likelihood via BFGS or L-BFGS.
* Add package tests
* Add `adjacency()` and `connectivity()` functions to create adjacency matrix
and check connectivity of the network.
* Remove `as.edgelist` generic and method for `"PlackettLuce" objects.
* Add `itempar` method for "PlackettLuce" objects.
* Implement ranking weights.
* Refactor code to speed up model fitting and computation of fitted values and 
vcov.
* Add `pltree()` function for use with `partykit::mob()`
* Implement pseudo-data approach - now used by default.
* Add Steffensen acceleration to iterative scaling algorithm
* Improve starting values for ability parameters

# PlackettLuce 0.1

* GitHub-only release of prototype package.
