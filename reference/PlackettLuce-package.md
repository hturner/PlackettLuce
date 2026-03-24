# Plackett-Luce Models for Rankings

Plackett-Luce provides functions to prepare rankings data in order to
fit the Plackett-Luce model or Plackett-Luce trees. The implementation
can handle ties, sub-rankings and rankings that imply disconnected or
weakly connected preference networks. Methods are provided for summary
and inference.

## Details

The main function in the package is the model-fitting function
[`PlackettLuce`](https://hturner.github.io/PlackettLuce/reference/PlackettLuce.md)
and the help file for that function provides details of the
Plackett-Luce model, which is extended here to accommodate ties.

Rankings data must be passed to `PlackettLuce` in a specific form, see
[`rankings`](https://hturner.github.io/PlackettLuce/reference/rankings.md)
for more details. Other functions for handling rankings include
`choices` to express the rankings as choices from alternatives;
`adjacency` to create an adjacency matrix of wins and losses implied by
the rankings and `connectivity` to check the connectivity of the
underlying preference network.

Several methods are available to inspect fitted Plackett-Luce models,
help files are available for less common methods or where arguments may
be specified: [`coef`](https://rdrr.io/r/stats/coef.html), `deviance`,
[`fitted`](https://rdrr.io/r/stats/fitted.values.html),
[`itempar`](https://rdrr.io/pkg/psychotools/man/itempar.html), `logLik`,
`print`,
[`qvcalc`](https://davidfirth.github.io/qvcalc/reference/qvcalc.html),
[`summary`](https://rdrr.io/r/base/summary.html),
[`vcov`](https://rdrr.io/r/stats/vcov.html).

PlackettLuce also provides the function `pltree` to fit a Plackett-Luce
tree i.e. a tree that partitions the rankings by covariate values,
identifying subgroups with different sets of worth parameters for the
items. In this case
[`group`](https://hturner.github.io/PlackettLuce/reference/group.md)
must be used to prepare the data.

Several data sets are provided in the package:
[`beans`](https://hturner.github.io/PlackettLuce/reference/beans.md),
[`nascar`](https://hturner.github.io/PlackettLuce/reference/nascar.md),
[`pudding`](https://hturner.github.io/PlackettLuce/reference/pudding.md).
The help files for these give further illustration of preparing rankings
data for modelling. The
[`read.soc`](https://hturner.github.io/PlackettLuce/reference/preflib.md)
function enables further example data sets of "Strict Orders - Complete
List" format (i.e. complete rankings with no ties) to be downloaded from
[PrefLib](https://www.preflib.org/).

A full explanation of the methods with illustrations using the package
data sets is given in the vignette,
[`vignette("Overview", package = "PlackettLuce")`](https://hturner.github.io/PlackettLuce/articles/Overview.md).

## References

Turner, H.L., van Etten, J., Firth, D. and Kosmidis, I. (2020) Modelling
Rankings in R: The PlackettLuce Package. *Computational Statistics*,
**35**, 1027–1057.
[doi:10.1007/s00180-020-00959-3](https://doi.org/10.1007/s00180-020-00959-3)
.

## See also

Useful links:

- <https://hturner.github.io/PlackettLuce/>

- <https://github.com/hturner/PlackettLuce>

- Report bugs at <https://github.com/hturner/PlackettLuce/issues>

## Author

**Maintainer**: Heather Turner <ht@heatherturner.net>
([ORCID](https://orcid.org/0000-0002-1256-3375))

Authors:

- Ioannis Kosmidis ([ORCID](https://orcid.org/0000-0003-1556-0302))

- David Firth ([ORCID](https://orcid.org/0000-0003-0302-2312))

Other contributors:

- Jacob van Etten ([ORCID](https://orcid.org/0000-0001-7554-2558))
  \[contributor\]
