# Plackett-Luce Model Summaries

Obtain the coefficients, model summary or coefficient
variance-covariance matrix for a model fitted by `PlackettLuce`.

## Usage

``` r
# S3 method for class 'PlackettLuce'
coef(object, ref = 1L, log = TRUE, type = "all", ...)

# S3 method for class 'PlackettLuce'
summary(object, ref = 1L, ...)

# S3 method for class 'PlackettLuce'
vcov(object, ref = 1L, type = c("expected", "observed"), ...)
```

## Arguments

- object:

  An object of class "PlackettLuce" as returned by `PlackettLuce`.

- ref:

  An integer or character string specifying the reference item (for
  which log worth will be set to zero). If `NULL` the sum of the log
  worth parameters is set to zero.

- log:

  A logical indicating whether to return parameters on the log scale
  with the item specified by `ref` set to zero.

- type:

  For `coef`, the type of coefficients to return: one of `"ties"`,
  `"worth"` or `"all"`. For `vcov`, the type of Fisher information to
  base the estimation on: either `"expected"` or `"observed"`.

- ...:

  additional arguments, passed to `vcov` by `summary`.

## Details

By default, parameters are returned on the log scale, as most suited for
inference. If `log = FALSE`, the worth parameters are returned,
constrained to sum to one so that they represent the probability that
the corresponding item comes first in a ranking of all items, given that
first place is not tied.

The variance-covariance matrix is returned for the worth and tie
parameters on the log scale, with the reference as specified by `ref`.
For models estimated by maximum likelihood, the variance-covariance is
the inverse of the Fisher information of the log-likelihood.

For models with a normal or gamma prior, the variance-covariance is
based on the Fisher information of the log-posterior. When adherence
parameters have been estimated, the log-posterior is not linear in the
parameters. In this case there is a difference between the expected and
observed Fisher information. By default, `vcov` will return the
variance-covariance based on the expected information, but `type` gives
to option to use the observed information instead. For large samples,
the difference between these options should be small. Note that the
estimation of the adherence parameters is accounted for in the
computation of the variance-covariance matrix, but only the sub-matrix
corresponding to the worth and tie parameters is estimated.
