# Fit a Plackett-Luce Model with Linear Predictor for Log-worth

Fit a Plackett-Luce model where the log-worth is predicted by a linear
function of covariates. The rankings may be partial (each ranking
completely ranks a subset of the items), but ties are not supported.

## Usage

``` r
pladmm(
  rankings,
  formula,
  data = NULL,
  weights = freq(rankings),
  start = NULL,
  contrasts = NULL,
  rho = 1,
  n_iter = 500,
  rtol = 1e-04
)
```

## Arguments

- rankings:

  a
  `"`[`rankings`](https://hturner.github.io/PlackettLuce/reference/rankings.md)`"`
  object, or an object that can be coerced by `as.rankings`. An
  [`"aggregated_rankings"`](https://hturner.github.io/PlackettLuce/reference/aggregate.md)
  object can be used to specify rankings and weights simultaneously.

- formula:

  a [formula](https://rdrr.io/r/stats/formula.html) specifying the
  linear model for log-worth.

- data:

  a data frame containing the variables in the model.

- weights:

  weights for the rankings.

- start:

  starting values for the coefficients.

- contrasts:

  an optional list specifying contrasts for the factors in `formula`.
  See the `contrasts.arg` of
  [`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html).

- rho:

  the penalty parameter in the penalized likelihood, see details.

- n_iter:

  the maximum number of iterations (also for inner loops).

- rtol:

  the convergence tolerance (also for inner loops)

## Details

The log-worth is modelled as a linear function of item covariates:
\$\$\log \alpha_i = \beta_0 + \beta_1 x\_{i1} + \ldots + \beta_p
x\_{ip}\$\$ where \\\beta_0\\ is fixed by the constraint that \\\sum_i
\alpha_i = 1\\.

The parameters are estimated using an Alternating Directions Method of
Multipliers (ADMM) algorithm proposed by Yildiz (2020). ADMM alternates
between estimating the worths \\\alpha_i\\ and the linear coefficients
\\\beta_k\\, encapsulating them in a quadratic penalty on the
likelihood: \$\$L(\boldsymbol{\beta}, \boldsymbol{\alpha},
\boldsymbol{u}) = \mathcal{L}(\mathcal{D}\|\boldsymbol{\alpha}) +
\frac{\rho}{2}\|\|\boldsymbol{X}\boldsymbol{\beta} - \log
\boldsymbol{\alpha} + \boldsymbol{u}\|\|^2_2 -
\frac{\rho}{2}\|\|\boldsymbol{u}\|\|^2_2\$\$ where \\\boldsymbol{u}\\ is
a dual variable that imposes the equality constraints (so that \\\log
\boldsymbol{\alpha}\\ converges to
\\\boldsymbol{X}\boldsymbol{\beta}\\).

## Note

This is a prototype function and the user interface is planned to change
in upcoming versions of PlackettLuce.

## References

Yildiz, I., Dy, J., Erdogmus, D., Kalpathy-Cramer, J., Ostmo, S.,
Campbell, J. P., Chiang, M. F. and Ioannidis, S. (2020) Fast and
Accurate Ranking Regression In Proceedings of the Twenty Third
International Conference on Artificial Intelligence and Statistics,
**108**, 77–-88.

## Examples

``` r
# data.frame of rankings for salad dressings A B C D
# 1 = most tart, 4 = least tart
salad[1:3,]
#>   A B C D
#> 1 1 2 3 4
#> 2 1 2 3 4
#> 3 2 1 3 4

# create data frame of corresponding features
# (acetic and gluconic acid concentrations in salad dressings)
features <- data.frame(salad = LETTERS[1:4],
                       acetic = c(0.5, 0.5, 1, 0),
                       gluconic = c(0, 10, 0, 10))

# fit Plackett-Luce model based on covariates
res_PLADMM <- pladmm(salad, ~ acetic + gluconic, data = features, rho = 8)
## coefficients
coef(res_PLADMM)
#> (Intercept)      acetic    gluconic 
#>  -4.8409655   3.2743081   0.2739226 
## worth
res_PLADMM$pi
#>          A          B          C          D 
#> 0.04061305 0.62842986 0.20872416 0.12223294 
## worth as predicted by linear function
res_PLADMM$tilde_pi
#>          A          B          C          D 
#> 0.04060714 0.62839568 0.20874175 0.12224363 
## equivalent to
drop(exp(res_PLADMM$x %*% coef(res_PLADMM)))
#>          A          B          C          D 
#> 0.04060714 0.62839568 0.20874175 0.12224363 
```
