# Finding the Number of Studies Needed to Obtain a Certain Amount of Power

Compute the minimum number of studies needed to obtain a specified power
level in a meta-analysis of dependent effect size estimates, given an
effect size of practical concern, estimation method, and further
assumptions about the distribution of studies.

## Usage

``` r
min_studies_MADE(
  mu,
  tau,
  omega,
  rho,
  alpha = 0.05,
  target_power = 0.8,
  d = 0,
  model = "CHE",
  var_df = "RVE",
  sigma2_dist = NULL,
  n_ES_dist = NULL,
  iterations = 100,
  seed = NULL,
  warning = TRUE,
  upper = 100,
  show_lower = FALSE
)
```

## Arguments

- mu:

  Effect size of practical concern. Can be one value or a vector of
  multiple values.

- tau:

  Between-study SD. Can be one value or a vector of multiple values.

- omega:

  Within-study SD. Can be one value or a vector of multiple values.

- rho:

  Correlation coefficient between effect size estimates from the same
  study. Can be one value or a vector of multiple values.

- alpha:

  Level of statistical significance. Can be one value or a vector of
  multiple values. Default is 0.05.

- target_power:

  Numerical value specifying the target power level. Can be one value or
  a vector of multiple values.

- d:

  Contrast value. Can be one value or a vector of multiple values.
  Default is 0.

- model:

  Assumed working model for dependent effect sizes, either `"CHE"` for
  the correlated-and-hierarchical effects model, `"CE"` for the
  correlated effects model, or `"MLMA"` for the multi-level
  meta-analysis model. Default is `"CHE"`. Can be one value or a vector
  of multiple values.

- var_df:

  Indicates the technique used to obtain the sampling variance of the
  average effect size estimate and the degrees of freedom, either
  `"Model"` for model-based variance estimator with degrees of freedom
  of `J - 1`, `"Satt"` for model-based variance estimator with
  Satterthwaite degrees of freedom, or `"RVE"` for robust variance
  estimator with Satterthwaite degrees of freedom. Default is `"RVE"`.
  Can be one value or a vector of multiple values.

- sigma2_dist:

  Distribution of sampling variance estimates from each study. Can be
  either a single value, a vector of plausible values, or a function
  that generates random values.

- n_ES_dist:

  Distribution of the number of effect sizes per study. Can be either a
  single value, a vector of plausible values, or a function that
  generates random values.

- iterations:

  Number of iterations per condition (default is 100).

- seed:

  Numerical value for a seed to ensure reproducibility of the iterated
  power approximations.

- warning:

  Logical indicating whether to return a warning when either sigma2_dist
  or n_ES_dist is based on balanced assumptions.

- upper:

  Numerical value containing the upper bound of the interval to be
  searched for the minimum number of studies.

- show_lower:

  Logical value indicating whether to report lower bound of the interval
  searched for the minimum number of studies. Default is `FALSE`.

## Value

Returns a `tibble` with information about the expectation of the effect
size of practical concern, the between-study and within-study variance
components, the sample correlation, the contrast effect, the level of
statistical significance, the target power value(s), the number of
studies needed, the number of iterations, the model to handle dependent
effect sizes, and the methods used to obtain sampling variance estimates
as well as the number effect sizes per study.

## Examples

``` r

min_studies_MADE(
  mu = 0.3,
  tau = 0.05,
  omega = 0.01,
  rho = 0.2,
  target_power = .7,
  alpha = 0.05,
  model = "CE",
  var_df = "RVE",
  sigma2_dist = 4 / 200,
  n_ES_dist = 5.5,
  seed = 10052510
)
#> Warning: Notice: It is generally recommended not to draw on balanced assumptions regarding the study precision (sigma2js) or the number of effect sizes per study (kjs). See Figures 2A and 2B in Vembye, Pustejovsky, and Pigott (2022).
#> # A tibble: 1 × 12
#>      mu   tau omega   rho     d alpha target_power studies_needed iterations
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>        <dbl>          <dbl>      <dbl>
#> 1   0.3  0.05  0.01   0.2     0  0.05          0.7              5        100
#> # ℹ 3 more variables: model <chr>, samp_method_sigma2 <chr>,
#> #   samp_method_kj <chr>

```
