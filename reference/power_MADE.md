# Power Approximation for Overall Average Effects in Meta-Analysis With Dependent Effect Sizes

Compute power of the test of the overall average effect size in a
meta-analysis of dependent effect size estimates, given a specified
number of studies, effect size of practical concern, estimation method,
and further assumptions about the distribution of studies.

## Usage

``` r
power_MADE(
  J,
  mu,
  tau,
  omega,
  rho,
  alpha = 0.05,
  d = 0,
  model = "CHE",
  var_df = "RVE",
  sigma2_dist = NULL,
  n_ES_dist = NULL,
  iterations = 100,
  seed = NULL,
  warning = TRUE,
  average_power = TRUE
)
```

## Arguments

- J:

  Number of studies. Can be one value or a vector of multiple values.

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

- average_power:

  Logical indicating whether to calculate average power across the
  iterations for each condition.

## Value

Returns a `tibble` with information about the expectation of the number
of studies, the effect size of practical concern, the between-study and
within-study variance components, the sample correlation, the contrast
effect, the level of statistical significance, the sampling variance of
overall average effect size of practical concern, the degrees of
freedom, the power, the mcse, the number of iterations, the model to
handle dependent effect sizes, and the methods used to obtain sampling
variance estimates as well as the number effect sizes per study.

## Details

Find all background material behind the power approximations in Vembye,
Pustejovsky, & Pigott (2022), including arguments for why it is
suggested neither to conduct power analysis based on balanced
assumptions about the number of effects per study and the study variance
nor to use the original power approximation assuming independence among
effect sizes (Hedges & Pigott, 2001).

## References

Vembye, M. H., Pustejovsky, J. E., & Pigott, T. D. (2022). Power
approximations for overall average effects in meta-analysis with
dependent effect sizes. *Journal of Educational and Behavioral
Statistics*, 1–33.
[doi:10.3102/10769986221127379](https://doi.org/10.3102/10769986221127379)

Hedges, L. V., & Pigott, T. D. (2001). The power of statistical tests in
meta-analysis. *Psychological Methods*, 6(3), 203–217.
[doi:10.1037/1082-989X.6.3.203](https://doi.org/10.1037/1082-989X.6.3.203)

## Examples

``` r
power <- power_MADE(
   J = c(40, 60),
   mu = 0.2,
   tau = 0.2,
   omega = 0.1,
   rho = 0.7,
   sigma2_dist = \(x) rgamma(x, shape = 5, rate = 10),
   n_ES_dist = \(x) 1 + stats::rpois(x, 5.5 - 1),
   model = c("CHE", "MLMA", "CE"),
   var_df = c("Model", "Satt", "RVE"),
   alpha = .05,
   seed = 10052510,
   iterations = 5
 )

power
#> # A tibble: 14 × 15
#>        J    mu   tau omega   rho     d alpha   var_b    df power   mcse
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl>  <dbl>
#>  1    40   0.2   0.2   0.1   0.7     0  0.05 0.00919  33.1 0.527 0.0111
#>  2    40   0.2   0.2   0.1   0.7     0  0.05 0.00917  39   0.533 0.0109
#>  3    40   0.2   0.2   0.1   0.7     0  0.05 0.00917  32.6 0.528 0.0112
#>  4    40   0.2   0.2   0.1   0.7     0  0.05 0.00917  33.0 0.528 0.0112
#>  5    40   0.2   0.2   0.1   0.7     0  0.05 0.0101   39   0.495 0.0110
#>  6    40   0.2   0.2   0.1   0.7     0  0.05 0.0101   38.0 0.494 0.0110
#>  7    40   0.2   0.2   0.1   0.7     0  0.05 0.0101   38.0 0.495 0.0110
#>  8    60   0.2   0.2   0.1   0.7     0  0.05 0.00594  47.9 0.720 0.0128
#>  9    60   0.2   0.2   0.1   0.7     0  0.05 0.00593  59   0.724 0.0127
#> 10    60   0.2   0.2   0.1   0.7     0  0.05 0.00593  47.5 0.721 0.0128
#> 11    60   0.2   0.2   0.1   0.7     0  0.05 0.00593  48.1 0.721 0.0127
#> 12    60   0.2   0.2   0.1   0.7     0  0.05 0.00659  59   0.679 0.0148
#> 13    60   0.2   0.2   0.1   0.7     0  0.05 0.00659  57.3 0.679 0.0148
#> 14    60   0.2   0.2   0.1   0.7     0  0.05 0.00659  57.4 0.679 0.0148
#> # ℹ 4 more variables: iterations <int>, model <chr>, samp_method_sigma2 <chr>,
#> #   samp_method_kj <chr>



```
