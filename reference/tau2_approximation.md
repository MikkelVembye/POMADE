# Between-Study Variance Approximation Function

Rough approximation of the between-study variance based on assumption
about the typical sample size of studies included in the synthesis

## Usage

``` r
tau2_approximation(sample_size = 100, es, df_minus2 = TRUE)
```

## Arguments

- sample_size:

  Typical sample size of studies

- es:

  Smallest effect size of practical concern

- df_minus2:

  If degrees of freedom should be df-2 or just df

## Value

A `tibble` with small, medium, and large magnitudes of tau2

## Examples

``` r

tau2_approximation(
sample_size = 50,
es = 0.1,
df_minus2 = TRUE
)
#>         tau2
#> small  0.027
#> medium 0.080
#> large  0.240

```
