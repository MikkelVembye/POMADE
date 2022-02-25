
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="man/figures/POMADE_hex.png" align="right" alt="" width="180" />

# POMADE

<!-- badges: start -->
<!-- badges: end -->

The goal of the POMADE package is to provide approximation and plot
functions for conducting power analysis of the correlated-hierarchical
effects (CHE), multi-level meta-analysis (MLMA), and correlated-effects
(CE) models for meta-analysis of dependent effect sizes developed by
Vembye, Pustejovsky, & Pigott (2022). These approximations replace
Hedges & Pigott’s (2001) previous power approximation based on the
assumption of independence between effect sizes which has been shown to
work inadequately to predict power for models that handle dependent
effect sizes.

## Installation

You can install the development version of POMADE like so:

``` r
# install.packages("devtools")
devtools::install_github("MikkelVembye/POMADE")
```

## Example

Example of how to approximate power for the CHE-RVE model (Pustejovsky &
Tipton, 2021)

``` r
library(POMADE)
library(dplyr)

#?VWB22_pilot
coteach_dat <- VWB22_pilot
#glimpse(coteach_dat)

dat_kjsigma2j <- select(coteach_dat, kj, sigma2j = vg_ms_mean)

power_CHE_RVE_empirical <- 
 power_CHE(
  J = 76,
  tau2 = 0.1^2,
  omega2 = 0.25^2,
  beta = 0.1,
  rho = 0.7,
  var_df = "RVE",
  sigma2_method = "empirical",
  pilot_data_kjsigma2 = dat_kjsigma2j,
  alpha = 0.05,
  seed = 10052510
)

power_CHE_RVE_empirical
#> # A tibble: 1 x 7
#>   samp_method        method     es   var_b    df power_sig05 iterations
#>   <chr>              <chr>   <dbl>   <dbl> <dbl>       <dbl>      <dbl>
#> 1 empirical sigma2js CHE-RVE   0.1 0.00134  40.9       0.761        100
```

# Acknowledgments

Thanks to [Savhannah Schulz](https://savhannahschulz.netlify.app/) for
making our hex stickers.

# Reference

Hedges & Pigott (2001). The Power of Statistical Tests in Meta-Analysis.
*Psychological Methods*, 6(3), 203

Pustejovsky & Tipton (2021). Meta-analysis with Robust Variance
Estimation: Expanding the range of working models. *Prevention Science*,
1-14

Vembye, Pustejovsky, & Pigott (2022). Power Approximations for
Meta-Analysis of Dependent Effect sizes.
<https://osf.io/preprints/metaarxiv/6tp9y/>
