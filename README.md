
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="man/figures/POMADE_hex.png" align="right" alt="" width="180" />

# POMADE

<!-- badges: start -->
<!-- badges: end -->

The goal of the POMADE package is to provide approximation and plot
functions for conducting power analysis of the correlated-hierarchical
effects (CHE), multi-level meta-analysis (MLMA), and correlated-effects
(CE) models for meta-analysis of dependent effect sizes developed by
Vembye, Pustejovsky, & Pigott (2022).

## Installation

You can install the development version of POMADE like so:

``` r
# install.packages("devtools")
devtools::install_github("MikkelVembye/POMADE")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(POMADE)
library(dplyr)

coteach_dat <- VWB22_pilot

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

Vembye, Pustejovsky, & Pigott (2022). Power Approximations for
Meta-Analysis of Dependent Effect sizes.
<https://osf.io/preprints/metaarxiv/6tp9y/>
