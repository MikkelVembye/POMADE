# Cluster Bias Correction

Function to conduct cluster bias correction of sampling variance
estimates obtained from cluster-randomized studies in which the reported
variance does not account for clustering.

## Usage

``` r
cluster_bias_adjustment(sigma2js, cluster_size = 22, icc = 0.2)
```

## Arguments

- sigma2js:

  A vector of sampling variance estimates that do not account for
  clustering.

- cluster_size:

  A numerical value for average cluster size.

- icc:

  Assumed intra-class correlation (proportion of total variance at the
  cluster level).

## Value

Returns a vector of cluster bias adjusted variance estimates

## Examples

``` r
cbc_var <- cluster_bias_adjustment(
  sigma2js = c(0.04, 0.06, 0.08, 0.1),
  cluster_size = 15,
  icc = 0.15
)

cbc_var
#> [1] 0.124 0.186 0.248 0.310

```
