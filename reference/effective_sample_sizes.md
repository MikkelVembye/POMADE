# Approximate Effective Sample Sizes

Approximate Effective Sample Sizes

## Usage

``` r
effective_sample_sizes(
  sample_sizes_raw = NULL,
  Nt_raw = NULL,
  Nc_raw = NULL,
  cluster_size = 22,
  icc = 0.22
)
```

## Arguments

- sample_sizes_raw:

  Vector of the raw total study sample size(s).

- Nt_raw:

  Vector of raw treatment group sample size(s).

- Nc_raw:

  Vector of raw control group sample size(s).

- cluster_size:

  Average cluster size (Default = 22, a common class size in education
  research studies).

- icc:

  Assumed intra-class correlation (Default = 0.22, the average ICC value
  in Hedges & Hedberg (2007) unconditional models)

## Value

A vector of effective sample sizes, adjusted for cluster-dependence.

## Details

`N_j/DE`

## Examples

``` r
sample_sizes <- sample(50:1000, 50, replace = TRUE)
effective_sample_sizes(
  sample_sizes_raw = sample_sizes,
  cluster_size = 20,
  icc = 0.15
)
#>  [1]  58 118 132  29 139 239 241 158 254 130  65  64 147 212 152 233  33 246 129
#> [20] 160 124  29 157 214 135  91 155  31 129  80 208 188 188  69 201 157 247 154
#> [39] 259  31  82  52  82 121  43 254 150  59  61  18

```
