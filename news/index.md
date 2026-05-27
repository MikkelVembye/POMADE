# Changelog

## POMADE 0.2.1

- Added `traffic_light_palette` option to let users specify their own
  color palette for traffic light strips in the
  [`plot_MADE()`](https://mikkelvembye.github.io/POMADE/reference/plot_MADE.md)
  functions.
- Replaced deprecated
  [`purrr::rerun()`](https://purrr.tidyverse.org/reference/rerun.html)
  with [`purrr::map()`](https://purrr.tidyverse.org/reference/map.html)
- Replaced deprecated
  [`purrr::cross_df()`](https://purrr.tidyverse.org/reference/cross.html)
  with
  [`tidyr::expand_grid()`](https://tidyr.tidyverse.org/reference/expand_grid.html)

## POMADE 0.2.0

CRAN release: 2024-02-13

- Corrected formula for expectation of tau-squared estimator in the
  CE-RVE model, in accordance with corrigendum.

## POMADE 0.1.0

CRAN release: 2022-12-02

- This is the first release of POMADE.
