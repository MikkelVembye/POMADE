# Co-Teaching Dataset

Data from a meta-analysis on the effects of collaborative models of
instruction on student achievement from Vembye, Weiss, and Bhat (2023).

## Usage

``` r
VWB23_pilot
```

## Format

A tibble with 76 rows/studies and 9 variables

- study_year:

  Study author and year of publication

- studyid:

  Unique study ID

- esid:

  Unique effect size ID

- kj:

  Number of effect sizes per study

- N_meanj:

  Average sample size of study

- Nt_meanj:

  Average sample size of treatment group within study

- Nc_meanj:

  Average sample size of control group within study

- ESS_meanj:

  Roughly approximated effective sample sizes

- vg_ms_mean:

  Average cluster bias corrected sampling variance estimates

## Source

Find background material on [Vembye's OSF page](https://osf.io/fby7w/),
and the preprint at <https://osf.io/preprints/metaarxiv/mq5v7/>.

## References

Vembye, M. H., Weiss, F., & Bhat, B. H. (2023). The Effects Co-Teaching
and Related Collaborative Models of Instruction on Student Achievement:
A Systematic Review and Meta-Analysis. *Review of Educational Research*,
[doi:10.3102/00346543231186588](https://doi.org/10.3102/00346543231186588)
