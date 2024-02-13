## Submission

This is the second version of POMADE. The package provides functions for conducting power analysis for overall average effects in meta-analysis with dependent effect sizes. In this submission, we corrected the formula for expectation of tau-squared estimator in the CE-RVE model, in accordance with a published corrigendum in Journal for Educational and Behavioral Statistics.

## Test environments

* local Windows 10 Enterprise, R 4.3.1
* ubuntu 20.04.3 LTS (on Github), R devel, release, oldrelease
* macOS-latest (on Github), R release
* windows-latest (on Github), R release
* win-builder (devel, release, oldrelease)
* mac-builder (release)
* r-hub:
  * Windows Server 2022, R-release, 32/64 bit
  * Ubuntu Linux 16.04 LTS, R-release, GCC
  * Fedora Linux, R-devel, clang, gfortran
  * Debian Linux, R-devel, GCC

## R CMD check results

There were no ERRORs or WARNINGs. 

There were 2 NOTE:

* Found the following (possibly) invalid URLs: Service Unavailable URL: https://doi.org/10.1002/jrsm.5 From: inst/doc/cwbmeta.html Status: 503 Message: Service Unavailable

The flagged URLs and DOI are correct.


* ONLY on Fedora Linux (r-hub): checking HTML version of manual ... NOTE Skipping checking HTML validation: no command 'tidy' found.

We are not able to change that Tidy is not on the path, or update Tidy on the external Fedora Linux server

## revdepcheck results

We checked 0 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

