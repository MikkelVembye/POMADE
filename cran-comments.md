## Submission

This is a patch version update of POMADE. The package provides functions for conducting power analysis for overall average effects in meta-analysis with dependent effect sizes. In this submission, we replaced the deprecated `purrr::rerun()` with `purrr::map()` and updated the traffic light color options to make them more flexible for the user. 

## Test environments

* local Windows 10 Enterprise, R 4.5.3
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

There were no ERRORs, WARNINGs or NOTEs. 

## URL check results

We found two potential URL errors

* Error: README.md:368:2 403: Forbidden
<https://doi.org/10.1002/jrsm.5>

* Error: README.md:363:2 404: Not Found
<https://doi.org/10.1037/1082-989X.6.3.203>

The flagged URLs and DOI are correct.




