## Submission

This is the first version of POMADE. A package to conduct power analysis for overall average effects in meta-analysis with dependent effect sizes.

## Test environments

* local Windows 10 Enterprise, R 4.2.0
* local Windows 11 Pro, R 4.1.2
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

There was 1 NOTE:

* Found the following (possibly) invalid URLs:
    URL: https://doi.org/10.1002/jrsm.5
      From: README.md
      Status: 503
      Message: Service Unavailable
    URL: https://doi.org/10.1037/1082-989X.6.3.203
      From: README.md
      Status: 400
      Message: Bad Request
    URL: https://doi.org/10.3102/10769986221127379
      From: README.md
      Status: 503
      Message: Service Unavailable

The flagged URLs are correct.