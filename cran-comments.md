## Submission

This is the first version of POMADE, which provides functions for conducting power analysis for overall average effects in meta-analysis with dependent effect sizes. In this submission, we have reduced the unit test computation times by over 50%, so that the total time for checking the package should now be less than 10 minutes.

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

There were 2 NOTEs:

* ONLY on win-builder oldrelease: Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.1037/1082-989X.6.3.203
    From: man/power_MADE.Rd
    Status: 400
    Message: Bad Request
  URL: https://doi.org/10.3102/10769986221127379
    From: man/power_MADE.Rd
    Status: 503
    Message: Service Unavailable

The flagged URLs are correct.

* ONLY on Fedora Linux (r-hub): checking HTML version of manual ... NOTE Skipping checking   HTML validation: no command 'tidy' found. 

We are not able to change that Tidy is not on the path, or update Tidy on the external Fedora Linux server.
