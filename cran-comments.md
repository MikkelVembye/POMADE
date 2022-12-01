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

There were 2 NOTEs:

* ONLY on win-builder oldrelease: Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.1037/1082-989X.6.3.203
    From: man/power_MADE.Rd
    Status: 400
    Message: Bad Request

The flagged URL is correct.

* ONLY on Fedora Linux (r-hub): checking HTML version of manual ... NOTE Skipping checking   HTML validation: no command 'tidy' found. 

We are not able to change that Tidy is not on the path, or update Tidy on the external Fedora Linux server.
