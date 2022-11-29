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

## R CMD check results

There were no ERRORs, WARNINGs 

There were 2 NOTES:

* Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.1037/1082-989X.6.3.203
    From: README.md
    Status: 400
    Message: Bad Request
  URL: https://doi.org/10.3102/10769986221127379
    From: README.md
    Status: 503
    Message: Service Unavailable
    

The flagged URLs are correct.

* checking examples ... [25s/93s] NOTE
Examples with CPU (user + system) or elapsed time > 5s
                       user system elapsed
plot_MADE             4.897  0.075  19.262
plot_MADE.power       4.337  0.012  16.089
plot_MADE.mdes        3.956  0.008  16.588
mdes_MADE             2.928  0.044  10.806
plot_MADE.min_studies 2.762  0.008  10.226
min_studies_MADE      2.552  0.004   9.061
