#' Co-teaching dataset
#'
#' Data from a meta-analysis on the effects of collaborative models
#' of instruction on student achievement from Vembye, Weiss, and
#' Bhat (In Press).
#'
#' @format A tibble with 76 rows/studies and 9 variables
#'\describe{
#' \item{study_year}{Study author and year of publication}
#' \item{studyid}{Unique study ID}
#' \item{esid}{Unique effect size ID}
#' \item{kj}{Number of effect sizes per study}
#' \item{N_meanj}{Average sample size of studies}
#' \item{Nt_meanj}{Average size of treatment group}
#' \item{Nc_meanj}{Average size of control group}
#' \item{ESS_meanj}{Roughly approximated effective sample sizes}
#' \item{vg_ms_mean}{Cluster bias corrected sampling variances}
#'}
#'
#'@source \href{https://bit.ly/3nhVX3H}{Vembye's OSF page}
#'
#' @references Vembye, Weiss, & Bhat (In Press). The Effects
#' Co-Teaching and Related Collaborative Models of Instruction on
#' Student Achievement: A Systematic Review and Meta-Analysis. _Review of
#' Educational Research_, forthcoming. Access to background material at <https://bit.ly/3nhVX3H>
#'

"VWB22_pilot"
