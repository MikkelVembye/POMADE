% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-documentation.R
\docType{data}
\name{VWB23_pilot}
\alias{VWB23_pilot}
\title{Co-Teaching Dataset}
\format{
A tibble with 76 rows/studies and 9 variables
\describe{
\item{study_year}{Study author and year of publication}
\item{studyid}{Unique study ID}
\item{esid}{Unique effect size ID}
\item{kj}{Number of effect sizes per study}
\item{N_meanj}{Average sample size of study}
\item{Nt_meanj}{Average sample size of treatment group within study}
\item{Nc_meanj}{Average sample size of control group within study}
\item{ESS_meanj}{Roughly approximated effective sample sizes}
\item{vg_ms_mean}{Average cluster bias corrected sampling variance estimates}
}
}
\source{
Find background material on \href{https://osf.io/fby7w/}{Vembye's OSF page},
and the preprint at \url{https://osf.io/preprints/metaarxiv/mq5v7/}.
}
\usage{
VWB23_pilot
}
\description{
Data from a meta-analysis on the effects of collaborative models
of instruction on student achievement from Vembye, Weiss, and
Bhat (2023).
}
\references{
Vembye, M. H., Weiss, F., & Bhat, B. H. (2023). The Effects
Co-Teaching and Related Collaborative Models of Instruction on
Student Achievement: A Systematic Review and Meta-Analysis. \emph{Review of
Educational Research}, \doi{10.3102/00346543231186588}
}
\keyword{datasets}
