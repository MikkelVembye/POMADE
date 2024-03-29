% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tau2_approximation.R
\name{tau2_approximation}
\alias{tau2_approximation}
\title{Between-Study Variance Approximation Function}
\usage{
tau2_approximation(sample_size = 100, es, df_minus2 = TRUE)
}
\arguments{
\item{sample_size}{Typical sample size of studies}

\item{es}{Smallest effect size of practical concern}

\item{df_minus2}{If degrees of freedom should be df-2 or just df}
}
\value{
A \code{tibble} with small, medium, and large magnitudes of tau2
}
\description{
Rough approximation of the between-study variance based on
assumption about the typical sample size of studies included in the
synthesis
}
\examples{

tau2_approximation(
sample_size = 50,
es = 0.1,
df_minus2 = TRUE
)


}
