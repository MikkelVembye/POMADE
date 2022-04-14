
#' @title Finding number of studies needed to obtain 80% power
#'
#' @param mu Smallest effect size of interest/practical concern
#' @param tau2 Between-study variance
#' @param omega2 Within-study variance
#' @param rho Sample correlation
#' @param var_df Variance and degrees of freedom estimation
#' @param sigma2_method Defines how sampling variance estimates are obtained
#' @param pilot_data_kjsigma2 Pilot data containing vectors of kjs and sigma2js
#' @param alpha Specifying the nominal Type 1 error rate
#' @param target_power Specifying the target power
#' @param iterations Number of iterations (default = 100)
#' @param seed Set seed to ensure reproducibility of the iterated power approximations
#' @param interval a vector containing the end-points of the
#' interval to be searched for the root.
#' @param extendInt character string specifying if the interval c(lower,upper)
#' should be extended or directly produce an error when f() does not have
#' differing signs at the endpoints. The default, "yes",
#' keeps the search interval and hence produces an error. Can be abbreviated.
#'
#' @return Returns a \code{tibble} with information about the sampling methods, type of model,
#' the preset significant and power levels, the number of studies needed to ontained 80% power,
#' and the number of iterations
#'
#' @importFrom stats df
#' @import dplyr
#'
#' @export
#'
#' @examples
#' library(POMADE)
#' library(dplyr)
#'
#' dat_kjsigma2j <- select(VWB22_pilot, kj, sigma2j = vg_ms_mean)
#'
#' find_J_CHE(
#'   mu = 0.1,
#'   tau2 = 0.1^2,
#'   omega2 = 0.25^2,
#'   rho = 0.7,
#'   pilot_data_kjsigma2 = dat_kjsigma2j,
#'   seed = 10052510
#' )
#'

find_J_CHE <- function(
  mu,
  tau2,
  omega2,
  rho,
  var_df = "RVE",
  sigma2_method = "empirical",
  pilot_data_kjsigma2,
  alpha = .05,
  target_power = .8,
  iterations = 100,
  seed = NULL,
  interval = c(4, 100),
  extendInt = "yes") {


  f <- function(J) {

    power_CHE(J = J, tau2 = tau2, omega2 = omega2,
              beta = mu, rho = rho, alpha = alpha,
              var_df = var_df,
              sigma2_method = sigma2_method,
              iterations = iterations,
              pilot_data_kjsigma2 = pilot_data_kjsigma2,
              seed = seed)$power_sig05 - target_power

  }

  J_needed <- round(stats::uniroot(f, interval = interval, extendInt = extendInt)$root)

  res <- tibble(
    samp_method = paste(sigma2_method, "sigma2s"),
    method = paste("CHE", var_df, sep = "-"),
    alpha = alpha,
    target_power = target_power,
    J_needed = J_needed,
    iterations = iterations
  )

  res

}
