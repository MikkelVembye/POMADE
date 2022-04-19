
#' @title Minimum Detectable Effect Size (MDES) for the CHE model
#'
#' @template mdes-arg
#'
#' @return Returns a \code{tibble} with information about the sampling methods, type of model,
#' the preset significant and power levels, the minimum detectable effect size under given
#' data and model conditions, and the number of iterations
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
#' MDES_CHE(
#'  J = 60,
#'  var_df = "RVE",
#'  tau2 = 0.2^2,
#'  omega2 = 0.1^2,
#'  rho = 0.7,
#'  pilot_data_kjsigma2 = dat_kjsigma2j,
#'  seed = 08042022
#')


MDES_CHE <- function(
  J,
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
  interval = c(0,2),
  extendInt = "no") {


  f <- function(mu) {

    power_CHE(J = J, tau2 = tau2, omega2 = omega2,
              beta = mu, rho = rho, alpha = alpha,
              var_df = var_df,
              sigma2_method = sigma2_method,
              iterations = iterations,
              pilot_data_kjsigma2 = pilot_data_kjsigma2,
              seed = seed)$power_sig05 - target_power

  }


  res <- tibble(
    samp_method = paste(sigma2_method, "sigma2s"),
    method = paste("CHE", var_df, sep = "-"),
    N_studies = J,
    alpha = alpha,
    target_power = target_power,
    MDES = stats::uniroot(f, interval = interval, extendInt = extendInt)$root,
    iterations = iterations
  )

  res

}
