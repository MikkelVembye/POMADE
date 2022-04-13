
#' @param J Expected number of studies to be found
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
#' differing signs at the endpoints. The default, "no",
#' keeps the search interval and hence produces an error. Can be abbreviated.
#'
