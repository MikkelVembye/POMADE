
#' @param J Expected number of studies to be found
#' @param tau Between-study SD
#' @param omega Within-study SD
#' @param beta Smallest effect size of practical/substantial concern
#' @param rho Sample correlation
#' @param var_df Variance and degrees of freedom estimation
#' @param sample_size_method Defines how sample sizes are obtained
#' @param sigma2_method Defines how sampling variance estimates are obtained
#' @param k_mean Average number of effect sizes per study
#' @param N_mean Average (effective) sample size of study
#' @param N_dist Simulated distribution o
#' @param pilot_data_kjN Pilot data containing kjs and effective sample sizes
#' @param sigma2_mean Average sampling variance
#' @param pilot_data_kjsigma2 Pilot data containing vectors of kjs and sigma2js
#' @param alpha statistical significance level
#' @param iterations Number of iterations (default = 100)
#' @param average_power Average power across the x iterated power estimations
#' @param seed Set seed to ensure reproducibility of the iterated power approximations
