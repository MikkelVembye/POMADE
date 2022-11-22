#' @param tau Between-study SD. Can be one value or a vector of multiple values.
#' @param omega Within-study SD. Can be one value or a vector of multiple
#'   values.
#' @param rho Correlation coefficient between effect size estimates from the
#'   same study. Can be one value or a vector of multiple values.
#' @param alpha Level of statistical significance. Can be one value or a vector
#'   of multiple values.
#' @param d Contrast value. Can be one value or a vector of multiple values.
#' @param model Assumed working model for dependent effect sizes, either
#'   \code{"CHE"} for the correlated-and-hierarchical effects model, \code{"CE"}
#'   for the correlated effects model, or \code{"MLMA"} for the multi-level
#'   meta-analysis model. Default is \code{"CHE"}. Can be one value or a vector
#'   of multiple values.
#' @param var_df Indicates the technique used to obtained the sampling variance
#'   of the average effect size estimate and the degrees of freedom, either
#'   \code{"Model"} for model-based variance estimator with degrees of freedom
#'   of \code{J - 1}, \code{"Satt"} for model-based variance estimator with
#'   Satterthwaite degrees of freedom, or \code{"RVE"} for robust variance
#'   estimator with Satterthwaite degrees of freedom. Default is \code{"RVE"}.
#'   Can be one value or a vector of multiple values.
#' @param sigma2_dist Distribution of sampling variance estimates from each
#'   study. Can be either a single value, a vector of plausible values, or a
#'   function that generates random values.
#' @param n_ES_dist Distribution of the number of effect sizes per study. Can be
#'   either a single value, a vector of plausible values, or a function that
#'   generates random values.
#' @param iterations Number of iterations per condition (default is 100)
#' @param seed Numerical value for a seed to ensure reproducibility of the
#'   iterated power approximations.
#' @param warning Logical indicating whether to return a warning when either
#'   sigma2_dist or n_ES_dist is based on balanced assumptions.
