#' @param model Indicates dependent effect size model, either "CHE", "CE" or "MLMA". Default = "CHE".
#' @param var_df Indicates the technique used to obtained the sampling variance of mu and the degrees of freedom either "Model", "Satt", or "RVE".
#' Default = "RVE".
#' @param sigma2_dist Distribution of sampling variance estimates from each study. Can take either one value, a function, or vector of plausible values.
#' @param n_ES_dist Distribution of the number of effect sizes per study. Can take either one value, a function, or vector of plausible values.
#' @param iterations Number of iteration per condition (default = 100).
#' @param seed Set seed to ensure reproducibility of the iterated power approximations.
#' @param warning Return warning when either sigma2_dist or n_ES_dist are based on balanced assumptions.
