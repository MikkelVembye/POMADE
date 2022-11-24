
#' @title Cluster Bias Correction
#'
#' @description Function to conduct cluster bias correction of sampling variance estimates
#' obtained from cluster-randomized studies in which the reported variance does not account for clustering.
#'
#' @param sigma2js A vector of sampling variance estimates that do not account for clustering.
#' @param cluster_size A numerical value for average cluster size.
#' @param icc Assumed intra-class correlation (proportion of total variance at the cluster level).
#'
#' @return Returns a vector of cluster bias adjusted variance estimates
#' @export
#'
#' @examples
#' cbc_var <- cluster_bias_adjustment(
#'   sigma2js = c(0.04, 0.06, 0.08, 0.1),
#'   cluster_size = 15,
#'   icc = 0.15
#' )
#'
#' cbc_var
#'
#'



cluster_bias_adjustment <-
  function(
    sigma2js,
    cluster_size = 22,
    icc = 0.2){

    design_effect <- 1 + (cluster_size - 1) * icc


    round(sigma2js * design_effect, 3)


  }
