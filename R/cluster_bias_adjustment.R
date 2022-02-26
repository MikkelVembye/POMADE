
#' @title Cluster bias correction
#'
#' @description Function to conduct cluster bias correction of variance estimates
#' obtained from cluster-studies but do not included cluster-level variation
#'
#' @param sigma2js A vector of sampling variance estimates not including cluster-level variation
#' @param cluster_size Average cluster size
#' @param icc Intraclass correlation
#'
#' @return Returns a vector of cluster bias adjusted variance estimates
#' @export
#'
#' @examples
#' cbc_var <- cluster_bias_adjustment(
#' sigma2js = c(0.04, 0.06, 0.08, 0.1),
#' cluster_size = 15, icc = 0.15
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
