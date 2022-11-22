
#' Approximate effective sample sizes
#'
#' @param sample_sizes_raw Vector of the raw total study sample size(s).
#' @param Nt_raw Vector of raw treatment group sample size(s).
#' @param Nc_raw Vector of raw control group sample size(s).
#' @param cluster_size Average cluster size (Default = 22, a common class size in education research studies).
#' @param icc Assumed intra-class correlation (Default = 0.22, the average ICC value in Hedges & Hedberg (2007) unconditional models)
#'
#' @details \code{N_j/DE}
#'
#'
#' @return A vector of effective sample sizes, adjusted for cluster-dependence.
#' @export
#'
#' @examples
#' sample_sizes <- sample(50:1000, 50, replace = TRUE)
#' effective_sample_sizes(
#'   sample_sizes_raw = sample_sizes,
#'   cluster_size = 20,
#'   icc = 0.15
#' )
#'
#'


effective_sample_sizes <-
  function(
    sample_sizes_raw = NULL,
    Nt_raw = NULL, Nc_raw = NULL,
    cluster_size = 22, icc = 0.22){

    design_effect <- 1 + (cluster_size - 1) * icc

    if (!is.null(sample_sizes_raw)){

      ess <- round(sample_sizes_raw/design_effect)

    }

    if (!is.null(Nt_raw) & !is.null(Nc_raw)){

      sample_size <- Nt_raw + Nc_raw

      ess <- round(sample_sizes_raw/design_effect)

    }

    ess

  }
