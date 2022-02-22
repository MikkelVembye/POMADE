
#' Approximate effective sample sizes
#'
#' @param sample_sizes_raw Takes a vector of the raw total study sample size(s)
#' @param Nt_raw Takes a vector of raw treatment group sample size(s)
#' @param Nc_raw Takes a vector of raw control group sample size(s)
#' @param cluster_size Average cluster size (Default = 22. Common class size in education)
#' @param icc Assumed intraclass correlation (Default = 0.22. Average ICC value in Hedges & Hedberg (2007) unconditional models)
#'
#' @details \code{N_j/DE}
#'
#'
#' @return Returns a vector of effective sample sizes
#' @export
#'
#' @examples
#' sample_sizes <- sample(50:1000, 50, replace = TRUE)
#' effective_sample_sizes(sample_sizes_raw = sample_sizes)
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
