

#' @title Power plot function
#'
#' @param J A sequence running from the minimum to maximum number of studies
#' expected to be found
#' @param tau2 Varying between-study variance estimates
#' @param omega2 Varying within-study variance estimates
#' @param beta Smallest effect size of practical concern. Insert one value only
#' @param rho Sample correlation coefficients
#' @param k_mean Average number of effect sizes per study
#' @param model Model (either CHE, MLMA, or CE)
#' @param var_df Indicate variance and degrees of freedom estimation
#' ("Model", "Satt", "RVE")
#' @param N_mean Average sample size
#' @param pilot_data_kjN Pilot data including a vector of kjs and sample sizes.
#' If clustering it presented in studies, use effective sample sizes.
#' @param sigma2_mean Average sampling variance
#' @param pilot_data_kjsigma2 Pilot data including a vector of kjs and sampling variance.
#' If clustering it presented in studies, this should be reflected int the sampling variance
#' estimates.
#' @param alpha Nominal level of statistical significance
#' @param iterations Number of times power should be calculated when based on
#' either simulated or empirical values for kjs and sample sizes or sampling variance estimates
#' @param power_min Minimum acceptable power level
#' @param expected_studies Range in which the expected number of studies
#' to be found is present.
#' @param seed Set seed to ensure reproducibility of the plot
#' @param ylab Change default ylab setting
#' @param xlab Change default xlab setting
#' @param Numbers Numbering plot in facet grid (Default = \code{TRUE})
#' @param number_size Size of numbers on plots
#' @param color If \code{ggplot2} colors should be used
#' @param color_brewer Customize color brewer
#' @param brewer_type Customize color brewer
#' @param palette Customize color brewer
#' @param scales_free Free scales in \code{facet_grid}
#' @param grid Control background grids
#' @param breaks Control breaks
#' @param limits Control limits
#' @param legend_position Control legend position
#' @param power_data Print approximated data. If \code{TRUE}, it returns the dataset only
#'
#' @return Returns a facet_grid power plot
#' @export
#'
#' @examples
#'
#' library(dplyr)
#'
#' coteach_dat <- VWB22_pilot
#' dat_kjsigma2j <- dplyr::select(coteach_dat, kj, sigma2j = vg_ms_mean)
#'
#' # CHE-RVE model
#'
#' power_CHE_RVE_color_plot <-
#'   power_plot(
#'     J = seq(50, 100, 10),
#'     tau2 = c(0, 0.05, 0.1, 0.2)^2,
#'     omega2 = c(0.05, 0.15, 0.25, .35)^2,
#'     beta = 0.1,
#'     rho = c(.2, .4, .7, .9),
#'     model = "CHE",
#'     var_df = "RVE",
#'     pilot_data_kjsigma2 = dat_kjsigma2j,
#'     expected_studies = c(66, 86),
#'     color = TRUE,
#'     color_brewer = TRUE,
#'     iterations = 1,
#'     seed = 10052510
#'   )
#'
#' power_CHE_RVE_color_plot
#'
#' # CE-RVE model
#'
#' power_CE_RVE_color_plot <-
#'   power_plot(
#'     J = seq(50, 100, 10),
#'     tau2 = c(0, 0.05, 0.1, 0.2)^2,
#'     omega2 = c(0.05, 0.15, 0.25, .35)^2,
#'     beta = 0.1,
#'     rho = c(.2, .4, .7, .9),
#'     model = "CE",
#'     var_df = "RVE",
#'     pilot_data_kjsigma2 = dat_kjsigma2j,
#'     expected_studies = c(66, 86),
#'     color = TRUE,
#'     color_brewer = TRUE,
#'     iterations = 1,
#'     seed = 10052510
#'   )
#'
#' power_CE_RVE_color_plot
#'
#' # MLMA-RVE model
#'
#' power_MLMA_RVE_color_plot <-
#'   power_plot(
#'     J = seq(50, 100, 10),
#'     tau2 = c(0, 0.05, 0.1, 0.2)^2,
#'     omega2 = c(0.05, 0.15, 0.25, .35)^2,
#'     beta = 0.1,
#'     rho = c(.2, .4, .7, .9),
#'     model = "MLMA",
#'     var_df = "RVE",
#'     pilot_data_kjsigma2 = dat_kjsigma2j,
#'     expected_studies = c(66, 86),
#'     color = TRUE,
#'     color_brewer = TRUE,
#'     iterations = 1,
#'     seed = 10052510
#'   )
#'
#' power_MLMA_RVE_color_plot
#'
#'
#' @importFrom magrittr %>%
#' @importFrom stats df cor
#' @import dplyr



power_plot <- function(
  # Approximation features
  J, tau2, omega2, beta, rho, k_mean = NULL,
  model = c("CHE", "MLMA", "CE"),
  var_df = "RVE",
  # Sample size methods
  N_mean = NULL,
  pilot_data_kjN = NULL,

  # Sampling variance methods
  sigma2_mean = NULL,
  pilot_data_kjsigma2 = NULL,
  alpha = .05,
  iterations = 100,
  power_min = 0.8,
  expected_studies = NULL,
  seed = NULL,

  # Plot features
  ylab = NULL,
  xlab = "Number of studies (J)",
  Numbers = TRUE,
  number_size = 2.5,
  color = FALSE,
  color_brewer = FALSE,
  brewer_type = "qual",
  palette = 2,
  scales_free = FALSE,
  grid = TRUE,
  breaks = seq(min(J), max(J), 10),
  limits = c(min(J), max(J)),
  legend_position = "bottom",

  power_data = FALSE

){

  design_factor <-
    list(

      J = J,
      tau2 = tau2,
      omega2 = omega2,
      beta = beta,
      rho = rho

    )

  params <- purrr::cross_df(design_factor)

  # Should be have a warning here?
  #if (params > 300 & iterations > 50) message("Can take a couple of minute due to the number of unique parameters and iterations")

  if (length(model) > 1) stop("Specify one model, only")
  if (length(var_df) > 1) stop("Specify one model, only")
  if (is.null(seed)) stop("Set seed to ensure reproducibility")

  # ADD  SIMULATED SAMPLE SIZES (AND SIGMA2JS?)

  if ("CE" %in% model){

    if (!is.null(k_mean) & !is.null(N_mean)){

      dat <-
        params %>%
        mutate(
          res = purrr::pmap(.l = params,
                            .f = power_CE,
                            var_df = var_df,
                            sample_size_method = "balanced",
                            k_mean = k_mean,
                            N_mean = N_mean,
                            alpha = alpha,
                            iterations = iterations,
                            seed = seed)
        ) %>%
        tidyr::unnest(res)
    }

    if (!is.null(pilot_data_kjN)){

      dat <-
        params %>%
        mutate(
          res = purrr::pmap(.l = params,
                            .f = power_CE,
                            var_df = var_df,
                            sample_size_method = "empirical",
                            pilot_data_kjN = pilot_data_kjN,
                            alpha = alpha,
                            iterations = iterations,
                            seed = seed)
        ) %>%
        tidyr::unnest(res)
    }

    # Approximation based on sigma2j methods

    if (!is.null(k_mean) & !is.null(sigma2_mean)){

      dat <-
        params %>%
        mutate(
          res = purrr::pmap(.l = params,
                            .f = power_CE,
                            var_df = var_df,
                            sigma2_method = "balanced",
                            k_mean = k_mean,
                            sigma2_mean = sigma2_mean,
                            alpha = alpha,
                            iterations = iterations,
                            seed = seed)
        ) %>%
        tidyr::unnest(res)
    }

    if (!is.null(pilot_data_kjsigma2)){

      dat <-
        params %>%
        mutate(
          res = purrr::pmap(.l = params,
                            .f = power_CE,
                            var_df = var_df,
                            sigma2_method = "empirical",
                            pilot_data_kjsigma2 = pilot_data_kjsigma2,
                            alpha = alpha,
                            iterations = iterations,
                            seed = seed)
        ) %>%
        tidyr::unnest(res)
    }

  }

  if ("MLMA" %in% model){

    if (!is.null(k_mean) & !is.null(N_mean)){

      dat <-
        params %>%
        mutate(
          res = purrr::pmap(.l = params,
                            .f = power_MLMA,
                            var_df = var_df,
                            sample_size_method = "balanced",
                            k_mean = k_mean,
                            N_mean = N_mean,
                            alpha = alpha,
                            iterations = iterations,
                            seed = seed)
        ) %>%
        tidyr::unnest(res)
    }

    if (!is.null(pilot_data_kjN)){

      dat <-
        params %>%
        mutate(
          res = purrr::pmap(.l = params,
                            .f = power_MLMA,
                            var_df = var_df,
                            sample_size_method = "empirical",
                            pilot_data_kjN = pilot_data_kjN,
                            alpha = alpha,
                            iterations = iterations,
                            seed = seed)
        ) %>%
        tidyr::unnest(res)
    }

    # Approximation based on sigma2j methods

    if (!is.null(k_mean) & !is.null(sigma2_mean)){

      dat <-
        params %>%
        mutate(
          res = purrr::pmap(.l = params,
                            .f = power_MLMA,
                            var_df = var_df,
                            sigma2_method = "balanced",
                            k_mean = k_mean,
                            sigma2_mean = sigma2_mean,
                            alpha = alpha,
                            iterations = iterations,
                            seed = seed)
        ) %>%
        tidyr::unnest(res)
    }

    if (!is.null(pilot_data_kjsigma2)){

      dat <-
        params %>%
        mutate(
          res = purrr::pmap(.l = params,
                            .f = power_MLMA,
                            var_df = var_df,
                            sigma2_method = "empirical",
                            pilot_data_kjsigma2 = pilot_data_kjsigma2,
                            alpha = alpha,
                            iterations = iterations,
                            seed = seed)
        ) %>%
        tidyr::unnest(res)
    }
  }

  if ("CHE" %in% model){

    if (!is.null(k_mean) & !is.null(N_mean)){

      dat <-
        params %>%
        mutate(
          res = purrr::pmap(.l = params,
                            .f = power_CHE,
                            var_df = var_df,
                            sample_size_method = "balanced",
                            k_mean = k_mean,
                            N_mean = N_mean,
                            alpha = alpha,
                            iterations = iterations,
                            seed = seed)
        ) %>%
        tidyr::unnest(res)
    }

    if (!is.null(pilot_data_kjN)){

      dat <-
        params %>%
        mutate(
          res = purrr::pmap(.l = params,
                            .f = power_CHE,
                            var_df = var_df,
                            sample_size_method = "empirical",
                            pilot_data_kjN = pilot_data_kjN,
                            alpha = alpha,
                            iterations = iterations,
                            seed = seed)
        ) %>%
        tidyr::unnest(res)
    }

    # Approximation based on sigma2j methods

    if (!is.null(k_mean) & !is.null(sigma2_mean)){

      dat <-
        params %>%
        mutate(
          res = purrr::pmap(.l = params,
                            .f = power_CHE,
                            var_df = var_df,
                            sigma2_method = "balanced",
                            k_mean = k_mean,
                            sigma2_mean = sigma2_mean,
                            alpha = alpha,
                            iterations = iterations,
                            seed = seed)
        ) %>%
        tidyr::unnest(res)
    }

    if (!is.null(pilot_data_kjsigma2)){

    dat <-
      params %>%
      mutate(
        res = purrr::pmap(.l = params,
                   .f = power_CHE,
                   var_df = var_df,
                   sigma2_method = "empirical",
                   pilot_data_kjsigma2 = pilot_data_kjsigma2,
                   alpha = alpha,
                   iterations = iterations,
                   seed = seed)
      ) %>%
      tidyr::unnest(res)
    }
  }

  if (power_data) { return(dat) }

  plot_dat <-
    dat %>%
    mutate(

      tau = sqrt(tau2),
      omega = sqrt(omega2),
      cor = factor(rho),

      tau_name = factor(paste("Study level SD =", round(tau, 3))),
      omega_name = factor(paste("ES level SD =", round(omega, 3)))

    )


  if (color){
    TLPP <-
      plot_dat %>%
      rename(Power = power_sig05) %>%
      ggplot2::ggplot(ggplot2::aes(x = J, y = Power, colour = cor, shape = cor, linetype = cor))

  } else(
    TLPP <-
      plot_dat %>%
      rename(Power = power_sig05) %>%
      ggplot2::ggplot(ggplot2::aes(x = J, y = Power, shape = cor, linetype = cor))

  )



  if (is.null(expected_studies)){

    TLPP <-
      TLPP +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::geom_hline(yintercept = power_min, linetype = "dashed")



  } else if (length(expected_studies) == 1){

    TLPP <-
      TLPP +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::geom_hline(yintercept = power_min, linetype = "dashed") +
      ggplot2::geom_vline(xintercept = expected_studies, linetype = "dashed")



  } else if (length(expected_studies) == 2){

    TLPP <-
      TLPP +
      ggplot2::geom_rect(ggplot2::aes(xmin = expected_studies[1], xmax = expected_studies[2],
                    ymin = -Inf, ymax = Inf),
                fill = "gray90", color = "gray90", alpha = 0.1) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::geom_hline(yintercept = power_min, linetype = "dashed")


  } else if (length(expected_studies) == 3){

    TLPP <-
      TLPP +
      ggplot2::geom_rect(ggplot2::aes(xmin = expected_studies[1], xmax = expected_studies[3],
                    ymin = -Inf, ymax = Inf),
                fill = "gray90", color = "gray90", alpha = 0.1) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::geom_hline(yintercept = power_min, linetype = "dashed") +
      ggplot2::geom_vline(xintercept = expected_studies[2], linetype = "dashed")


  }

  if (color & color_brewer){

    TLPP <-
      TLPP +
      ggplot2::scale_color_brewer(type = brewer_type, palette = palette)


  } else {

    TLPP <- TLPP



  }

  if (scales_free) {

    TLPP <-
      TLPP +
      ggplot2::facet_grid(omega_name ~ tau_name, scales = "free") +
      ggplot2::theme_bw()

  } else {

    TLPP <-
      TLPP +
      ggplot2::facet_grid(omega_name ~ tau_name) +
      ggplot2::theme_bw() +
      ggplot2::scale_x_continuous(breaks = breaks, limits = limits) +
      ggplot2::scale_y_continuous(breaks = seq(0, 1, .2), limits = c(0,1))
  }


  if (!is.null(ylab)){

    ylab <- ylab

  } else{

    ylab <- paste0("Power", " (", unique(dat$method), ")")

  }

  if (grid){

    TLPP <-
      TLPP +
      ggplot2::theme(legend.position = legend_position) +
      ggplot2::labs(x = xlab, y = ylab) +
      ggplot2::expand_limits(y = 0)

  } else {

    TLPP <-
      TLPP +
      ggplot2::theme(legend.position = legend_position, panel.grid = ggplot2::element_blank()) +
      ggplot2::labs(x = xlab, y = ylab) +
      ggplot2::expand_limits(y = 0)

  }


  if (Numbers){

    label_length <- length(tau2)*length(omega2)

    df_text <- tibble(
      tau_name = factor(rep(unique(plot_dat$tau_name), n_distinct(plot_dat$omega_name))),

      omega_name = factor(rep(unique(plot_dat$omega_name), each = n_distinct(plot_dat$tau_name))),

      label = paste0("(", 1:label_length, ")"),

      cor = factor(rho[1])
    )

    TLPP +
      ggplot2::geom_text(data = df_text, ggplot2::aes(x = max(J), y = .01, label = label),
                size = number_size, color = "black")

   } else {

    TLPP

  }

}

gc()
