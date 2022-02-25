

#' @title Power plot function
#'
#' @param J Insert
#' @param tau2 Insert
#' @param omega2 Insert
#' @param beta Insert
#' @param rho Insert
#' @param k_mean Insert
#' @param model Insert
#' @param var_df Insert
#' @param sigma2_method Insert
#' @param pilot_data_kjsigma2 Insert
#' @param alpha Insert
#' @param iterations Insert
#' @param power_min Insert
#' @param expected_studies Insert
#' @param seed Insert
#' @param ylab Insert
#' @param xlab Insert
#' @param Numbers Insert
#' @param number_size Insert
#' @param color Insert
#' @param color_brewer Insert
#' @param brewer_type Insert
#' @param palette Insert
#' @param scales_free Insert
#' @param grid Insert
#' @param breaks Insert
#' @param limits Insert
#' @param legend_position Insert
#'
#' @return Returns a facet_grid power plot
#' @export
#'
#'
#' @importFrom magrittr %>%
#' @importFrom stats df cor
#' @import dplyr



power_plot <- function(
  # Approximation features
  J, tau2, omega2, beta, rho, k_mean = NULL,
  model = "CHE",
  var_df = "RVE",

  # Sample size methods
  #TODO

  # Sampling variance methods
  # Add more options
  sigma2_method = "empirical",
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
  legend_position = "bottom"

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
  if ("empirical" %in% sigma2_method & is.null(pilot_data_kjsigma2)) stop("Enter pilot data")
  #if (is.null(expected_studies)) stop("Indicate the number or interval of the expected numbers of studies in model")

  # ADD CE AND MLMA PLUS EMPIRICAL AND SIMULATED SAMPLE SIZES AND BALANCED SAMPLE SIZES AND SIGMA2JS

  if ("CHE" %in% model){


    dat <-
      params %>%
      mutate(
        res = purrr::pmap(.l = params,
                   .f = power_CHE,
                   var_df = var_df,
                   sigma2_method = sigma2_method,
                   pilot_data_kjsigma2 = pilot_data_kjsigma2,
                   alpha = alpha,
                   iterations = iterations,
                   seed = seed)
      ) %>%
      tidyr::unnest(res)

  }


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
