

#' @title Minimum Detectable Effect Size (MDES) plot function
#'
#' @param J A sequence running from the minimum to maximum number of studies
#' expected to be found
#' @param tau2 Varying between-study variance estimates
#' @param omega2 Varying within-study variance estimates
#' @param rho Sample correlation coefficients
#' @param model Model (default CHE)
#' @param var_df Indicate variance and degrees of freedom estimation
#' ("Model", "Satt", "RVE")
#' @param pilot_data_kjsigma2 Pilot data including a vector of kjs and sampling variance.
#' If clustering it presented in studies, this should be reflected int the sampling variance
#' estimates.
#' @param alpha Nominal level of statistical significance
#' @param target_power Preset power level
#' @param iterations Number of times power should be calculated when based on
#' either simulated or empirical values for kjs and sample sizes or sampling variance estimates
#' @param seed Set seed to ensure reproducibility of the plot
#' @param interval a vector containing the end-points of the interval to be searched for the root.
#' @param extendInt character string specifying if the interval c(lower,upper)
#' should be extended or directly produce an error when f() does not have
#' differing signs at the endpoints. The default, "no", keeps the search
#' interval and hence produces an error. Can be abbreviated.
#'
#' @param min_ES_pr Smallest effect size of practical relevance
#' @param expected_studies Range in which the expected number of studies
#' to be found is present.
#'
#'
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
#'
#' @param mdes_data Returns data behind plot
#'
#'
#'
#' @return Returns a facet grid plot with minimum detectable effect sizes
#' @importFrom magrittr %>%
#' @importFrom stats df cor
#' @import dplyr
#'
#' @export
#'
#' @examples
#' library(POMADE)
#' library(dplyr)
#'
#' dat_kjsigma2j <- select(VWB22_pilot, kj, sigma2j = vg_ms_mean)
#'
#' MDES_plot(
#'
#'   J = seq(50, 100, 10),
#'   tau2 = c(0, 0.05, 0.1, 0.2)^2,
#'   omega2 = c(0.05, 0.15, 0.25, 0.35)^2,
#'   rho = c(.2, .4, .7, .9),
#'   pilot_data_kjsigma2 = dat_kjsigma2j,
#'   iterations = 1, # Default = 100
#'   seed = 13042022,
#'   expected_studies = c(66, 86)
#'
#' )


MDES_plot <-
  function(
    J,
    tau2,
    omega2,
    rho,
    model = "CHE",
    var_df = "RVE",
    pilot_data_kjsigma2,
    alpha = .05,
    target_power = .8,
    iterations = 100,
    seed = NULL,
    interval = c(0,2),
    extendInt = "no",

    min_ES_pr = 0.1,
    expected_studies = NULL,


    # Plot features
    ylab = paste0("Minimum detecable ES (alpha = ", alpha, ", Power = ", target_power, ")"),
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

    mdes_data = FALSE

  ){

    design_factor <-
      list(

        J = J,
        tau2 = tau2,
        omega2 = omega2,
        rho = rho

      )

    params <- purrr::cross_df(design_factor)

    if ("CHE" %in% model) {

      dat <-
        params %>%
        dplyr::mutate(
          res = purrr::pmap(.l = params,
                            .f = MDES_CHE,
                            var_df = var_df,
                            sigma2_method = "empirical",
                            pilot_data_kjsigma2 = pilot_data_kjsigma2,
                            alpha = alpha,
                            target_power = target_power,
                            iterations = iterations,
                            seed = seed,
                            interval = interval,
                            extendInt = extendInt
          )
        ) %>%
        tidyr::unnest(res)
    }


    if (mdes_data) { return(dat) }

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

      mdes_plot <-
        plot_dat %>%
        ggplot2::ggplot(ggplot2::aes(x = J, y = MDES, colour = cor, shape = cor, linetype = cor))

    } else(

      mdes_plot <-
        plot_dat %>%
        ggplot2::ggplot(ggplot2::aes(x = J, y = MDES, shape = cor, linetype = cor))

    )



    if (is.null(expected_studies)){

      mdes_plot <-
        mdes_plot +
        ggplot2::geom_point() +
        ggplot2::geom_line() +
        ggplot2::geom_hline(yintercept = min_ES_pr, linetype = "dashed")



    } else if (length(expected_studies) == 1){

      mdes_plot <-
        mdes_plot +
        ggplot2::geom_point() +
        ggplot2::geom_line() +
        ggplot2::geom_hline(yintercept = min_ES_pr, linetype = "dashed") +
        ggplot2::geom_vline(xintercept = expected_studies, linetype = "dashed")



    } else if (length(expected_studies) == 2){

      mdes_plot <-
        mdes_plot +
        ggplot2::geom_rect(ggplot2::aes(xmin = expected_studies[1], xmax = expected_studies[2],
                                        ymin = -Inf, ymax = Inf),
                           fill = "gray90", color = "gray90", alpha = 0.1) +
        ggplot2::geom_point() +
        ggplot2::geom_line() +
        ggplot2::geom_hline(yintercept = min_ES_pr, linetype = "dashed")


    } else if (length(expected_studies) == 3){

      mdes_plot <-
        mdes_plot +
        ggplot2::geom_rect(ggplot2::aes(xmin = expected_studies[1], xmax = expected_studies[3],
                                        ymin = -Inf, ymax = Inf),
                           fill = "gray90", color = "gray90", alpha = 0.1) +
        ggplot2::geom_point() +
        ggplot2::geom_line() +
        ggplot2::geom_hline(yintercept = min_ES_pr, linetype = "dashed") +
        ggplot2::geom_vline(xintercept = expected_studies[2], linetype = "dashed")


    }

    if (color & color_brewer){

      mdes_plot <-
        mdes_plot +
        ggplot2::scale_color_brewer(type = brewer_type, palette = palette)


    } else {

      mdes_plot <- mdes_plot



    }

    if (scales_free) {

      mdes_plot <-
        mdes_plot +
        ggplot2::facet_grid(omega_name ~ tau_name, scales = "free") +
        ggplot2::theme_bw()

    } else {

      mdes_plot <-
        mdes_plot +
        ggplot2::facet_grid(omega_name ~ tau_name) +
        ggplot2::theme_bw() +
        ggplot2::scale_x_continuous(breaks = breaks, limits = limits) +
        ggplot2::scale_y_continuous(
          breaks = seq(round(min(plot_dat$MDES) - 0.015, 2) , round(max(plot_dat$MDES) + 0.015, 2), .02),
          limits = c(round(min(plot_dat$MDES) - 0.015, 2) , round(max(plot_dat$MDES) + 0.015, 2) )
          )
    }


    if (grid){

      mdes_plot <-
        mdes_plot +
        ggplot2::theme(legend.position = legend_position) +
        ggplot2::labs(x = xlab, y = ylab)

    } else {

      mdes_plot <-
        mdes_plot +
        ggplot2::theme(legend.position = legend_position, panel.grid = ggplot2::element_blank()) +
        ggplot2::labs(x = xlab, y = ylab)

    }


    if (Numbers){

      label_length <- length(tau2)*length(omega2)

      df_text <- tibble(
        tau_name = factor(rep(unique(plot_dat$tau_name), n_distinct(plot_dat$omega_name))),

        omega_name = factor(rep(unique(plot_dat$omega_name), each = n_distinct(plot_dat$tau_name))),

        label = paste0("(", 1:label_length, ")"),

        cor = factor(rho[1])
      )

      mdes_plot +
        ggplot2::geom_text(data = df_text, ggplot2::aes(x = max(J), y = round(max(plot_dat$MDES) + 0.015, 2),
                           label = label), size = number_size, color = "black")

    } else {

      mdes_plot

    }

}
