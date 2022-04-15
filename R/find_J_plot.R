
#' @title Number of studies needed to find smallest effect size of interests
#'
#' @param mu Smallest effect size of interest/practical concern
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
#' @param legend_position Control legend position
#'
#' @param J_data If \code{TRUE} it returns data behind plot
#'
#' @return If length of mu equal one it return a facet wrap plot displaying
#' the number of studies needed to find the specific value of mu. If length of
#' mu if above 1, it returns a facet grid plot showing the number of studies needed
#' varying across mu
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
#' find_J_plot(
#'
#'   mu = 0.1,
#'   tau2 = c(0, 0.05, 0.1, 0.2)^2,
#'   omega2 = c(0.05, 0.15, 0.25, 0.35)^2,
#'   rho = c(.2, .4, .7, .9),
#'   pilot_data_kjsigma2 = dat_kjsigma2j,
#'   iterations = 1,
#'   seed = 10052510
#'
#' )


find_J_plot <-
  function(
    mu,
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
    interval = c(4,100),
    extendInt = "yes",


    # Plot features
    ylab = NULL,
    xlab = NULL,
    Numbers = TRUE,
    number_size = 3,
    color = FALSE,
    color_brewer = FALSE,
    brewer_type = "qual",
    palette = 2,
    scales_free = FALSE,
    grid = TRUE,
    legend_position = "bottom",

    J_data = FALSE

  ){

    design_factor <-
      list(

        mu = mu,
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
                            .f = find_J_CHE,
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


    if (J_data) { return(dat) }

    plot_dat <-
      dat %>%
      mutate(

        tau = sqrt(tau2),
        omega = sqrt(omega2),
        cor = factor(rho),

        tau_name = factor(paste("Study level SD =", round(tau, 3))),
        omega_name = factor(paste("ES level SD =", round(omega, 3)))

      )

    if (length(mu) == 0) {stop("Specify the smalles es of interest")}

    if (length(mu) == 1) {


      if (color){

        J_plot <-
          plot_dat %>%
          ggplot2::ggplot(ggplot2::aes(x = omega, y = J_needed, colour = cor, shape = cor, linetype = cor)) +
          ggplot2::geom_point() +
          ggplot2::geom_line()

      } else(

        J_plot <-
          plot_dat %>%
          ggplot2::ggplot(ggplot2::aes(x = omega, y = J_needed, shape = cor, linetype = cor))+
          ggplot2::geom_point() +
          ggplot2::geom_line()

      )



      if (color & color_brewer){

        J_plot <-
          J_plot +
          ggplot2::scale_color_brewer(type = brewer_type, palette = palette)


      } else {

        J_plot <- J_plot



      }

      if (scales_free) {

        J_plot <-
          J_plot +
          ggplot2::facet_wrap(vars(tau_name), scales = "free") +
          ggplot2::theme_bw() +
          ggplot2::scale_x_continuous(breaks = sqrt(omega2))

      } else {

        J_plot <-
          J_plot +
          ggplot2::facet_wrap(vars(tau_name)) +
          ggplot2::theme_bw() +
          ggplot2::scale_x_continuous(breaks = sqrt(omega2))

      }

      if (!is.null(ylab)){

        ylab <- ylab

      } else{

        ylab <- paste0("Studies needed to find mu = ", mu, " (alpha = ",
                       alpha, ", Power = ", target_power, ")")

      }

      if (!is.null(xlab)){

        xlab <- xlab

      } else{

        xlab <- "ES level SD"

      }


      if (grid){

        J_plot <-
          J_plot +
          ggplot2::theme(legend.position = legend_position) +
          ggplot2::labs(x = xlab, y = ylab)

      } else {

        J_plot <-
          J_plot +
          ggplot2::theme(legend.position = legend_position, panel.grid = ggplot2::element_blank()) +
          ggplot2::labs(x = xlab, y = ylab)

      }

    } else  {

      if (color){

        J_plot <-
          plot_dat %>%
          ggplot2::ggplot(ggplot2::aes(x = mu, y = J_needed, colour = cor, shape = cor, linetype = cor)) +
          ggplot2::geom_point() +
          ggplot2::geom_line()

      } else(

        J_plot <-
          plot_dat %>%
          ggplot2::ggplot(ggplot2::aes(x = mu, y = J_needed, shape = cor, linetype = cor))+
          ggplot2::geom_point() +
          ggplot2::geom_line()

      )



      if (color & color_brewer){

        J_plot <-
          J_plot +
          ggplot2::scale_color_brewer(type = brewer_type, palette = palette)


      } else {

        J_plot <- J_plot



      }

      if (scales_free) {

        J_plot <-
          J_plot +
          ggplot2::facet_grid(omega_name ~ tau_name, scales = "free") +
          ggplot2::theme_bw() +
          ggplot2::scale_x_continuous(breaks = mu)


      } else {

        J_plot <-
          J_plot +
          ggplot2::facet_grid(omega_name ~ tau_name) +
          ggplot2::theme_bw() +
          ggplot2::scale_x_continuous(breaks = mu)

      }

      if (!is.null(ylab)){

        ylab <- ylab

      } else{

        ylab <- paste0("Number of studies needed (alpha = ", alpha, ", Power = ", target_power, ")")

      }

      if (!is.null(xlab)){

        xlab <- xlab

      } else{

        xlab <- "Smallest ES of interest"

      }


      if (grid){

        J_plot <-
          J_plot +
          ggplot2::theme(legend.position = legend_position) +
          ggplot2::labs(x = xlab, y = ylab)

      } else {

        J_plot <-
          J_plot +
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

        J_plot <- J_plot +
          ggplot2::geom_text(data = df_text, ggplot2::aes(x = min(mu), y = min(plot_dat$J_needed),
                             label = label), size = number_size, color = "black")

      } else {

        J_plot

      }

    }

  J_plot

}
