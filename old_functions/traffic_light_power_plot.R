#' @title Make traffic light power plot
#'
#' @description Traffic light coloring of the strips of a \code{power_plot}
#'
#' @param power_plot Facet grid \code{power_plot} object
#' @param assumptions Assumptions regarding the likelihood of the included scenarios
#' in the \code{power_plot}. Make assumptions from upper-left strip to lower-right strip
#'
#' @return Returns a traffic light power plot
#' @export
#'
#' @examples
#'
#' library(dplyr)
#'
#' coteach_dat <- VWB22_pilot
#' dat_kjsigma2j <- dplyr::select(coteach_dat, kj, sigma2j = vg_ms_mean)
#'
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
#' traffic_light_power_plot(
#'   power_plot = power_CHE_RVE_color_plot,
#'   assumptions = c("unlikely", "likely", "expected", "likely",
#'                   "unlikely", "likely", "expected", "unlikely")
#' )
#'



traffic_light_power_plot <-
  function(
    power_plot, assumptions

  ){



    assump <-
      dplyr::tibble(assumptions) %>%
      dplyr::mutate(
        color = dplyr::recode(
          assumptions,
          "unlikely" = "lightcoral",
          "likely" = "lightgoldenrodyellow",
          "expected" = "mediumaquamarine"
        )
      )

    g <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(power_plot))
    strip_both <- which(grepl('strip-', g$layout$name))
    fills <- assump$color

    k <- 1
    for (i in strip_both) {
      j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
      g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
      k <- k+1
    }

    traffic_plot <- g
    grid::grid.draw(traffic_plot)


  }
