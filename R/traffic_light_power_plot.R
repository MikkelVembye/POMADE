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
