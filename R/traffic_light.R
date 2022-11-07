#' @export

traffic_light <-
  function(plot, assumptions) {

    if (is.vector(plot[[1]][[1]])){

      TLP <- traffic_light_engine(plot = plot, assumptions = assumptions)

    }  else {

      TLP <- purrr::map(plot, ~ traffic_light_engine(plot = .x, assumptions = assumptions))

    }

    TLP
  }


traffic_light_engine <-
  function(plot, assumptions){

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

    g <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(plot))
    strip_both <- which(grepl('strip-', g$layout$name))
    fills <- assump$color

    k <- 1
    for (i in strip_both) {
      j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
      g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
      k <- k+1
    }

    traffic_light_plot <- g
    grid::grid.draw(traffic_light_plot)


  }
