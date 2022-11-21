#' Generic plot function for 'MADE' objects
#'
#' \code{plot_MADE} returns a facet_grip plot for a set of power analyses.
#'
#' @param data Data/object for which the plot should be made.
#' @param v_lines Optional integer or vector to specify vertical line(s) in facet_grid plot(s) (the default is \code{NULL}).
#' @param legend_position Optional character string to specify position of legend (default is "bottom").
#' @param color Optional logical to specify coloring of plot (default is \code{FALSE}).
#' @param numbers Optional logical to specify numbering of plots (default is \code{TRUE}).
#' @param number_size Optional integer to specify the size of the plot numbers (default is 2.5).
#' @param numbers_ynudge Optional integer to nudge number on the y-axis (default is \code{NULL}).
#' @param caption Optional logical to specify inclusion on caption with detailed information regarding
#' the given analysis (default is \code{TRUE}).
#' @param x_lab Title for the x-axis (default is \code{NULL}). If \code{NULL}, the x_lab is specified automatically
#' within the given S3 method.
#' @param x_breaks Optional sequence to specify breaks on the x-axis (default is \code{NULL}).
#' @param x_limits Optional vector to specify the limits of the x-axis (default is \code{NULL}).
#' @param y_breaks Optional sequence to specify breaks on the y-axis (default is \code{NULL}).
#' @param y_limits Optional vector to specify the limits of the y-axis (default is \code{NULL}).
#' @param y_expand Optional vector to expand the limits of the y-axis.
#' @param warning Optional logical to specify if warnings should be returned  (default is \code{TRUE}).
#' @param traffic_light_assumptions Optional logical to specify coloring of strips of the facet grids to emphasize
#' assumptions about the likelihood the given analytical scenario (default is \code{NULL}).
#' See Vembye, Pustejovsky, & Pigott (In preparation) for further details.
#' @param ... Additional arguments available for some classes of objects.
#'
#' @description This is a generic function to make facet_grid plots, with specific methods defined for
#' \code{\link{power_MADE}}, \code{\link{mdes_MADE}}, and
#' \code{\link{min_studies_MADE}} objects.
#'
#' @details In general, it can be rather difficult to guess/approximate the true model parameters
#' and sample characteristics a priori. Making only a single power approximation can easily be misleading
#' even if the true model and data structure slightly diverge
#' from the yielded data and model assumptions.
#' To maximize the informativeness of the power approximations,
#' Vembye, Pustejovsky, & Pigott (In preparation) suggest accommodating the uncertainty of the power approximations
#' by reporting or plotting power estimates across a range of possible scenarios, which can be
#' done by \code{plot_MADE}.
#'
#' @references Vembye, M. H., Pustejovsky, J. E., & Pigott, T. D. (In preparation).
#' Conducting power analysis for meta-analysis of dependent effect sizes: Common guidelines
#' and an Introduction to the POMADE R package.
#'
#' @return A \code{ggplot} object
#'
#' @seealso \code{\link{plot_MADE.power}}
#'
#' @examples
#'
#' power_dat <-
#'   power_MADE(
#'     J = seq(40, 60, 5),
#'     mu = 0.1,
#'     tau = c(0.05, 0.1, 0.2),
#'     omega = c(0.1, 0.2),
#'     rho = c(0.2, 0.7),
#'     sigma2_dist = \(x) rgamma(x, shape = 5, rate = 10),
#'     n_ES_dist = \(x) 1 + stats::rpois(x, 5.5 - 1),
#'     iterations = 5, # default is 100
#'     seed = 10052510
#'   )
#'
#' power_example <-
#'   plot_MADE(
#'    data = power_dat,
#'    power_min = 0.8,
#'    expected_studies = c(45, 55),
#'    warning = FALSE,
#'    caption = TRUE,
#'    color = TRUE,
#'    model_comparison = FALSE,
#'    #traffic_light_assumptions = c("unlikely", "likely", "expected", "likely", "expected")
#'    )
#'
#' power_example
#'
#'
#' @export

plot_MADE <-
  function(
    data,
    v_lines,
    legend_position,
    color,
    numbers,
    number_size,
    numbers_ynudge,
    caption,
    x_lab,
    x_breaks,
    x_limits,
    y_breaks,
    y_limits,
    y_expand = NULL,
    warning,
    traffic_light_assumptions,
    ...
  ) UseMethod("plot_MADE")

#' @export

plot_MADE.default <-
  function(
    data,
    v_lines = NULL,
    legend_position = "bottom",
    color = FALSE,
    numbers = TRUE,
    number_size = 2.5,
    numbers_ynudge = NULL,
    caption = TRUE,
    x_lab = NULL,
    x_breaks = NULL,
    x_limits = NULL,
    y_breaks = NULL,
    y_limits = NULL,
    y_expand = NULL,
    warning = TRUE,
    traffic_light_assumptions,
    ...
  ) {

    warning(paste0("plot_MADE does not know how to handle object of class ", class(data),
                  ". It can only be used on objects of class 'power', 'mdes', or 'min_studies'."))

  }

plot_MADE_engine <-
  function(
    data,
    x,
    y,
    x_grid,
    y_grid,
    color = NULL,
    shape = NULL,
    linetype = NULL,
    h_lines = NULL,
    v_lines = NULL,
    v_shade = NULL,
    x_breaks = NULL,
    y_breaks = ggplot2::waiver(),
    x_limits = NULL,
    y_limits = NULL,
    y_expand = NULL,
    x_lab = NULL,
    y_lab = NULL,
    color_lab = NULL,
    shape_lab = NULL,
    line_lab = NULL,
    caption = NULL,
    legend_position = "bottom",
    grid_labs = TRUE,
    labs_ynudge = 0.05,
    labs_size = 2.5,
    shape_scale = NULL,
    assumptions = NULL
  ) {

    # pre-process color, shape, and linetype
    data <-
      data |>
      mutate(
        across(c({{color}}, {{shape}}, {{linetype}}), as.factor)
      )

    # define aesthetics
    color_plot <- ggplot2::aes(x = {{x}}, y = {{y}}, color = {{color}}, shape = {{shape}}, linetype = {{linetype}})


    # handle x-limits and line breaks
    if (is.null(x_limits)) {
      x_limits <- data |> dplyr::pull({{x}}) |> range()
    }
    if (is.null(x_breaks)) {
      x_breaks <- data |> dplyr::distinct({{x}}) |> pull({{x}})
    }
    x_scale <- ggplot2::scale_x_continuous(breaks = x_breaks, limits = x_limits)


    # handle y-limits and line breaks
    y_scale <- ggplot2::scale_y_continuous(breaks = y_breaks, limits = y_limits)

    # handle y expansion
    if (is.null(y_expand)) {
      expansion <- ggplot2::expand_limits(y = y_expand)
    } else {
      expansion <- NULL
    }


    # horizontal lines

    if (!is.null(h_lines)) {
      hlines <- ggplot2::geom_hline(yintercept = h_lines, linetype = "dashed")
    } else {
      hlines <- NULL
    }

    # vertical lines

    if (!is.null(v_lines)) {
      vlines <- ggplot2::geom_vline(xintercept = v_lines, linetype = "dashed")
    } else {
      vlines <- NULL
    }


    # vertical shading

    if (!is.null(v_shade)) {
      if (length(v_shade) != 2 || !is.numeric(v_shade)) stop("v_shade must be a vector with two values.")
      gray_shade <-
        ggplot2::geom_rect(
          ggplot2::aes(
            xmin = v_shade[1],
            xmax = v_shade[2],
            ymin = -Inf,
            ymax = Inf
          ),
          fill = "gray90",
          color = "gray90",
          alpha = 0.1
        )
    } else {
      gray_shade <- NULL
    }

    # labels and captions
    plot_labs <- ggplot2::labs(
      x = x_lab,
      y = y_lab,
      color = color_lab,
      shape = shape_lab,
      linetype = line_lab,
      caption = caption
    )

    # grid labeling

    if (grid_labs) {

      if (is.null(x_limits)) {
        x_max <- data |> pull({{x}}) |> max()
      } else {
        x_max <- max(x_limits)
      }

      if (is.null(y_limits)) {
        y_min <- data |> pull({{y}}) |> min()
      } else {
        y_min <- min(y_limits)
      }

      df_text <-
        data |>
        group_by({{x_grid}}, {{y_grid}}) |>
        summarise(
          across(c({{color}}, {{shape}}, {{linetype}}), ~ unique(.x)[1]),
          .groups = "drop",
        ) |>
        arrange({{x_grid}}, {{y_grid}}) |>
        mutate(
          x = x_max,
          y = y_min + labs_ynudge,
          label = paste0("(", row_number(), ")")
        )

      text_labs <-
        ggplot2::geom_text(
          data = df_text,
          ggplot2::aes(
            x = x,
            y = y,
            label = label
          ),
          size = labs_size,
          color = "black"
        )
    } else {
      text_labs <- NULL
    }


    # handle user input for shape scales
    if (is.null(shape_scale)) {
     shape_scale_manual <- NULL
    } else if (shape_scale == "model") {
     model_shapes <- c(`CHE-RVE` = "square", `CHE-Model+Satt` = "diamond", `CHE-Model` = "square open",
                       `MLMA-RVE` = "triangle", `MLMA-Model+Satt` = "triangle open", `MLMA-Model` = "triangle down open",
                       `CE-RVE` = "circle")
     shape_scale_manual <- ggplot2::scale_shape_manual(values = model_shapes)
    } else {
     shape_scale_manual <- ggplot2::scale_shape_manual(values = shape_scale)
    }

    plot <-
      ggplot2::ggplot(data = data) +
      ggplot2::facet_grid(rows = vars({{y_grid}}), cols = vars({{x_grid}})) +
      color_plot +
      gray_shade +
      hlines +
      vlines +
      text_labs +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::theme_bw() +
      x_scale +
      y_scale +
      shape_scale_manual +
      ggplot2::theme(
        legend.position = legend_position,
        plot.caption.position = "plot",
        plot.caption = ggplot2::element_text(hjust = 0)
      ) +
      plot_labs


    if (is.null(assumptions)) {
      return(plot)
    } else {
      plot <- traffic_light_engine(plot = plot, assumptions = assumptions)
      return(plot)
    }


}

traffic_light_engine <-
  function(plot, assumptions) {

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
