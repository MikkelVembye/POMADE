# Add arguments
#' @export

plot_MADE <- function(data, power_min, es_min, expected_studies, v_shade, v_lines, legend_position, color, numbers, number_size, caption, xlab, x_breaks, x_limits, y_breaks, y_limits, warning, model_comparison, ...)
  UseMethod("plot_MADE")

#' @export

plot_MADE.default <-
  function(
    data,
    power_min = NULL,
    es_min = NULL,
    expected_studies = NULL,
    v_shade = NULL,
    v_lines = NULL,
    legend_position = "bottom",
    color = FALSE,
    numbers = TRUE,
    number_size = 2.5,
    caption = TRUE,
    x_lab = NULL,
    x_breaks = NULL,
    x_limits = NULL,
    y_breaks = NULL,
    y_limits = NULL,
    warning = TRUE,
    model_comparison,
    ...) {

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
    shape_scale_manually = FALSE
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

    if (shape_scale_manually){

      n_shapes <- data |> dplyr::distinct({{shape}}) |> pull({{shape}})
      shape_scale_manual <- ggplot2::scale_shape_manual(values = 1:n_shapes)

    } else {

      shape_scale_manual <- NULL

    }

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

  }
