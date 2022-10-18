# Add arguments

plot_MADE <-
  function(
    data,
    power_min = NULL,
    MDES_min = NULL,
    expected_studies = NULL,
    breaks = NULL,
    limits = NULL,
    legend_position = "bottom",
    color = FALSE,
    numbers = TRUE,
    number_size = 2.5,
    caption = TRUE,
    warning = TRUE
  ){

  if (warning){
    if(n_distinct(data$model) > 1){
      warning("We recommend to create the plot for one model only", call. = FALSE)
    }
  }

  plot_dat <-
    data |>
    mutate(

      tau = sqrt(tau2),
      omega = sqrt(omega2),
      cor = factor(rho),

      tau_name = factor(paste("Study level SD =", round(tau, 3))),
      omega_name = factor(paste("ES level SD =", round(omega, 3)))

   )

  if (is.null(breaks)){
    breaks <- unique(plot_dat$J)
  } else {
    breaks <- breaks
  }

  if (is.null(limits)){
    limits <- c(min(plot_dat$J), max(plot_dat$J))
  } else {
    limits <- limits
  }

  xlab <- "Number of Studies (J)"

  if (any(colnames(data) == "power")){

    if (color){
      color_plot <- ggplot2::aes(x = J, y = power, colour = cor, shape = cor, linetype = cor)
    } else {
      color_plot <- ggplot2::aes(x = J, y = power, shape = cor, linetype = cor)
    }


    if (is.null(expected_studies)){

      gray_shade <- NULL
      vline <- NULL


    } else if (length(expected_studies) == 1){

      gray_shade <- NULL
      vline <- ggplot2::geom_vline(xintercept = expected_studies, linetype = "dashed")


    } else if (length(expected_studies) == 2){

      gray_shade <- ggplot2::geom_rect(
                    ggplot2::aes(xmin = expected_studies[1], xmax = expected_studies[2],
                     ymin = -Inf, ymax = Inf), fill = "gray90", color = "gray90", alpha = 0.1)

      vline <- NULL


    } else if (length(expected_studies) == 3){

      gray_shade <- ggplot2::geom_rect(
                      ggplot2::aes(xmin = expected_studies[1], xmax = expected_studies[3],
                      ymin = -Inf, ymax = Inf), fill = "gray90", color = "gray90", alpha = 0.1)

      vline <- ggplot2::geom_vline(xintercept = expected_studies[2], linetype = "dashed")

    } else if (length(expected_studies) > 3) {

      stop("expected_studies must not exceed 3 values")

    }


    if (caption){

    plot <-
      plot_dat |>
      group_by(mu, d, alpha, model) |>
      group_map(
        .f = ~ ggplot2::ggplot(data = .x) +
          color_plot +
          gray_shade +
          ggplot2::geom_point() +
          ggplot2::geom_line() +
          ggplot2::geom_hline(yintercept = power_min, linetype = "dashed") +
          vline +
          ggplot2::facet_grid(omega_name ~ tau_name) +
          ggplot2::theme_bw() +
          ggplot2::scale_x_continuous(breaks = breaks, limits = limits) +
          ggplot2::scale_y_continuous(breaks = seq(0, 1, .2), limits = c(0,1)) +
          ggplot2::labs(
            x = xlab,
            y = paste0("Power", " (", unique(.y$model), ")"),
            caption = paste0("Note: Effect size of practical concern = ", unique(.y$mu), ", ", "contrast values = ", unique(.y$d),
                             ", and ", "alpha = ", unique(.y$alpha), ".")
          ) +
          ggplot2::theme(legend.position = legend_position,
                         plot.caption.position = "plot",
                         plot.caption = ggplot2::element_text(hjust = 0)) +
          ggplot2::expand_limits(y = 0)
      )

    } else {

      plot <-
        plot_dat |>
        group_by(mu, d, alpha, model) |>
        group_map(
          .f = ~ ggplot2::ggplot(data = .x) +
            color_plot +
            gray_shade +
            ggplot2::geom_point() +
            ggplot2::geom_line() +
            ggplot2::geom_hline(yintercept = power_min, linetype = "dashed") +
            vline +
            ggplot2::facet_grid(omega_name ~ tau_name) +
            ggplot2::theme_bw() +
            ggplot2::scale_x_continuous(breaks = breaks, limits = limits) +
            ggplot2::scale_y_continuous(breaks = seq(0, 1, .2), limits = c(0,1)) +
            ggplot2::labs(x = xlab, y = paste0("Power", " (", unique(.y$model), ")")) +
            ggplot2::theme(legend.position = legend_position) +
            ggplot2::expand_limits(y = 0)
        )

    }

    if (numbers){

      label_length <- n_distinct(plot_dat$tau2)*n_distinct(plot_dat$omega2)

      df_text <-
        tibble(
          tau_name = factor(rep(unique(plot_dat$tau_name), n_distinct(plot_dat$omega_name))),

          omega_name = factor(rep(unique(plot_dat$omega_name), each = n_distinct(plot_dat$tau_name))),

          label = paste0("(", 1:label_length, ")"),

          cor = factor(unique(plot_dat$rho[1]))
        )

      plot <- purrr::map(plot, ~ .x + ggplot2::geom_text(data = df_text, ggplot2::aes(x = max(plot_dat$J), y = .01, label = label),
                                             size = number_size, color = "black"))

    }

  }

  ####################################
  # MDES plot(s)
  ####################################

  if (any(colnames(data) == "MDES")){

    if (color){
      color_plot <- ggplot2::aes(x = J, y = MDES, colour = cor, shape = cor, linetype = cor)
    } else {
      color_plot <- ggplot2::aes(x = J, y = MDES, shape = cor, linetype = cor)
    }


    if (is.null(expected_studies)){

      gray_shade <- NULL
      vline <- NULL


    } else if (length(expected_studies) == 1){

      gray_shade <- NULL
      vline <- ggplot2::geom_vline(xintercept = expected_studies, linetype = "dashed")


    } else if (length(expected_studies) == 2){

      gray_shade <- ggplot2::geom_rect(
        ggplot2::aes(xmin = expected_studies[1], xmax = expected_studies[2],
                     ymin = -Inf, ymax = Inf), fill = "gray90", color = "gray90", alpha = 0.1)

      vline <- NULL


    } else if (length(expected_studies) == 3){

      gray_shade <- ggplot2::geom_rect(
        ggplot2::aes(xmin = expected_studies[1], xmax = expected_studies[3],
                     ymin = -Inf, ymax = Inf), fill = "gray90", color = "gray90", alpha = 0.1)

      vline <- ggplot2::geom_vline(xintercept = expected_studies[2], linetype = "dashed")

    } else if (length(expected_studies) > 3) {

      stop("expected_studies must not exceed 3 values")

    }


    if (caption){

      plot <-
        plot_dat |>
        group_by(d, alpha, target_power, model) |>
        group_map(
          .f = ~ ggplot2::ggplot(data = .x) +
            color_plot +
            gray_shade +
            ggplot2::geom_point() +
            ggplot2::geom_line() +
            ggplot2::geom_hline(yintercept = MDES_min, linetype = "dashed") +
            vline +
            ggplot2::facet_grid(omega_name ~ tau_name) +
            ggplot2::theme_bw() +
            ggplot2::scale_x_continuous(breaks = breaks, limits = limits) +
            ggplot2::scale_y_continuous(
              breaks = seq(round(min(plot_dat$MDES) - 0.015, 2) , round(max(plot_dat$MDES) + 0.015, 2), .02),
              limits = c(round(min(plot_dat$MDES) - 0.015, 2) , round(max(plot_dat$MDES) + 0.015, 2) )
            ) +
            ggplot2::labs(
              x = xlab,
              y = paste0("Minimum Detectable ES", " (", unique(.y$model), ")"),
              caption = paste0("Note:", " Contrast value = ", unique(.y$d), ", ",
                               "alpha = ", unique(.y$alpha), ", ", "target power = ", unique(.y$target_power), ".")
            ) +
            ggplot2::theme(legend.position = legend_position,
                           plot.caption.position = "plot",
                           plot.caption = ggplot2::element_text(hjust = 0))
        )

    } else {

      plot <-
        plot_dat |>
        group_by(d, alpha, target_power, model) |>
        group_map(
          .f = ~ ggplot2::ggplot(data = .x) +
            color_plot +
            gray_shade +
            ggplot2::geom_point() +
            ggplot2::geom_line() +
            ggplot2::geom_hline(yintercept = MDES_min, linetype = "dashed") +
            vline +
            ggplot2::facet_grid(omega_name ~ tau_name) +
            ggplot2::theme_bw() +
            ggplot2::scale_x_continuous(breaks = breaks, limits = limits) +
            ggplot2::scale_y_continuous(
              breaks = seq(round(min(plot_dat$MDES) - 0.015, 2) , round(max(plot_dat$MDES) + 0.015, 2), .02),
              limits = c(round(min(plot_dat$MDES) - 0.015, 2) , round(max(plot_dat$MDES) + 0.015, 2) )
            ) +
            ggplot2::labs(x = xlab, y = paste0("Minimum Detectable ES", " (", unique(.y$model), ")")) +
            ggplot2::theme(legend.position = legend_position)
        )

    }

    if (numbers){

      label_length <- n_distinct(plot_dat$tau2)*n_distinct(plot_dat$omega2)

      df_text <-
        tibble(
          tau_name = factor(rep(unique(plot_dat$tau_name), n_distinct(plot_dat$omega_name))),

          omega_name = factor(rep(unique(plot_dat$omega_name), each = n_distinct(plot_dat$tau_name))),

          label = paste0("(", 1:label_length, ")"),

          cor = factor(unique(plot_dat$rho[1]))
        )

      plot <- purrr::map(plot, ~ .x + ggplot2::geom_text(data = df_text,
                                                         ggplot2::aes(x = max(plot_dat$J),
                                                         y = round(max(plot_dat$MDES) + 0.015, 2), label = label),
                                                         size = number_size, color = "black"))

    }

  }

  if (length(plot) == 1) plot <- plot[[1]]

  plot

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
    labs_size = 2.5
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
    ggplot2::theme(
      legend.position = legend_position,
      plot.caption.position = "plot",
      plot.caption = ggplot2::element_text(hjust = 0)
    )

  }
