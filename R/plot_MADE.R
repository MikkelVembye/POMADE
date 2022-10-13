# Add arguments

plot_MADE <-
  function(
    data,
    power_min = 0.8,
    expected_studies = NULL,
    breaks = NULL,
    limits = NULL,
    legend_position = "bottom",
    color = FALSE,
    warning = TRUE
  ){

  if (any(colnames(data) == "power")){

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


    if (color){

      color_plot <- ggplot2::aes(x = J, y = Power, colour = cor, shape = cor, linetype = cor)

    } else {

      color_plot <- ggplot2::aes(x = J, y = Power, shape = cor, linetype = cor)

    }

    if (is.null(expected_studies)){

      plot <-
        plot_dat |>
        rename(Power = power) |>
        group_by(model) |>
        group_map(
          .f = ~ ggplot2::ggplot(data = .x) +
            color_plot +
            ggplot2::geom_point() +
            ggplot2::geom_line() +
            ggplot2::geom_hline(yintercept = power_min, linetype = "dashed") +
            ggplot2::facet_grid(omega_name ~ tau_name) +
            ggplot2::theme_bw() +
            ggplot2::scale_x_continuous(breaks = breaks, limits = limits) +
            ggplot2::scale_y_continuous(breaks = seq(0, 1, .2), limits = c(0,1)) +
            ggplot2::theme(legend.position = legend_position) +
            ggplot2::labs(x = "Number of studies (J)", y = paste0("Power", " (", unique(.y$model), ")")) +
            ggplot2::expand_limits(y = 0)
        )



    } else if (length(expected_studies) == 1){

      plot <-
        plot_dat |>
        rename(Power = power) |>
        group_by(model) |>
        group_map(
          .f = ~ ggplot2::ggplot(data = .x) +
            color_plot +
            ggplot2::geom_point() +
            ggplot2::geom_line() +
            ggplot2::geom_hline(yintercept = power_min, linetype = "dashed") +
            ggplot2::geom_vline(xintercept = expected_studies, linetype = "dashed") +
            ggplot2::facet_grid(omega_name ~ tau_name) +
            ggplot2::theme_bw() +
            ggplot2::scale_x_continuous(breaks = breaks, limits = limits) +
            ggplot2::scale_y_continuous(breaks = seq(0, 1, .2), limits = c(0,1)) +
            ggplot2::theme(legend.position = legend_position) +
            ggplot2::labs(x = "Number of studies (J)", y = paste0("Power", " (", unique(.y$model), ")")) +
            ggplot2::expand_limits(y = 0)
        )


    } else if (length(expected_studies) == 2){

      plot <-
        plot_dat |>
        rename(Power = power) |>
        group_by(model) |>
        group_map(
          .f = ~ ggplot2::ggplot(data = .x) +
            color_plot +
            ggplot2::geom_rect(
              ggplot2::aes(xmin = expected_studies[1], xmax = expected_studies[2],
                           ymin = -Inf, ymax = Inf), fill = "gray90", color = "gray90", alpha = 0.1) +
            ggplot2::geom_point() +
            ggplot2::geom_line() +
            ggplot2::geom_hline(yintercept = power_min, linetype = "dashed") +
            ggplot2::facet_grid(omega_name ~ tau_name) +
            ggplot2::theme_bw() +
            ggplot2::scale_x_continuous(breaks = breaks, limits = limits) +
            ggplot2::scale_y_continuous(breaks = seq(0, 1, .2), limits = c(0,1)) +
            ggplot2::theme(legend.position = legend_position) +
            ggplot2::labs(x = "Number of studies (J)", y = paste0("Power", " (", unique(.y$model), ")")) +
            ggplot2::expand_limits(y = 0)
        )


    } else if (length(expected_studies) == 3){

      plot <-
        plot_dat |>
        rename(Power = power) |>
        group_by(model) |>
        group_map(
          .f = ~ ggplot2::ggplot(data = .x) +
            color_plot +
            ggplot2::geom_rect(
              ggplot2::aes(xmin = expected_studies[1], xmax = expected_studies[3],
                          ymin = -Inf, ymax = Inf), fill = "gray90", color = "gray90", alpha = 0.1) +
            ggplot2::geom_point() +
            ggplot2::geom_line() +
            ggplot2::geom_hline(yintercept = power_min, linetype = "dashed") +
            ggplot2::geom_vline(xintercept = expected_studies[2], linetype = "dashed") +
            ggplot2::facet_grid(omega_name ~ tau_name) +
            ggplot2::theme_bw() +
            ggplot2::scale_x_continuous(breaks = breaks, limits = limits) +
            ggplot2::scale_y_continuous(breaks = seq(0, 1, .2), limits = c(0,1)) +
            ggplot2::theme(legend.position = legend_position) +
            ggplot2::labs(x = "Number of studies (J)", y = paste0("Power", " (", unique(.y$model), ")")) +
            ggplot2::expand_limits(y = 0)
        )

    } else if (length(expected_studies) > 3) {

      stop("expected_studies must not exceed 3 values")

    }


    if (n_distinct(data$model) == 1) plot <- plot[[1]]

  }


  plot

}
