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
      group_by(model, mu, d, alpha) |>
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
          ggplot2::theme(legend.position = legend_position) +
          ggplot2::labs(
            x = "Number of studies (J)",
            y = paste0("Power", " (", unique(.y$model), ")"),
            caption = paste0("Note: mu = ", unique(.y$mu), ", ", "d = ", unique(.y$d),
                             ", and ", "alpha = ", unique(.y$alpha))
          ) +
          ggplot2::theme(plot.caption.position = "plot", plot.caption = ggplot2::element_text(hjust = 0)) +
          ggplot2::expand_limits(y = 0)
      )

    } else {

      plot <-
        plot_dat |>
        group_by(model, mu, d, alpha) |>
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
            ggplot2::theme(legend.position = legend_position) +
            ggplot2::labs(x = "Number of studies (J)", y = paste0("Power", " (", unique(.y$model), ")")) +
            ggplot2::expand_limits(y = 0)
        )

    }

    if (numbers){

      label_length <- n_distinct(plot_dat$tau2)*n_distinct(plot_dat$omega2)

      df_text <- tibble(
        tau_name = factor(rep(unique(plot_dat$tau_name), n_distinct(plot_dat$omega_name))),

        omega_name = factor(rep(unique(plot_dat$omega_name), each = n_distinct(plot_dat$tau_name))),

        label = paste0("(", 1:label_length, ")"),

        cor = factor(unique(plot_dat$rho[1]))
      )

      plot <- purrr::map(plot, ~ .x + ggplot2::geom_text(data = df_text, ggplot2::aes(x = max(plot_dat$J), y = .01, label = label),
                                             size = number_size, color = "black"))

    }


    if (n_distinct(data$model) == 1) plot <- plot[[1]]

  }

  ####################################
  # MDES plots
  ####################################

  # Insert MDES plot function

  plot

}
