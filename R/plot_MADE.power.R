#' @export

plot_MADE.power <-
  function(
    data,
    power_min = NULL,
    expected_studies = NULL,
    v_lines = NULL,
    legend_position = "bottom",
    color = TRUE,
    numbers = TRUE,
    number_size = 2.5,
    numbers_ynudge = 0.5,
    caption = TRUE,
    x_lab = "Number of studies (J)",
    x_breaks = NULL,
    x_limits = NULL,
    y_breaks = seq(0, 1, .2),
    y_limits = c(0, 1),
    y_expand = NULL,
    warning = TRUE
  ){

  if (warning) {
    if(n_distinct(data$model) > 1){
      warning("We recommend to create the plot for one model only", call. = FALSE)
    }
  }

  plot_dat <-
    data |>
    mutate(
      tau_name = factor(paste("Study Level SD =", tau)),
      omega_name = factor(paste("ES Level SD =", omega))
    ) |>
    rename(cor = rho) |>
    group_nest(mu, d, alpha, model) |>
    mutate(

      cap = paste0("Note: Effect size of practical concern = ", mu, ", ", "contrast value = ", d,
                        ", and ", "alpha = ", unique(alpha), "."),

      y_lab = paste0("Power", " (", model, ")")

    ) |>
    rowwise() |>
    select(data, cap, y_lab)


  if (!caption) {
    plot_dat$cap <- NULL
  }

  plot <- dplyr::group_map(
    plot_dat,
    ~ plot_MADE_engine(
      data = .x$data[[1]],
      x = J,
      y = power,
      x_grid = tau_name,
      y_grid = omega_name,
      color = NULL,
      shape = cor,
      linetype = cor,
      h_lines = power_min,
      v_lines = v_lines,
      v_shade = expected_studies,
      x_breaks = x_breaks,
      x_limits = x_limits,
      y_breaks = y_breaks,
      y_limits = y_limits,
      y_expand = y_expand,
      x_lab = x_lab,
      y_lab = .x$y_lab,
      color_lab = NULL,
      shape_lab = "Cor",
      line_lab = "Cor",
      caption = .x$cap,
      legend_position = legend_position,
      grid_labs = numbers,
      labs_ynudge = numbers_ynudge,
      labs_size = number_size
    ))

  if (length(plot) == 1) plot <- plot[[1]]

  plot

}





