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
    caption = TRUE,
    x_lab = "Number of studies (J)",
    x_breaks = NULL,
    x_limits = NULL,
    y_breaks = seq(0, 1, .2),
    y_limits = c(0, 1),
    warning = TRUE,
    ...
  ){

  if (warning){
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
    group_by(mu, d, alpha, model) |>
    mutate(

      cap = paste0("Note: Effect size of practical concern = ", mu, ", ", "contrast values = ", d,
                        ", and ", "alpha = ", unique(alpha), "."),

      y_lab = paste0("Power", " (", unique(model), ")")

    )


  plot_list <-
    plot_dat |>
    tidyr::nest() |>
    pull(data)


  if (caption) {

    cap_list <-
      plot_dat |>
      select(mu, d, alpha, model, cap) |>
      summarise(caption = cap[1]) |>
      group_by(mu, d, alpha, model) |>
      tidyr::nest() |>
      pull(data)

  } else {

    cap_list <- list(NULL)

  }



  plot <- purrr::map2(plot_list, cap_list, ~ plot_MADE_engine(
    data = .x,
    x = J,
    y = power,
    x_grid = tau_name,
    y_grid = omega_name,
    color = NULL,
    shape = cor,
    linetype = cor,
    color_lab = NULL,
    shape_lab = "Cor",
    line_lab = "Cor",
    h_lines = power_min,
    v_lines = v_lines,
    v_shade = expected_studies,
    x_lab = x_lab,
    y_lab = .x$y_lab,
    caption = .y,
    grid_labs = numbers,
    y_breaks = y_breaks,
    y_limits = y_limits,
    legend_position = legend_position
    ),
    ...
  )


  if (length(plot) == 1) plot <- plot[[1]]

  plot

}





