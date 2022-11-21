#' @export

plot_MADE.min_studies <-
  function(
    data,
    v_lines = NULL,
    legend_position = "bottom",
    color = TRUE,
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
    traffic_light_assumptions = NULL,
    v_shade = NULL,
    h_lines = NULL,
    ...
  ){

    if (warning) {
      if(n_distinct(data$model) > 1){
        warning("We recommend to create the plot for one model only", call. = FALSE)
      }
    }


    if (n_distinct(data$mu) == 1){

      if (is.null(x_lab)){
        x_lab <- "Effect Size Level SD"
      }

      if (is.null(y_limits)) {
        y_limits <- data |> dplyr::pull(studies_needed) |> range()
      }
      if (is.null(y_breaks)) {
        y_breaks <- seq(min(data$studies_needed), max(data$studies_needed) + 10, 10)
      }

      if (is.null(numbers_ynudge)){
        numbers_ynudge <- 0
      }

      plot_dat <-
        data |>
        mutate(
          tau_name = factor(paste("Study Level SD =", tau)),
          omega_name = factor(paste("ES Level SD =", omega))
        ) |>
        rename(cor = rho) |>
        group_nest(alpha, target_power, d, model, mu) |>
        mutate(

          cap = paste0("Note: Alpha = ", alpha, ", ",
                       "power = ", target_power, ", and ",
                       "contrast value = ", d,
                       "."),

          y_lab = paste0("Studies Needed to Find An Effect of ", mu, " (", model, ")")

        ) |>
        rowwise() |>
        select(data, cap, y_lab)

      if (!caption) {
        plot_dat$cap <- NULL
      }


      if (color){

        plot <- suppressWarnings(dplyr::group_map(
          plot_dat,
          ~ plot_MADE_engine(
            data = .x$data[[1]],
            x = omega,
            y = studies_needed,
            x_grid = tau_name,
            y_grid = NULL,
            color = cor,
            shape = cor,
            linetype = cor,
            h_lines = h_lines,
            v_lines = v_lines,
            v_shade = v_shade,
            x_breaks = x_breaks,
            x_limits = x_limits,
            y_breaks = y_breaks,
            y_limits = y_limits,
            y_expand = y_expand,
            x_lab = x_lab,
            y_lab = .x$y_lab,
            color_lab = "Cor",
            shape_lab = "Cor",
            line_lab = "Cor",
            caption = .x$cap,
            legend_position = legend_position,
            grid_labs = numbers,
            labs_ynudge = numbers_ynudge,
            labs_size = number_size,
            assumptions = traffic_light_assumptions
          ))
        )

      } else {

        plot <- suppressWarnings(dplyr::group_map(
          plot_dat,
          ~ plot_MADE_engine(
            data = .x$data[[1]],
            x = omega,
            y = studies_needed,
            x_grid = tau_name,
            y_grid = NULL,
            color = NULL,
            shape = cor,
            linetype = cor,
            h_lines = h_lines,
            v_lines = v_lines,
            v_shade = v_shade,
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
            labs_size = number_size,
            assumptions = traffic_light_assumptions
          ))
        )
      }

    }


    if (n_distinct(data$mu) > 1){

      if(is.null(x_lab)){
        x_lab <- "Effect Size of Practical Concern"
      }

      if (is.null(y_limits)) {
        y_limits <- data |> dplyr::pull(studies_needed) |> range()
      }
      if (is.null(y_breaks)) {
        y_breaks <- seq(min(data$studies_needed), max(data$studies_needed) + 20, 20)
      }

      if (is.null(numbers_ynudge)){
        numbers_ynudge <- max(data$studies_needed) - min(data$studies_needed)
      }

      plot_dat <-
        data |>
        mutate(
          tau_name = factor(paste("Study Level SD =", tau)),
          omega_name = factor(paste("ES Level SD =", omega))
        ) |>
        rename(cor = rho) |>
        group_nest(alpha, target_power, d, model) |>
        mutate(

          cap = paste0("Note: Alpha = ", alpha, ", ",
                       "power = ", target_power, ", and ",
                       "contrast value = ", d,
                       "."),

          y_lab = paste0("Number of Studies Needed", " (", model, ")")

        ) |>
        rowwise() |>
        select(data, cap, y_lab)

      if (!caption) {
        plot_dat$cap <- NULL
      }


      if (color){

        plot <- suppressWarnings(dplyr::group_map(
          plot_dat,
          ~ plot_MADE_engine(
            data = .x$data[[1]],
            x = mu,
            y = studies_needed,
            x_grid = tau_name,
            y_grid = omega_name,
            color = cor,
            shape = cor,
            linetype = cor,
            h_lines = h_lines,
            v_lines = v_lines,
            v_shade = v_shade,
            x_breaks = x_breaks,
            x_limits = x_limits,
            y_breaks = y_breaks,
            y_limits = y_limits,
            y_expand = y_expand,
            x_lab = x_lab,
            y_lab = .x$y_lab,
            color_lab = "Cor",
            shape_lab = "Cor",
            line_lab = "Cor",
            caption = .x$cap,
            legend_position = legend_position,
            grid_labs = numbers,
            labs_ynudge = numbers_ynudge,
            labs_size = number_size,
            #shape_scale_manually = TRUE,
            assumptions = traffic_light_assumptions
          ))
        )
      } else {

      plot <- suppressWarnings(dplyr::group_map(
        plot_dat,
        ~ plot_MADE_engine(
          data = .x$data[[1]],
          x = mu,
          y = studies_needed,
          x_grid = tau_name,
          y_grid = omega_name,
          color = NULL,
          shape = cor,
          linetype = cor,
          h_lines = h_lines,
          v_lines = v_lines,
          v_shade = v_shade,
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
          labs_size = number_size,
          #shape_scale_manually = TRUE,
          assumptions = traffic_light_assumptions
        ))
       )
      }

    }

    if (length(plot) == 1) plot <- plot[[1]]

    plot

}
