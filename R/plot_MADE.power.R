
#' Plot function for a 'power' object
#'
#' \code{plot_MADE.power} returns a facet_grip plot for power analyses conducted with
#' \code{power_MADE}.
#'
#' @param data Data/object for which the plot should be made.
#' @param v_lines Optional integer or vector to specify vertical line(s) in facet_grid plot(s) (the default is \code{NULL}).
#' @param legend_position Optional character string to specify position of legend (default is "bottom").
#' @param color Optional logical to specify coloring of plot (default is \code{TRUE}).
#' @param numbers Optional logical to specify numbering of plots (default is \code{TRUE}).
#' @param number_size Optional integer to specify the size of the plot numbers (default is 2.5).
#' @param numbers_ynudge Optional integer to nudge number on the y-axis (default is 0).
#' @param caption Optional logical to specify inclusion on caption with detailed information regarding
#' the given analysis (default is \code{TRUE}).
#' @param x_lab Title for the x-axis (default is \code{x_lab = "Number of studies (J)"}).
#' @param x_breaks Optional sequence to specify breaks on the x-axis (default is \code{NULL}).
#' @param x_limits Optional vector to specify the limits of the x-axis (default is \code{NULL}).
#' @param y_breaks Optional sequence to specify breaks on the y-axis (default is \code{y_breaks = seq(0, 1, 0.2)}).
#' @param y_limits Optional vector to specify the limits of the y-axis (default is \code{y_limits = c(0, 1)}).
#' @param y_expand Optional vector to expand the limits of the y-axis (default is \code{NULL}).
#' @param warning Optional logical to specify if warnings should be returned when multiple models appear in the data
#' (default is \code{TRUE}).
#' @param traffic_light_assumptions Optional logical to specify coloring of strips of the facet grids to emphasize
#' assumptions about the likelihood the given analytical scenario (default is \code{NULL}).
#' See Vembye, Pustejovsky, & Pigott (In preparation) for further details.
#' @param power_min Optional integer or vector to either specify a horizontal line or intervals, indicating the minimum
#' power wanted to be obtained (default is \code{NULL}).
#' @param expected_studies Optional vector to specify the interval of studies expected to be found in the literature.
#' When specified this interval will be shaded across facet_grip plots (default is \code{NULL}).
#' @param model_comparison Optional logical to indicate if power estimates should be plotted across different models
#' to handle dependent effect sizes (default is \code{FALSE}) instead of across values of sampling correlations.
#' @param ... Additional arguments.
#'
#'
#' @description Function to make facet_grid plots for an object of class \code{"power"}.
#'
#' @details In general, it can be rather difficult to guess/approximate the true model parameters
#' and sample characteristics a priori. Making only a single power approximation can easily be misleading
#' even if the true model and data structure only slightly diverge
#' from the yielded data and model assumptions.
#' To maximize the informativeness of the power approximations,
#' Vembye, Pustejovsky, & Pigott (In preparation) suggest accommodating the uncertainty of the power approximations
#' by reporting or plotting power estimates across a range of possible scenarios, which can be
#' done by \code{plot_MADE.power}.
#'
#' @references Vembye, M. H., Pustejovsky, J. E., & Pigott, T. D. (In preparation).
#' Conducting power analysis for meta-analysis of dependent effect sizes: Common guidelines
#' and an Introduction to the POMADE R package.
#'
#' @return A \code{ggplot} facet_grip plot showing power across the expected number of
#' studies faceted by the between-study and within-study SDs, with different colors, lines, and
#' shapes corresponding to different values of the assumed sample correlation. If
#' \code{model_comparison = TRUE}, it returns a \code{ggplot} facet_grip plot showing power
#' across the expected number of studies faceted by the between-study and within-study SDs,
#' with different colors, lines, and shapes corresponding to different models to handle dependent effect sizes
#'
#' @seealso \code{\link{plot_MADE}}
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

plot_MADE.power <-
  function(
    data,
    v_lines = NULL,
    legend_position = "bottom",
    color = TRUE,
    numbers = TRUE,
    number_size = 2.5,
    numbers_ynudge = 0,
    caption = TRUE,
    x_lab = "Number of studies (J)",
    x_breaks = NULL,
    x_limits = NULL,
    y_breaks = seq(0, 1, .2),
    y_limits = c(0, 1),
    y_expand = NULL,
    warning = TRUE,
    traffic_light_assumptions = NULL,
    power_min = NULL,
    expected_studies = NULL,
    model_comparison = FALSE,
    ...
  ){

  if (warning) {
    if(n_distinct(data$model) > 1){
      warning("We recommend to create the plot for one model only", call. = FALSE)
    }
  }


  if (!model_comparison) {

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
                          ", and ", "alpha = ", alpha, "."),

        y_lab = paste0("Power", " (", model, ")")

      ) |>
      rowwise() |>
      select(data, cap, y_lab)


    if (!caption) {
      plot_dat$cap <- NULL
    }


    if (color) {

      plot <- suppressWarnings(dplyr::group_map(
        plot_dat,
        ~ plot_MADE_engine(
          data = .x$data[[1]],
          x = J,
          y = power,
          x_grid = tau_name,
          y_grid = omega_name,
          color = cor,
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
          labs_size = number_size,
          assumptions = traffic_light_assumptions
        ))
      )
    }
  }

  if (model_comparison) {

    if (n_distinct(data$model) == 1) {
      stop("Power approximations for more than one model are needed")
    }

    plot_dat <-
      data |>
      mutate(
        tau_name = factor(paste("Study Level SD =", tau)),
        omega_name = factor(paste("ES Level SD =", omega))
      ) |>
      rename(cor = rho) |>
      group_nest(mu, d, alpha, cor) |>
      mutate(

        cap = paste0("Note: Effect size of practical concern = ", mu, ", ", "contrast value = ", d,
                     ", ", "alpha = ", alpha, ", and ", "sample correlation = ", cor, "."),

        y_lab = "Power"

      ) |>
      rowwise() |>
      select(data, cap, y_lab)


    if (!caption) {
      plot_dat$cap <- NULL
    }


    if (color) {

      plot <- suppressWarnings(dplyr::group_map(
        plot_dat,
        ~ plot_MADE_engine(
          data = .x$data[[1]],
          x = J,
          y = power,
          x_grid = tau_name,
          y_grid = omega_name,
          color = model,
          shape = model,
          linetype = model,
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
          color_lab = "Model",
          shape_lab = "Model",
          line_lab = "Model",
          shape_scale = "model",
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
          x = J,
          y = power,
          x_grid = tau_name,
          y_grid = omega_name,
          color = NULL,
          shape = model,
          linetype = model,
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
          shape_lab = "Model",
          line_lab = "Model",
          shape_scale = "model",
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

  if (length(plot) == 1) plot <- plot[[1]]

  plot

}





