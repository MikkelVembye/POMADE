
#' @title Plot function for a 'power' object
#'
#' @template plot_MADE-arg
#' @param power_min Either an integer specify a horizontal line or a length-2
#'   vector to specify an interval, indicating a benchmark level of power
#'   (default is \code{NULL}).
#' @param expected_studies Optional vector of length 2 specifying a range for
#'   the number of studies one expects to include in the meta-analysis. If
#'   specified, this interval will be shaded across facet_grip plots (default is
#'   \code{NULL}).
#' @param model_comparison Logical indicating whether power estimates should be
#'   plotted across different working models for dependent effect size estimates
#'   (default is \code{FALSE}) instead of across values for the sampling
#'   correlation.
#'
#'
#' @description Creates a faceted plot or plots for power analyses conducted
#'   with \code{power_MADE}.
#'
#' @details In general, it can be rather difficult to guess/approximate the true
#'   model parameters and sample characteristics a priori. Calculating power
#'   under only a single set of assumptions can easily be misleading even if the
#'   true model and data structure only slightly diverge from the yielded data
#'   and model assumptions. To maximize the informativeness of the power
#'   approximations, Vembye, Pustejovsky, & Pigott (In preparation) suggest
#'   accommodating the uncertainty of the power approximations by reporting or
#'   plotting power estimates across a range of possible scenarios, which can be
#'   done using \code{plot_MADE.power}.
#'
#' @references Vembye, M. H., Pustejovsky, J. E., & Pigott, T. D. (In
#'   preparation). Conducting power analysis for meta-analysis of dependent
#'   effect sizes: Common guidelines and an introduction to the POMADE R
#'   package.
#'
#' @return A \code{ggplot} plot showing power across the expected number of
#'   studies, faceted by the between-study and within-study SDs, with different
#'   colors, lines, and shapes corresponding to different values of the assumed
#'   sample correlation. If \code{model_comparison = TRUE}, it returns a
#'   \code{ggplot} plot showing power across the expected number of studies,
#'   faceted by the between-study and within-study SDs, with different colors,
#'   lines, and shapes corresponding to different working models for dependent
#'   effect size estimates
#'
#' @seealso \code{\link{plot_MADE}}
#'
#' @examples
#' power_dat <-
#'   power_MADE(
#'     J = c(50, 56),
#'     mu = 0.15,
#'     tau = 0.1,
#'     omega = 0.05,
#'     rho = 0,
#'     sigma2_dist = 4 / 200,
#'     n_ES_dist = 6
#'   )
#'
#' power_example <-
#'   plot_MADE(
#'    data = power_dat,
#'    power_min = 0.8,
#'    expected_studies = c(52, 54),
#'    warning = FALSE,
#'    caption = TRUE,
#'    color = TRUE,
#'    model_comparison = FALSE,
#'    numbers = FALSE
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
    x_lab = NULL,
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

  if (is.null(x_lab)) x_lab <- "Number of Studies (J)"

  if (!model_comparison) {

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





