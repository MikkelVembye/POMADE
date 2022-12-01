
#' @title Plot function for a 'mdes' object
#'
#' @template plot_MADE-arg
#' @inheritParams plot_MADE.power
#' @param es_min Optional integer or vector to specify a horizontal line or
#'   interval, indicating a benchmark value or values for the minimum effect
#'   size of practical concern (default is \code{NULL}).
#'
#' @description Creates a faceted plot for minimum detectable effect size (mdes)
#'   analyses calculated using \code{mdes_MADE}.
#'
#' @details In general, it can be rather difficult to guess/approximate the true
#'   model parameters and sample characteristics a priori. Calculating the
#'   minimum detectable effect size under just a single set of assumptions can
#'   easily be misleading even if the true model and data structure only
#'   slightly diverge from the yielded data and model assumptions. To maximize
#'   the informativeness of the analysis, Vembye, Pustejovsky, & Pigott (In
#'   preparation) suggest accommodating the uncertainty of the power
#'   approximations by reporting or plotting minimum detectable effect size
#'   estimates across a range of possible scenarios, which can be done using
#'   \code{plot_MADE.mdes}.
#'
#' @references Vembye, M. H., Pustejovsky, J. E., & Pigott, T. D. (In
#'   preparation). Conducting power analysis for meta-analysis of dependent
#'   effect sizes: Common guidelines and an introduction to the POMADE R
#'   package.
#'
#' @return A \code{ggplot} plot showing the minimum detectable effect
#'   size across the expected number of studies, faceted by the between-study and
#'   within-study SDs, with different colors, lines, and shapes corresponding to
#'   different values of the assumed sample correlation.
#'
#' @seealso \code{\link{plot_MADE}}
#'
#' @examples
#' mdes_MADE(
#'   J = c(25, 35),
#'   tau = 0.05,
#'   omega = 0,
#'   rho = 0,
#'   target_power = .6,
#'   alpha = 0.1,
#'   sigma2_dist = 4 / 200,
#'   n_ES_dist = 8,
#'   seed = 10052510
#' ) |>
#'   plot_MADE(expected_studies = c(28, 32), numbers = FALSE)
#'
#'
#'
#' @export

plot_MADE.mdes <-
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
    y_breaks = ggplot2::waiver(),
    y_limits = NULL,
    y_expand = NULL,
    warning = TRUE,
    traffic_light_assumptions = NULL,
    es_min = NULL,
    expected_studies = NULL,
    ...
  ){

    if (warning) {
      if(n_distinct(data$model) > 1){
        warning("We recommend to create the plot for one model only", call. = FALSE)
      }
    }

    if (is.null(x_lab)) x_lab <- "Number of Studies (J)"

    # if (is.null(y_breaks)) {
    #   y_breaks <- seq(0, round(max(data$MDES) + 0.015, 2), .02)
    # }

    if (is.null(y_limits)) {
      y_limits <- c(0, ceiling(20 * max(data$MDES)) / 20)
    }

    if (is.null(numbers_ynudge)) {
      numbers_ynudge <- max(data$MDES)
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

        y_lab = paste0("Minimum Detectable Effect Sizes", " (", model, ")")

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
          y = MDES,
          x_grid = tau_name,
          y_grid = omega_name,
          color = cor,
          shape = cor,
          linetype = cor,
          h_lines = es_min,
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
          y = MDES,
          x_grid = tau_name,
          y_grid = omega_name,
          color = NULL,
          shape = cor,
          linetype = cor,
          h_lines = es_min,
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

    if (length(plot) == 1) plot <- plot[[1]]

    plot

  }
