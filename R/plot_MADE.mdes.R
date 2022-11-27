
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
#'   effect sizes: Common guidelines and an Introduction to the POMADE R
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
#'
#' mdes_dat <-
#'   mdes_MADE(
#'     J = seq(60, 90, 10),
#'     tau = c(0, 0.25),
#'     omega = c(0, 0.1),
#'     rho = c(0.2, 0.7),
#'     target_power = .8,
#'     alpha = 0.05,
#'     sigma2_dist = \(x) rgamma(x, shape = 5, rate = 10),
#'     n_ES_dist = \(x) 1 + stats::rpois(x, 5.5 - 1),
#'     seed = 10052510,
#'     iterations = 5
#'   )
#'
#' plot_mdes <- plot_MADE(data = mdes_dat, expected_studies = c(70, 80), numbers_ynudge = 0.139)
#' plot_mdes
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
    y_breaks = NULL,
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

    if (is.null(y_breaks) && min(data$MDES) >= 0.05){
      y_breaks <- seq(round(min(data$MDES) - 0.015, 2), round(max(data$MDES) + 0.015, 2), .02)
    } else if (is.null(y_breaks) && min(data$MDES) < 0.05) {
      y_breaks <- seq(0, round(max(data$MDES) + 0.015, 2), .02)
    }

    if (is.null(y_limits) && min(data$MDES) >= 0.05){
      y_limits <- c(round(min(data$MDES) - 0.015, 2) , round(max(data$MDES) + 0.01, 2) )
    } else if (is.null(y_limits) && min(data$MDES) < 0.05) {
      y_limits <- c(0, round(max(data$MDES) + 0.015, 2))
    }

    if (is.null(numbers_ynudge)){
      numbers_ynudge <- round(max(data$MDES) + 0.015 - min(data$MDES) + 0.015, 2)
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
