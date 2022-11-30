
#' @title Plot function for a 'min_studies' object
#'
#' @template plot_MADE-arg
#' @param v_shade Optional vector of length 2 specifying the range of the x-axis
#'   interval to be shaded in each plot.
#' @param h_lines Optional integer or vector specifying horizontal lines on each
#'   plot.
#'
#' @description Creates a faceted plot with analyses of the minimum number of
#'   studies needed to obtained a given effect size with specified levels of
#'   power, as calculated using \code{min_studies_MADE}.
#'
#' @details In general, it can be rather difficult to guess/approximate the true
#'   model parameters and sample characteristics a priori. Calculating the
#'   minimum number of studies needed under just a single set of assumptions can
#'   easily be misleading even if the true model and data structure only
#'   slightly diverge from the yielded data and model assumptions. To maximize
#'   the informativeness of the analysis, Vembye, Pustejovsky, &
#'   Pigott (In preparation) suggest accommodating the uncertainty of the power
#'   approximations by reporting or plotting power estimates across a range of
#'   possible scenarios, which can be done using \code{plot_MADE.power}.
#'
#' @references Vembye, M. H., Pustejovsky, J. E., & Pigott, T. D. (In
#'   preparation). Conducting power analysis for meta-analysis of dependent
#'   effect sizes: Common guidelines and an introduction to the POMADE R
#'   package.
#'
#' @return A \code{ggplot} plot showing the minimum number of studies needed to
#'   obtain a given effect size with a certain amount of power and level-alpha, faceted
#'   across levels of the within-study SD and the between-study SD,
#'   with different colors, lines, and shapes corresponding to different values
#'   of the assumed sample correlation. If \code{length(unique(data$mu)) > 1}, it
#'   returns a \code{ggplot} plot showing the minimum studies needed
#'   to obtained a given effect size with a certain amount of power and
#'   level-alpha across effect sizes of practical concern, faceted by the
#'   between-study and within-study SDs, with different colors, lines, and
#'   shapes corresponding to different values of the assumed sample correlation.
#'
#' @seealso \code{\link{plot_MADE}}
#'
#' @examples
#' min_studies_MADE(
#'   mu = c(0.25, 0.35),
#'   tau = 0.05,
#'   omega = 0.02,
#'   rho = 0.2,
#'   target_power = .7,
#'   sigma2_dist = 4 / 200,
#'   n_ES_dist = 6,
#'   seed = 10052510
#' ) |>
#'   plot_MADE(y_breaks = seq(0, 10, 2), numbers = FALSE)
#'
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
    y_breaks = ggplot2::waiver(),
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

    if (is.null(y_limits)) {
      y_limits <- c(0, 10 * ceiling(max(data$studies_needed) / 10))
    }

    if (inherits(y_breaks,"waiver")) {
      y_breaks <- seq(10, max(y_limits), 10)
    }

    if (!caption) {
      plot_dat$cap <- NULL
    }

    if (n_distinct(data$mu) == 1) {

      if (is.null(x_lab)){
        x_lab <- "Effect Size Level SD"
      }

      if (is.null(numbers_ynudge)) {
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


      if (color) {

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


    if (n_distinct(data$mu) > 1) {

      if (is.null(x_lab)) {
        x_lab <- "Effect Size of Practical Concern"
      }

      if (is.null(numbers_ynudge)) {
        numbers_ynudge <- max(data$studies_needed)
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

      if (color) {

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
          assumptions = traffic_light_assumptions
        ))
       )
      }

    }

    if (length(plot) == 1) plot <- plot[[1]]

    plot

}
