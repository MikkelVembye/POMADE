
#' Plot function for a 'min_studies' object
#'
#' \code{plot_MADE.min_studies} returns a facet_grip plot with analyses of the minimum studies needed to obtained a given
#' effect size with a certain amount of power (usually 80%) and a pre-specified level-alpha conducted with
#' \code{min_studies_MADE}.
#'
#' @param data Data/object for which the plot should be made.
#' @param v_lines Optional integer or vector to specify vertical line(s) in facet_grid plot(s) (the default is \code{NULL}).
#' @param legend_position Optional character string to specify position of legend (default is "bottom").
#' @param color Optional logical to specify coloring of plot (default is \code{TRUE}).
#' @param numbers Optional logical to specify numbering of plots (default is \code{TRUE}).
#' @param number_size Optional integer to specify the size of the plot numbers (default is 2.5).
#' @param numbers_ynudge Optional integer to nudge number on the y-axis (default is \code{NULL}).
#' @param caption Optional logical to specify inclusion on caption with detailed information regarding
#' the given analysis (default is \code{TRUE}).
#' @param x_lab Title for the x-axis (default is \code{NULL}). If \code{length(data$mu) == 1}, the x-axis label will
#' be "Effect Size Level SD". If \code{length(data$mu) > 1}, the x-axis label will "Effect Size of Practical Concern".
#' @param x_breaks Optional sequence to specify breaks on the x-axis (default is \code{NULL}).
#' @param x_limits Optional vector to specify the limits of the x-axis (default is \code{NULL}).
#' @param y_breaks Optional sequence to specify breaks on the y-axis (default is \code{NULL}).
#' @param y_limits Optional vector to specify the limits of the y-axis (default is \code{NULL}).
#' @param y_expand Optional vector to expand the limits of the y-axis (default is \code{NULL}).
#' @param warning Optional logical to specify if warnings should be returned when multiple models appear in the data
#' (default is \code{TRUE}).
#' @param traffic_light_assumptions Optional logical to specify coloring of strips of the facet grids to emphasize
#' assumptions about the likelihood the given analytical scenario (default is \code{NULL}).
#' See Vembye, Pustejovsky, & Pigott (In preparation) for further details.
#' @param v_shade Optional vector to specify range of the x-axis interval to be shaded on facet_grip plots.
#' @param h_lines Optional integer or vector to specify horizontal lines on plots.
#' @param ... Additional arguments.
#'
#'
#' @description Function to make facet_grid plots for an object of class \code{"min_studies"}.
#'
#' @details In general, it can be rather difficult to guess/approximate the true model parameters
#' and sample characteristics a priori. Making only a single approximation of the minimum studies needed to obtained a given
#' effect size with a certain amount of power (usually 80%) and a pre-specified level-alpha can easily be misleading
#' even if the true model and data structure only slightly diverge
#' from the yielded data and model assumptions.
#' To maximize the informativeness of the minimum studies needed to obtained a given
#' effect size approximations, Vembye, Pustejovsky, & Pigott (In preparation) suggest accommodating the uncertainty
#' of the approximations by reporting or plotting estimates across a range of possible scenarios,
#' which can be done by \code{plot_MADE.min_studies}.
#'
#' @references Vembye, M. H., Pustejovsky, J. E., & Pigott, T. D. (In preparation).
#' Conducting power analysis for meta-analysis of dependent effect sizes: Common guidelines
#' and an Introduction to the POMADE R package.
#'
#' @return A \code{ggplot} facet_grip plot showing the minimum studies needed to obtained a given
#' effect size with a certain amount of power and level-alpha across estimates of the within-study SD
#' faceted by the between-study SDs, with different colors, lines, and
#' shapes corresponding to different values of the assumed sample correlation. If \code{length(data$mu) > 1},
#' it returns a \code{ggplot} facet_grip plot showing the minimum studies needed to obtained a given
#' effect size with a certain amount of power and level-alpha across effect sizes of practical concern
#' faceted by the between-study and within-study SDs, with different colors, lines, and
#' shapes corresponding to different values of the assumed sample correlation.
#'
#' @seealso \code{\link{plot_MADE}}
#' @examples
#'
#' min_studies_obj <-
#'   min_studies_MADE(
#'     mu = 0.2,
#'     tau = c(0.1, 0.2),
#'     omega = c(0, 0.1, 0.2, 0.3),
#'     rho = c(0.2, 0.7),
#'     target_power = .8,
#'     sigma2_dist = \(x) rgamma(x, shape = 5, rate = 10),
#'     n_ES_dist = \(x) 1 + stats::rpois(x, 5.5 - 1),
#'     seed = 10052510,
#'     iterations = 5
#'
#'   )
#'
#' min_studies_example <- plot_MADE(data = min_studies_obj)
#' min_studies_example
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
