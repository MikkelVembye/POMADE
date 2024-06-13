#' @param data Data/object for which the plot should be made.
#' @param v_lines Integer or vector to specify vertical line(s) in within each
#'   plot. Default is \code{NULL}.
#' @param legend_position Character string to specify position of legend.
#'   Default is \code{"bottom"}.
#' @param color Logical indicating whether to use color in the plot(s). Default
#'   is \code{TRUE}.
#' @param numbers Logical indicating whether to number the plots. Default is
#'   \code{TRUE}.
#' @param number_size Integer value specifying the size of the (optional) plot
#'   numbers. Default is \code{2.5}.
#' @param numbers_ynudge Integer value for vertical nudge of the (optional) plot
#'   numbers.
#' @param caption Logical indicating whether to include a caption with detailed
#'   information regarding the analysis. Default is \code{TRUE}.
#' @param x_lab Title for the x-axis. If \code{NULL} (the default), the x_lab is
#'   specified automatically.
#' @param x_breaks Optional vector to specify breaks on the x-axis. Default is
#'   \code{NULL}.
#' @param x_limits Optional vector of length 2 to specify the limits of the
#'   x-axis. Default is \code{NULL}, which allows limits to be determined
#'   automatically from the data.
#' @param y_breaks Optional vector to specify breaks on the y-axis.
#' @param y_limits Optional vector of length 2 to specify the limits of the
#'   y-axis.
#' @param y_expand Optional vector to expand the limits of the y-axis. Default
#'   is \code{NULL}.
#' @param warning Logical indicating whether warnings should be returned when
#'   multiple models appear in the data. Default is \code{TRUE}.
#' @param traffic_light_assumptions Optional logical to specify coloring of
#'   strips of the facet grids to emphasize assumptions about the likelihood the
#'   given analytical scenario. See Vembye, Pustejovsky, & Pigott (forthcoming)
#'   for further details.
#' @param traffic_light_palette Character string or character vector to control
#'   the color of traffic light strips. If set to \code{'green-yellow-red'} (the
#'   default), expected scenarios will be colored green, likely scenarios will
#'   be colored yellow, and unlikely scenarios will be colored red. If set to
#'   \code{'greyscale'}, a gray-scale version of the traffic light plot is
#'   provided with white indicating the expected scenario, light gray indicating
#'   other plausible scenarios, and dark gray indicating less likely scenarios.
#'   Users can also specify their own palette by setting
#'   \code{traffic_light_palette} to a character vector with colors for the
#'   expected, likely, and unlikely scenarios. In this case, the vector must
#'   have three entries named \code{'expected'}, \code{'likely'}, and
#'   \code{'unlikely'},
#' @param ... Additional arguments available for some classes of objects.
#'
