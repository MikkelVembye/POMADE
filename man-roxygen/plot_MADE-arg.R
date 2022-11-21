
#' @param data Data/object for which the plot should be made.
#' @param v_lines Optional integer or vector to specify vertical line(s) in facet_grid plot(s) (the default is \code{NULL}).
#' @param legend_position Optional character string to specify position of legend (default is "bottom").
#' @param color Optional logical to specify coloring of plot (default is \code{FALSE}).
#' @param numbers Optional logical to specify numbering of plots (default is \code{TRUE}).
#' @param number_size Optional integer to specify the size of the plot numbers (default is 2.5).
#' @param numbers_ynudge Optional integer to nudge number on the y-axis (default is \code{NULL}).
#' @param caption Optional logical to specify inclusion on caption with detailed information regarding
#' the given analysis (default is \code{TRUE}).
#' @param x_lab Title for the x-axis (default is \code{NULL}). If \code{NULL}, the x_lab is specified automatically
#' within the given S3 method.
#' @param x_breaks Optional sequence to specify breaks on the x-axis (default is \code{NULL}).
#' @param x_limits Optional vector to specify the limits of the x-axis (default is \code{NULL}).
#' @param y_breaks Optional sequence to specify breaks on the y-axis (default is \code{NULL}).
#' @param y_limits Optional vector to specify the limits of the y-axis (default is \code{NULL}).
#' @param y_expand Optional vector to expand the limits of the y-axis.
#' @param warning Optional logical to specify if warnings should be returned  (default is \code{TRUE}).
#' @param traffic_light_assumptions Optional logical to specify coloring of strips of the facet grids to emphasize
#' assumptions about the likelihood the given analytical scenario (default is \code{NULL}).
#' See Vembye, Pustejovsky, & Pigott (In preparation) for further details.
