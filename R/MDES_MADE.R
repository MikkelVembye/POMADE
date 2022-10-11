
#' @title Minimum Detectable Effect Size (MDES) for Meta-Analysis With Dependent Effect Sizes
#'
#' @param J Number of studies
#' @template common-arg
#' @param target_power Specifying the target power
#' @param interval A vector containing the end-points of the interval to be searched for the root
#'
#' @return Returns a \code{tibble} with information about the expectation of the number of
#' studies, the between-study and within-study variance components,
#' the sample correlation, the contrast effect, the level of statistical significance, the target power value(s),
#' the minimum detectable effect size, the number of iterations, the model to handle dependent effect sizes,
#' and the methods used to obtain sampling variance estimates as well as the number effect sizes per study.
#'
#' @importFrom stats df
#' @import dplyr
#'
#' @export
#'
#' @examples
#'
#' mdes <- MDES_MADE(
#'   J = c(40, 60),
#'   tau2 = 0.2^2,
#'   omega2 = 0.1^2,
#'   rho = 0.7,
#'   model = c("CHE", "MLMA", "CE"),
#'   var_df = c("Model", "Satt", "RVE"),
#'   sigma2_dist = \(x) rgamma(x, shape = 5, rate = 10),
#'   n_ES_dist = \(x) 1 + stats::rpois(x, 5.5 - 1),
#'   seed = 10052510
#' )
#'
#' mdes
#'
#'

MDES_MADE <-
  function(
    J,
    tau2,
    omega2,
    rho,
    alpha = 0.05,
    target_power = 0.8,
    d = 0,

    model = "CHE",
    var_df = "RVE",

    sigma2_dist = NULL,
    n_ES_dist = NULL,

    iterations = 100,
    seed = NULL,
    warning = TRUE,
    interval = c(0,2)

  ) {

    if (warning) {
      if (is.numeric(sigma2_dist) && length(sigma2_dist) == 1 || is.numeric(n_ES_dist) && length(n_ES_dist) == 1){
        warning(
          paste0("Notice: It is generally recommended not to draw on balanced assumptions ",
                 "regarding the study precision (sigma2js) or the number of effect sizes per study (kjs). ",
                 "See Figures 2A and 2B in Vembye, Pustejovsky, and Pigott (2022)."),
          call. = FALSE
        )
      }
    }

    model <- match.arg(model, c("CHE","MLMA","CE"), several.ok = TRUE)
    var_df <- match.arg(var_df, c("Model","Satt","RVE"), several.ok = TRUE)
    if ("CE" %in% model & !("RVE" %in% var_df)) stop("CE model is only available for var_df = 'RVE'.")

    design_factors <-
      list(
        J = J,
        tau2 = tau2,
        omega2 = omega2,
        rho = rho,
        alpha = alpha,
        target_power = target_power,
        d = d,
        model = model,
        var_df = var_df
        )

    params <- purrr::cross_df(design_factors) |>
      filter(model != "CE" | var_df == "RVE")


    res <- purrr::pmap_dfr(
      .l = params, .f = MDES_MADE_engine,
      sigma2_dist = sigma2_dist, n_ES_dist = n_ES_dist, iterations = iterations,
      seed = seed, interval = interval, extendInt = "no"
    )


    res |>
      dplyr::arrange(dplyr::across(J:target_power))
}


MDES_MADE_engine <-
  function(
    J,
    tau2,
    omega2,
    rho,
    alpha = 0.05,
    target_power = 0.8,
    d = 0,

    model = "CHE",
    var_df = "RVE",

    sigma2_dist = NULL,
    n_ES_dist = NULL,

    iterations = 5,
    seed = NULL,
    interval = c(0,2),
    extendInt = "no"
  ) {


  ####################################
  # Sampling variance estimates labels
  ####################################

  # Assuming balanced sampling variance estimates across studies
  if (is.numeric(sigma2_dist) && length(sigma2_dist) == 1) {
   samp_method_sigma2 <- "balanced"
  }

  # Stylized distribution of sampling variance estimates
  if (is.function(sigma2_dist)) {
   samp_method_sigma2 <- "stylized"
  }

  # Empirical distribution of sampling variance estimates across studies
  if (is.numeric(sigma2_dist) & length(sigma2_dist) > 1 & length(sigma2_dist) != length(n_ES_dist)) {
   samp_method_sigma2 <- "empirical"
  }

  #########################################
  # Number of effect sizes per study labels
  #########################################

  if (is.numeric(n_ES_dist) && length(n_ES_dist) == 1) {
   # Assuming that all studies yield the same number of effect sizes
   samp_method_kj <- "balanced"
  } else if (is.function(n_ES_dist)) {
   # Stylized distribution of the number of effect sizes per study
   samp_method_kj <- "stylized"
  } else if (is.numeric(n_ES_dist) && length(n_ES_dist) > 1 && length(sigma2_dist) != length(n_ES_dist)) {
   # Empirical distribution of the number of effect sizes per study
   samp_method_kj <- "empirical"
  } else if (length(sigma2_dist) > 1 && length(n_ES_dist) > 1 && length(sigma2_dist) == length(n_ES_dist)) {
   # If both sigma2js and kjs are empirically obtained
   samp_method_sigma2 <- "empirical_combi"
   samp_method_kj <- "empirical_combi"
  }


  f <- function(mu) {

  power_MADE_engine(
    J = J, mu = mu, tau2 = tau2, omega2 = omega2,
    rho = rho, alpha = alpha, d = d,
    model = model, var_df = var_df,
    sigma2_dist = sigma2_dist, n_ES_dist = n_ES_dist,
    iterations = iterations, average_power = TRUE,
    seed = seed
  )$power - target_power

  }

  MDES <- stats::uniroot(f, interval = interval, extendInt = extendInt)$root

  # To align the results with the power_MADE function
  if ("Satt" %in% var_df) var_df <- "Model+Satt"

  tibble(
   J = J,
   tau2 = tau2,
   omega2 = omega2,
   rho = rho,
   d = d,
   alpha = alpha,
   target_power = target_power,
   MDES = MDES,
   iterations = iterations,
   model = paste(model, var_df, sep = "-"),
   samp_method_sigma2 = samp_method_sigma2,
   samp_method_kj = samp_method_kj
  )

}

















