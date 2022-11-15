#' @title Finding the Number of Studies Needed for the Confidence Interval to Exceed Value x (Usually Null)
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#'
#' @template common-arg-precision
#' @param x The threshold value that of the estimated confidence interval should exceed
#' @template common-arg-precision2
#' @param upper Upper value of the uniroot interval
#'
#' @return Returns a \code{tibble} with information about the effect size of practical concern,
#' the between-study and within-study variance components,  the sample correlation, the selected threshold value,
#' the expectation of the number of studies needed to obtained the target width of the confidence interval,
#' the number of iterations, the model to handle dependent effect sizes,
#' and the methods used to obtain sampling variance estimates as well as the number effect sizes per study.
#'
#' @importFrom magrittr %>%
#' @importFrom stats df
#' @import dplyr
#'
#' @export
#'
#' @examples
#' width_exceed_null <-
#'   width_above_x_MADE(
#'
#'     mu = 0.1,
#'     tau = c(0.1, 0.2),
#'     omega = 0.1,
#'     rho = c(0.2, 0.7),
#'     level = 0.95,
#'     x = 0,
#'
#'     model = "CHE",
#'     var_df = "RVE",
#'
#'     sigma2_dist = \(x) rgamma(x, shape = 5, rate = 10),
#'     n_ES_dist = \(x) 1 + stats::rpois(x, 5.5 - 1),
#'
#'     iterations = 5, # defualt = 100
#'     seed = 10052510,
#'     warning = TRUE,
#'     upper = 100
#'
#'   )
#'
#' width_exceed_null
#'


width_above_x_MADE <-
  function(
    mu,
    tau,
    omega,
    rho,
    level = 0.95,
    x = 0,

    model = "CHE",
    var_df = "RVE",

    sigma2_dist = NULL,
    n_ES_dist = NULL,

    iterations = 100,
    seed = NULL,
    warning = TRUE,
    upper = 100

  ){

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
        mu = mu,
        tau = tau,
        omega = omega,
        rho = rho,
        level = level,
        x = x,
        model = model,
        var_df = var_df
      )

    params <- purrr::cross_df(design_factors) |>
      filter(model != "CE" | var_df == "RVE")

    furrr_seed <- if (is.null(seed)) TRUE else NULL

    suppressPackageStartupMessages(
      res <- furrr::future_pmap_dfr(
        .l = params, .f = width_above_x_MADE_engine,
        sigma2_dist = sigma2_dist, n_ES_dist = n_ES_dist, iterations = iterations,
        seed = seed, upper = upper, extendInt = "yes",
        .options = furrr::furrr_options(seed = furrr_seed)
      ) |>
        dplyr::arrange(dplyr::across(mu:threshold_val))
    )

    tibble::new_tibble(res, class = "width_above_x")

  }


width_above_x_MADE_engine <-
  function(
    mu,
    tau,
    omega,
    rho,
    level = 0.95,
    x = 0,

    model = "CHE",
    var_df = "RVE",

    sigma2_dist = NULL,
    n_ES_dist = NULL,

    iterations = 5,
    seed = NULL,
    upper = 100,
    extendInt = "yes"

    ){

    if (!is.null(seed) && (is.function(sigma2_dist) | is.function(n_ES_dist))) {
      set.seed(seed)
    }

    ####################################
    # Sampling variance estimates labels
    ####################################

    # Assuming balanced sampling variance estimates across studies
    if (is.numeric(sigma2_dist) && length(sigma2_dist) == 1) {
      samp_method_sigma2 <- "balanced"
      sigma2j <- sigma2_dist
    }

    # Stylized distribution of sampling variance estimates
    if (is.function(sigma2_dist)) {
      samp_method_sigma2 <- "stylized"
      sigma2j <- sigma2_dist(1000)
    }

    # Empirical distribution of sampling variance estimates across studies
    if (is.numeric(sigma2_dist) & length(sigma2_dist) > 1 & length(sigma2_dist) != length(n_ES_dist)) {
      samp_method_sigma2 <- "empirical"
      sigma2j <- sigma2_dist
    }

    #########################################
    # Number of effect sizes per study labels
    #########################################

    if (is.numeric(n_ES_dist) && length(n_ES_dist) == 1) {
      # Assuming that all studies yield the same number of effect sizes
      samp_method_kj <- "balanced"
      kj <- n_ES_dist
    } else if (is.function(n_ES_dist)) {
      # Stylized distribution of the number of effect sizes per study
      samp_method_kj <- "stylized"
      kj <- n_ES_dist(1000)
    } else if (is.numeric(n_ES_dist) && length(n_ES_dist) > 1 && length(sigma2_dist) != length(n_ES_dist)) {
      # Empirical distribution of the number of effect sizes per study
      samp_method_kj <- "empirical"
      kj <- n_ES_dist
    } else if (length(sigma2_dist) > 1 && length(n_ES_dist) > 1 && length(sigma2_dist) == length(n_ES_dist)) {
      # If both sigma2js and kjs are empirically obtained
      samp_method_sigma2 <- "empirical_combi"
      samp_method_kj <- "empirical_combi"
      sigma2j <- sigma2_dist
      kj <- n_ES_dist
    }

    if (mu > 0) {

      f <- function(J){

        precision_MADE_engine(
          J = J, tau = tau, omega = omega, mu = mu, rho = rho,
          level = level,
          model = model,
          var_df = var_df,
          sigma2_dist = sigma2_dist, n_ES_dist = n_ES_dist,
          iterations = iterations, average_precision = TRUE,
          J_hi = J, seed = seed
        )$lower_bound - x

      }

    } else if (mu < 0){

      f <- function(J){

        precision_MADE_engine(
          J = J, tau = tau, omega = omega, mu = mu, rho = rho,
          level = level,
          model = model,
          var_df = var_df,
          sigma2_dist = sigma2_dist, n_ES_dist = n_ES_dist,
          iterations = iterations, average_precision = TRUE,
          J_hi = J, seed = seed
        )$upper_bound - x

      }

    }

    J_needed <- round(stats::uniroot(f, interval = c(4, upper), extendInt = extendInt)$root)

    if ("Satt" %in% var_df) var_df <- "Model+Satt"

    tibble(
      mu = mu,
      tau = tau,
      omega = omega,
      rho = rho,
      level = level,
      threshold_val = x,
      studies_needed = J_needed,
      iterations = iterations,
      model = paste(model, var_df, sep = "-"),
      samp_method_sigma2 = samp_method_sigma2,
      samp_method_kj = samp_method_kj
    )


  }
