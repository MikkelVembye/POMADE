
#' @title Minimum Detectable Effect Size (MDES) for Meta-Analysis With Dependent
#'   Effect Sizes
#'
#' @description Compute the minimum detectable effect size in a meta-analysis of
#'   dependent effect size estimates, given a specified number of studies, power
#'   level, estimation method, and further assumptions about the distribution of
#'   studies.
#'
#' @param J Number of studies. Can be one value or a vector of multiple values.
#' @template common-arg
#' @param target_power Numerical value specifying the target power level. Can be
#'   one value or a vector of multiple values.
#' @param upper Numerical value containing the upper bound of the interval to be
#'   searched for the MDES.
#' @param show_lower Logical value indicating whether to report lower bound of
#'   the interval searched for the MDES. Default is \code{FALSE}.
#'
#' @return Returns a \code{tibble} with information about the expectation of the
#'   number of studies, the between-study and within-study variance components,
#'   the sample correlation, the contrast effect, the level of statistical
#'   significance, the target power value(s), the minimum detectable effect
#'   size, the number of iterations, the model to handle dependent effect sizes,
#'   and the methods used to obtain sampling variance estimates as well as the
#'   number effect sizes per study.
#'
#' @export
#'
#' @examples
#'
#' mdes_MADE(
#'   J = 30,
#'   tau = 0.05,
#'   omega = 0.02,
#'   rho = 0.2,
#'   model = "CHE",
#'   var_df = "RVE",
#'   sigma2_dist = 4 / 100,
#'   n_ES_dist = 6,
#'   seed = 10052510
#' )
#'
#'

mdes_MADE <-
  function(
    J,
    tau,
    omega,
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
    upper = 2,
    show_lower = FALSE

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
        tau = tau,
        omega = omega,
        rho = rho,
        alpha = alpha,
        target_power = target_power,
        d = d,
        model = model,
        var_df = var_df
      )

    params <- purrr::cross_df(design_factors) |>
      filter(model != "CE" | var_df == "RVE")

    furrr_seed <- if (is.null(seed)) TRUE else NULL

    suppressPackageStartupMessages(
      res <- furrr::future_pmap_dfr(
        .l = params, .f = mdes_MADE_engine,
        sigma2_dist = sigma2_dist, n_ES_dist = n_ES_dist, iterations = iterations,
        seed = seed, upper = upper, extendInt = "yes",
        .options = furrr::furrr_options(seed = furrr_seed)
      ) |>
        dplyr::arrange(dplyr::across(J:target_power))
    )

    if (!show_lower) res <- dplyr::select(res, -lower)

    tibble::new_tibble(res, class = "mdes")

  }


mdes_MADE_engine <-
  function(
    J,
    tau,
    omega,
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
    upper = 2,
    extendInt = "yes"
  ) {


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

    f <- function(mu) {

      power_MADE_engine(
        J = J, mu = mu, tau = tau, omega = omega,
        rho = rho, alpha = alpha, d = d,
        model = model, var_df = var_df,
        sigma2_dist = sigma2_dist, n_ES_dist = n_ES_dist,
        iterations = iterations, average_power = TRUE,
        seed = seed
      )$power - target_power

    }

    # Determine lower bound of interval to search
    wbar <- mean(kj / (kj * tau^2 + kj * rho * sigma2j + omega^2 + (1 - rho) * sigma2j))
    lower <- d + (qnorm(1 - alpha / 2) - qnorm(1 + alpha / 2 - target_power)) / sqrt(wbar * J)
    while (f(lower) > 0) lower <- lower / 2
    if (lower > upper) upper <- 2 * lower
    interval <- c(lower, upper)

    # Find MDES
    MDES <- stats::uniroot(f, interval = interval, extendInt = extendInt)$root

    # To align the results with the power_MADE function
    if ("Satt" %in% var_df) var_df <- "Model+Satt"

    tibble(
      J = J,
      tau = tau,
      omega = omega,
      rho = rho,
      d = d,
      alpha = alpha,
      target_power = target_power,
      lower = lower,
      MDES = MDES,
      iterations = iterations,
      model = paste(model, var_df, sep = "-"),
      samp_method_sigma2 = samp_method_sigma2,
      samp_method_kj = samp_method_kj
    )

  }
