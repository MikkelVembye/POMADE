
#' @title Power Approximation for Overall Average Effects in  Meta-Analysis With Dependent Effect Sizes
#'
#' @description Compute power of the test of the overall average effect size in
#'   a meta-analysis of dependent effect size estimates, given a specified
#'   number of studies, effect size of practical concern, estimation method, and
#'   further assumptions about the distribution of studies.
#'
#' @details Find all background material behind the power approximations in
#'   Vembye, Pustejovsky, & Pigott (2022), including arguments for why it is suggested
#'   neither to conduct power analysis based on balanced assumptions about
#'   the number of effects per study and the study variance nor to use the original
#'   power approximation assuming independence among effect sizes (Hedges & Pigott, 2001).
#'
#' @references Vembye, M. H., Pustejovsky, J. E., & Pigott, T. D. (2022).
#'   Power approximations for overall average effects in meta-analysis with dependent effect sizes.
#'   \emph{Journal of Educational and Behavioral Statistics}, 1–33. \doi{10.3102/10769986221127379}
#'
#' Hedges, L. V., & Pigott, T. D. (2001). The power of statistical tests in meta-analysis.
#' \emph{Psychological Methods}, 6(3), 203–217. \doi{10.1037/1082-989X.6.3.203}
#'
#'
#' @param J Number of studies. Can be one value or a vector of multiple values.
#' @param mu Effect size of practical concern. Can be one value or a vector of multiple values.
#' @template common-arg
#' @param average_power Logical indicating whether to calculate average power
#'   across the iterations for each condition.
#'
#' @return Returns a \code{tibble} with information about the expectation of the
#'   number of studies, the effect size of practical concern, the between-study
#'   and within-study variance components, the sample correlation, the contrast
#'   effect, the level of statistical significance, the sampling variance of
#'   overall average effect size of practical concern, the degrees of freedom,
#'   the power, the mcse, the number of iterations, the model to handle
#'   dependent effect sizes, and the methods used to obtain sampling variance
#'   estimates as well as the number effect sizes per study.
#'
#' @importFrom magrittr %>%
#' @importFrom stats df
#' @import dplyr
#'
#' @export
#'
#' @examples
#' power <- power_MADE(
#'    J = c(40, 60),
#'    mu = 0.2,
#'    tau = 0.2,
#'    omega = 0.1,
#'    rho = 0.7,
#'    sigma2_dist = \(x) rgamma(x, shape = 5, rate = 10),
#'    n_ES_dist = \(x) 1 + stats::rpois(x, 5.5 - 1),
#'    model = c("CHE", "MLMA", "CE"),
#'    var_df = c("Model", "Satt", "RVE"),
#'    alpha = .05,
#'    seed = 10052510,
#'    iterations =5
#'  )
#'
#' power
#'
#'
#'
#'


power_MADE <-
  function(
    J, mu, tau, omega, rho,
    alpha = 0.05,
    d = 0,

    model = "CHE",
    var_df = "RVE",

    sigma2_dist = NULL,
    n_ES_dist = NULL,

    iterations = 100,
    seed = NULL,
    warning = TRUE,
    average_power = TRUE

  ) {


    if (warning) {
      if (is.numeric(sigma2_dist) && length(sigma2_dist) == 1 || is.numeric(n_ES_dist) && length(n_ES_dist) == 1) {
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
        mu = mu,
        tau = tau,
        omega = omega,
        rho = rho,
        d = d
      )

    params <- purrr::cross_df(design_factors)

    furrr_seed <- if (is.null(seed)) TRUE else NULL

    suppressPackageStartupMessages(
      dat <-
        params %>%
        mutate(
          res = furrr::future_pmap(
            .l = params,
            .f = power_MADE_engine,
            alpha = alpha,
            model = model,
            var_df = var_df,
            sigma2_dist = sigma2_dist,
            n_ES_dist = n_ES_dist,
            iterations = iterations,
            average_power = average_power,
            seed = seed,
            .options = furrr::furrr_options(seed = furrr_seed)
          )
        ) %>%
        tidyr::unnest(res)
    )

    tibble::new_tibble(dat, class = "power")

}


power_MADE_engine <-
  function(
    J, mu, tau, omega, rho,
    alpha = 0.05,
    d = 0,

    model = "CHE",
    var_df = "RVE",

    sigma2_dist = NULL,
    n_ES_dist = NULL,

    iterations = 5,
    average_power = TRUE,
    J_hi = J,
    seed = NULL
  ){

  if (!is.null(seed)) set.seed(seed)

  # J_hi must be at least as large as J
  if (J_hi < J) J_hi <- J

  ###################################
  # Sampling variance estimates
  ###################################

  # Assuming balanced sampling variance estimates across studies
  if (is.numeric(sigma2_dist) && length(sigma2_dist) == 1) {

    samp_method_sigma2 <- "balanced"
    sigma2js <- sigma2_dist

  }

  # Stylized distribution of sampling variance estimates
  if (is.function(sigma2_dist)) {

    samp_method_sigma2 <- "stylized"
    sigma2js <- purrr::rerun(iterations, sigma2_dist(J_hi)[1:J])

  }

  # Empirical distribution of sampling variance estimates across studies
  if (is.numeric(sigma2_dist) & length(sigma2_dist) > 1 & length(sigma2_dist) != length(n_ES_dist)) {

    samp_method_sigma2 <- "empirical"
    sigma2js <- purrr::rerun(iterations, sample(sigma2_dist, J_hi, replace = TRUE)[1:J])

  }

  ###################################
  # Number of effect size per study
  ###################################

  if (is.numeric(n_ES_dist) && length(n_ES_dist) == 1) {

    # Assuming that all studies yields the same number of effect sizes
    samp_method_kj <- "balanced"
    kjs <- n_ES_dist

  } else if (is.function(n_ES_dist)) {

    # Stylized distribution of the number of effect sizes per study
    samp_method_kj <- "stylized"
    kjs <- purrr::rerun(iterations, n_ES_dist(J_hi)[1:J])

  } else if (is.numeric(n_ES_dist) && length(n_ES_dist) > 1 && length(sigma2_dist) != length(n_ES_dist)) {

    # Empirical distribution of the number of effect sizes per study
    samp_method_kj <- "empirical"
    kjs <- purrr::rerun(iterations, sample(n_ES_dist, J_hi, replace = TRUE)[1:J])

  } else if (length(sigma2_dist) > 1 && length(n_ES_dist) > 1 && length(sigma2_dist) == length(n_ES_dist)) {

    # If both sigma2js and kjs are empirically obtained

    samp_method_sigma2 <- "empirical_combi"
    samp_method_kj <- "empirical_combi"

    pilot_data <- bind_cols(sigma2j = sigma2_dist, kj = n_ES_dist)
    id <- seq_along(sigma2_dist)
    pilot_sigma2j_kj <- purrr::rerun(iterations, pilot_data[sample(id, size = J_hi, replace = TRUE)[1:J],])
    sigma2js <- purrr::map(pilot_sigma2j_kj, ~ .x$sigma2j)
    kjs <- purrr::map(pilot_sigma2j_kj, ~ .x$kj)

  }


  # Generate results across iterations

  res <-
    purrr::map2_dfr(
      .x = sigma2js, .y = kjs, .f = power_MADE_single,
      J = J, mu = mu, tau = tau, omega = omega, rho = rho,
      model = model, var_df, alpha = alpha, d = d, .id = "iteration"
    ) |>
    mutate(
      samp_method_sigma2 = samp_method_sigma2,
      samp_method_kj = samp_method_kj,
    )

  if (average_power) {

    res <-
      res |>
      group_by(alpha, model, samp_method_sigma2, samp_method_kj) |>
      summarise(
        mcse = sd(power) / sqrt(n()),
        across(c(var_b, df, power), mean),
        iterations = n(),
        .groups = "drop"
      ) |>
      relocate(mcse, .after = power) |>
      relocate(model:samp_method_kj, .after = iterations)

  }

  return(res)

}




power_MADE_single <-
  function(
    J, mu, tau, omega, rho,
    sigma2j,
    kj,
    model = c("CHE", "MLMA", "CE"),
    var_df = c("Model", "Satt", "RVE"),
    alpha,
    d

  ) {


    # ensure J is an integer
    J <- as.integer(floor(J))

    if (length(sigma2j) != J) sigma2j <- rep(sigma2j, length.out = J)

    if (length(kj) != J) kj <- rep(kj, length.out = J)

    res <- tibble()

    ###################
    # CHE models
    ###################

    if ("CHE" %in% model) {

      # Equation 6 in Vembye, Pustejovsky, & Pigott (2022)
      wj <- kj / (kj * tau^2 + kj * rho * sigma2j + omega^2 + (1 - rho) * sigma2j)
      W <- sum(wj)

      # Equation 8 in Vembye, Pustejovsky, & Pigott (2022)
      var_b <- 1 / W

      # Equation 10 in Vembye, Pustejovsky, & Pigott (2022)
      lambda <- (mu - d) / sqrt(var_b)

      x <- sum(wj^2) / W
      y <- sum(wj^2 / kj) / W

      s <- x^2 + W * x - 2 * sum(wj^3) / W
      t <- y^2 + sum(wj^2 / kj^2) + sum((kj - 1) / (omega^2 + (1 - rho) * sigma2j)^2) - 2 * sum(wj^3 / kj^2) / W
      u <- x * y + W * y - 2 * sum(wj^3 / kj) / W

      # Equation 12 in Vembye, Pustejovsky, & Pigott (2022)
      df_CHE_satt <- (s * t - u^2) / (s * y^2 + t * x^2 - 2 * u * x * y)

      if ("Model" %in% var_df) {

        # Equation 11 in Vembye, Pustejovsky, & Pigott (2022)
        power_CHE_model <- power_t(df = df_CHE_satt, lambda = lambda, alpha = alpha, df_test = J - 1)

        res <- tibble(
          var_b = var_b,
          df = J - 1,
          alpha = alpha,
          power = power_CHE_model,
          model = "CHE-Model"
        ) %>%
          bind_rows(res, .)

      }

      if ("Satt" %in% var_df) {

        # Equation 11 in Vembye, Pustejovsky, & Pigott (2022)
        power_CHE_satt <- power_t(df = df_CHE_satt, lambda = lambda, alpha = alpha)

        res <- tibble(
          var_b = var_b,
          df = round(df_CHE_satt, 1),
          alpha = alpha,
          power = power_CHE_satt,
          model = "CHE-Model+Satt"
        ) %>%
          bind_rows(res, .)

      }

      if ("RVE" %in% var_df) {

        # Equation 16 in Vembye, Pustejovsky, & Pigott (2022)
        df_CHE_app <- Satt_df(wj)

        # Equation 15 in Vembye, Pustejovsky, & Pigott (2022)
        power_CHE_RVE <- power_t(df = df_CHE_app, lambda = lambda, alpha = alpha)

        res <-
          tibble(
            var_b,
            df = df_CHE_app,
            alpha = alpha,
            power = power_CHE_RVE,
            model = "CHE-RVE"
          ) %>%
          bind_rows(res, .)

      }


    }

    #####################
    # MLMA models
    #####################

    if ("MLMA" %in% model) {

      # See Supplementary Material to Vembye, Pustejovsky, & Pigott (2022)
      tau_omega_tilde <- find_tau_omega(tau = tau, omega = omega,
                                        phi = rho, rho = 0,
                                        k_j = kj, sigmasq_j = sigma2j)
      # _t = tilde
      tau2_t <- tau_omega_tilde$tau_tilde^2
      omega2_t <- tau_omega_tilde$omega_tilde^2

      # Equation 21 in Vembye, Pustejovsky, & Pigott (2022)
      wj_t <- kj / (kj * tau2_t + omega2_t + sigma2j)
      W_t <- sum(wj_t)

      # Equation 22 in Vembye, Pustejovsky, & Pigott (2022)
      S_t <- 1/W_t^2 * sum(wj_t^2 * (tau^2 + rho * sigma2j + (omega^2 + (1 - rho) * sigma2j) / kj) )

      lambda_MLMA <- (mu - d) / sqrt(S_t)


      x_t <- sum(wj_t^2) / W_t
      y_t <- sum(wj_t^2 / kj) / W_t

      s_t <- x_t^2 + W_t * x_t - 2 * sum(wj_t^3) / W_t
      t_t <- y_t^2 + sum(wj_t^2 / kj^2) + sum((kj - 1) / (omega2_t + sigma2j)^2) - 2 * sum(wj_t^3 / kj^2) / W_t
      u_t <- x_t * y_t + W_t * y_t - 2 * sum(wj_t^3 / kj) / W_t

      # Equation 12 in Vembye, Pustejovsky, & Pigott (2022)
      # w_j substituted with w^tilde_j
      df_MLMA_satt <- (s_t * t_t - u_t^2) / (s_t * y_t^2 + t_t * x_t^2 - 2 * u_t * x_t * y_t)

      g <- 1 / sqrt(W_t * S_t)

      if ("Model" %in% var_df) {

        # Equation 23 in Vembye, Pustejovsky, & Pigott (2022)
        power_MLMA_model <- power_t(df = df_MLMA_satt, lambda = lambda_MLMA, alpha = alpha, df_test = J - 1, g = g)

        res <- tibble(
          var_b = S_t,
          df = J - 1,
          alpha = alpha,
          power = power_MLMA_model,
          model = "MLMA-Model"
        ) %>%
          bind_rows(res, .)

      }

      if ("Satt" %in% var_df) {

        # Equation 23 in Vembye, Pustejovsky, & Pigott (2022)
        power_MLMA_satt <- power_t(df = df_MLMA_satt, lambda = lambda_MLMA, alpha = alpha, g = g)

        res <- tibble(
          var_b = S_t,
          df = df_MLMA_satt,
          alpha = alpha,
          power = power_MLMA_satt,
          model = "MLMA-Model+Satt"
        ) %>%
          bind_rows(res, .)

      }

      if ("RVE" %in% var_df) {

        # Equation 16 in Vembye, Pustejovsky, & Pigott (2022)
        # w_j substituted with w^tilde_j
        df_MLMA_app <- Satt_df(wj_t)

        # Equation 23 in Vembye, Pustejovsky, & Pigott (2022)
        power_MLMA_RVE <- power_t(df = df_MLMA_app, lambda = lambda_MLMA, alpha = alpha)

        res <-
          tibble(
            var_b = S_t,
            df = df_MLMA_app,
            alpha = alpha,
            power = power_MLMA_RVE,
            model = "MLMA-RVE"
          ) %>%
          bind_rows(res, .)

      }

    }

    ###################
    # CE-RVE model
    ###################

    if ("CE" %in% model & "RVE" %in% var_df) {

      # Equation 17 in Vembye, Pustejovsky, & Pigott (2022)
      tau2_e <- tau^2 + omega^2 * (1 - sum(1 / (kj * sigma2j^2))) / (1 - sum(1 / sigma2j^2))

      # _dd = dotdot
      wj_dd <- 1 / (sigma2j + tau2_e)

      W_dd <- sum(wj_dd)

      S_dd <- 1/W_dd^2 * sum(wj_dd^2 * (tau^2 + rho * sigma2j + (1 / kj) * (omega^2 + (1 - rho) * sigma2j)) )

      lambda_CE <- (mu - d) / sqrt(S_dd)

      # Equation 16 in Vembye, Pustejovsky, & Pigott (2022)
      # w_j substituted with w^dotdot_j
      df_CE_app <- Satt_df(wj_dd)

      power_CE <- power_t(df = df_CE_app, lambda = lambda_CE, alpha = alpha)

      res <-
        tibble(
          var_b = S_dd,
          df = df_CE_app,
          alpha = alpha,
          power = power_CE,
          model = "CE-RVE"
        ) %>%
        bind_rows(res, .)

    }

    res

  }









































