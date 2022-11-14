#' @title Precision Analysis for Meta-Analysis of Dependent Effect Sizes
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#'
#' @param J Number of studies.
#' @template common-arg-precision
#' @template common-arg-precision2
#' @param average_precision Average \code{"lower_bound"}, \code{"upper_bound"}, and \code{"width"} across the number of iteration for each condition.
#'
#' @return Returns a \code{tibble} with information about the expectation of the number of
#' studies, the effect size of practical concern, the between-study and within-study variance components,
#' the sample correlation, the desired coverage level for confidence intervals,
#' the sampling variance of overall average effect size of practical concern, the degrees of freedom,
#' the lower bound of the confidence interval, the upper bound of the confidence interval, and the width of confidence interval,
#' the MCSE of the width, the number of iterations, the model to handle dependent effect sizes,
#' and the methods used to obtain sampling variance estimates as well as the number effect sizes per study.
#'
#' @importFrom magrittr %>%
#' @importFrom stats df
#' @import dplyr
#'
#' @export
#'
#' @examples
#' precision <-
#'  precision_MADE(
#'    J = c(40, 60),
#'    mu = 0.1,
#'    tau = 0.2,
#'    omega = 0.1,
#'    rho = 0.7,
#'    sigma2_dist = \(x) rgamma(x, shape = 5, rate = 10),
#'    n_ES_dist = \(x) 1 + stats::rpois(x, 5.5 - 1),
#'    model = c("CHE", "MLMA", "CE"),
#'    var_df = c("Model", "Satt", "RVE"),
#'    level = .95,
#'    seed = 10052510
#'  )
#'
#' precision
#'
#'
#'
#'

precision_MADE <-
  function(
    J,
    mu,
    tau,
    omega,
    rho,
    level = .95,

    model = "CHE",
    var_df = "RVE",

    sigma2_dist = NULL,
    n_ES_dist = NULL,

    iterations = 100,
    seed = NULL,
    warning = TRUE,
    average_precision = TRUE

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
        rho = rho
      )

    params <- purrr::cross_df(design_factors)

    furrr_seed <- if (is.null(seed)) TRUE else NULL

    suppressPackageStartupMessages(
      dat <-
        params %>%
        mutate(
          res = furrr::future_pmap(
            .l = params,
            .f = precision_MADE_engine,
            level = level,
            model = model,
            var_df = var_df,
            sigma2_dist = sigma2_dist,
            n_ES_dist = n_ES_dist,
            iterations = iterations,
            average_precision = average_precision,
            seed = seed,
            .options = furrr::furrr_options(seed = furrr_seed)
          )
        ) %>%
        tidyr::unnest(res)
    )

    tibble::new_tibble(dat, class = "precision")

  }


precision_MADE_engine <-
  function(
    J, mu, tau, omega, rho,
    level = 0.95,

    model = "CHE",
    var_df = "RVE",

    sigma2_dist = NULL,
    n_ES_dist = NULL,

    iterations = 5,
    average_precision = TRUE,
    J_hi = J,
    seed = NULL

  ) {

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
        .x = sigma2js, .y = kjs, .f = precision_MADE_single,
        J = J, mu = mu, tau = tau, omega = omega, rho = rho,
        model = model, var_df, level = level, .id = "iteration"
      ) |>
      mutate(
        samp_method_sigma2 = samp_method_sigma2,
        samp_method_kj = samp_method_kj,
      )


    if (average_precision) {

      res <-
        res |>
        group_by(level, model, samp_method_sigma2, samp_method_kj) |>
        summarise(
          width_mcse = sqrt( var(upper_bound - lower_bound) / n() ),
          across(c(se_b, df, lower_bound:width), mean),
          iterations = n(),
          .groups = "drop"
        ) |>
        relocate(width_mcse, .after = width) |>
        relocate(model:samp_method_kj, .after = iterations)

    }

    return(res)


  }


precision_MADE_single <-
  function(
    J, mu, tau, omega, rho,
    sigma2j,
    kj,
    model = c("CHE", "MLMA", "CE"),
    var_df = c("Model", "Satt", "RVE"),
    level

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
      se_b <- sqrt(1 / W)

      x <- sum(wj^2) / W
      y <- sum(wj^2 / kj) / W

      s <- x^2 + W * x - 2 * sum(wj^3) / W
      t <- y^2 + sum(wj^2 / kj^2) + sum((kj - 1) / (omega^2 + (1 - rho) * sigma2j)^2) - 2 * sum(wj^3 / kj^2) / W
      u <- x * y + W * y - 2 * sum(wj^3 / kj) / W

      # Equation 12 in Vembye, Pustejovsky, & Pigott (2022)
      df_CHE_satt <- (s * t - u^2) / (s * y^2 + t * x^2 - 2 * u * x * y)

      if ("Model" %in% var_df) {

        lb_CHE_model <- ci_lower(level = level, df = J-1, mu = mu, se = se_b)
        ub_CHE_model <- ci_upper(level = level, df = J-1, mu = mu, se = se_b)

        res <- tibble(
          se_b = se_b,
          df = J - 1,
          level = level,
          lower_bound = lb_CHE_model,
          upper_bound = ub_CHE_model,
          width = upper_bound - lower_bound,
          model = "CHE-Model"
        ) %>%
          bind_rows(res, .)

      }

      if ("Satt" %in% var_df) {

        lb_CHE_satt <- ci_lower(level = level, df = df_CHE_satt, mu = mu, se = se_b)
        ub_CHE_satt <- ci_upper(level = level, df = df_CHE_satt, mu = mu, se = se_b)

        res <- tibble(
          se_b = se_b,
          df = round(df_CHE_satt, 1),
          level = level,
          lower_bound = lb_CHE_satt,
          upper_bound = ub_CHE_satt,
          width = upper_bound - lower_bound,
          model = "CHE-Model+Satt"
        ) %>%
          bind_rows(res, .)

      }

      if ("RVE" %in% var_df) {

        # Equation 16 in Vembye, Pustejovsky, & Pigott (2022)
        df_CHE_app <- Satt_df(wj)

        lb_CHE_RVE <- ci_lower(level = level, df = df_CHE_app, mu = mu, se = se_b)
        ub_CHE_RVE <- ci_upper(level = level, df = df_CHE_app, mu = mu, se = se_b)

        res <-
          tibble(
            se_b = se_b,
            df = df_CHE_app,
            level = level,
            lower_bound = lb_CHE_RVE,
            upper_bound = ub_CHE_RVE,
            width = upper_bound - lower_bound,
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

      se_b_t <- sqrt(S_t)

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

        lb_MLMA_model <- ci_lower(level = level, df = J-1, mu = mu, se = se_b_t, g = g)
        ub_MLMA_model <- ci_upper(level = level, df = J-1, mu = mu, se = se_b_t, g = g)

        res <- tibble(
          se_b = se_b_t,
          df = J - 1,
          level = level,
          lower_bound = lb_MLMA_model,
          upper_bound = ub_MLMA_model,
          width = upper_bound - lower_bound,
          model = "MLMA-Model"
        ) %>%
          bind_rows(res, .)

      }

      if ("Satt" %in% var_df) {

        lb_MLMA_satt <- ci_lower(level = level, df = df_MLMA_satt, mu = mu, se = se_b_t, g = g)
        ub_MLMA_satt <- ci_upper(level = level, df = df_MLMA_satt, mu = mu, se = se_b_t, g = g)

        res <- tibble(
          se_b = se_b_t,
          df = df_MLMA_satt,
          level = level,
          lower_bound = lb_MLMA_satt,
          upper_bound = ub_MLMA_satt,
          width = upper_bound - lower_bound,
          model = "MLMA-Model+Satt"
        ) %>%
          bind_rows(res, .)

      }

      if ("RVE" %in% var_df) {

        # Equation 16 in Vembye, Pustejovsky, & Pigott (2022)
        # w_j substituted with w^tilde_j
        df_MLMA_app <- Satt_df(wj_t)

        lb_MLMA_RVE <- ci_lower(level = level, df = df_MLMA_app, mu = mu, se = se_b_t, g = g)
        ub_MLMA_RVE <- ci_upper(level = level, df = df_MLMA_app, mu = mu, se = se_b_t, g = g)

        res <-
          tibble(
            se_b = se_b_t,
            df = df_MLMA_app,
            level = level,
            lower_bound = lb_MLMA_RVE,
            upper_bound = ub_MLMA_RVE,
            width = upper_bound - lower_bound,
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

      se_b_dd <- sqrt(S_dd)

      # Equation 16 in Vembye, Pustejovsky, & Pigott (2022)
      # w_j substituted with w^dotdot_j
      df_CE_app <- Satt_df(wj_dd)

      lb_CE_RVE <- ci_lower(level = level, df = df_CE_app, mu = mu, se = se_b_dd)
      ub_CE_RVE <- ci_upper(level = level, df = df_CE_app, mu = mu, se = se_b_dd)

      res <-
        tibble(
          se_b = se_b_dd,
          df = df_CE_app,
          level = level,
          lower_bound = lb_CE_RVE,
          upper_bound = ub_CE_RVE,
          width = upper_bound - lower_bound,
          model = "CE-RVE"
        ) %>%
        bind_rows(res, .)

   }

    res

  }
