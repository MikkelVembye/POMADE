# Test

#power_MADE <-
#  function(...){
#
#  }


power_MADE_engine <-
  function(
    J, tau2, omega2, beta, rho,
    sigma2j,
    kj,
    model = c("CHE", "MLMA", "CE"),
    var_df = c("Model", "Satt", "RVE"),
    alpha,
    d

  ){


    if (length(sigma2j) != J) sigma2j <- rep(sigma2j, length.out = J)

    if (length(kj) != J) kj <- rep(kj, length.out = J)

    res <- tibble()

    ###################
    # CHE models
    ###################

    if("CHE" %in% model) {

      # Equation 6 in Vembye, Pustejovsky, & Pigott (2022)
      wj <- kj / (kj * tau2 + kj * rho * sigma2j + omega2 + (1 - rho) * sigma2j)
      W <- sum(wj)

      # Equation 8 in Vembye, Pustejovsky, & Pigott (2022)
      var_b <- 1 / W

      # Equation 10 in Vembye, Pustejovsky, & Pigott (2022)
      lambda <- (beta - d) / sqrt(var_b)

      x <- sum(wj^2) / W
      y <- sum(wj^2 / kj) / W

      s <- x^2 + W * x - 2 * sum(wj^3) / W
      t <- y^2 + sum(wj^2 / kj^2) + sum((kj - 1) / (omega2 + (1 - rho) * sigma2j)^2) - 2 * sum(wj^3 / kj^2) / W
      u <- x * y + W * y - 2 * sum(wj^3 / kj) / W

      # Equation 12 in Vembye, Pustejovsky, & Pigott (2022)
      df_CHE_satt <- (s * t - u^2) / (s * y^2 + t * x^2 - 2 * u * x * y)

      if ("Model" %in% var_df) {

        # Equation 11 in Vembye, Pustejovsky, & Pigott (2022)
        power_CHE_model <- power_t(df = df_CHE_satt, lambda = lambda, alpha = alpha, df_test = J - 1)

        res <- tibble(
          var_b = var_b,
          df = J - 1,
          power_CHE_model,
          method = "CHE-Model"
        ) %>%
          bind_rows(res, .)

      }

      if ("Satt" %in% var_df){

        # Equation 11 in Vembye, Pustejovsky, & Pigott (2022)
        power_CHE_satt <- power_t(df = df_CHE_satt, lambda = lambda, alpha = alpha)

        res <- tibble(
          var_b = var_b,
          df = round(df_CHE_satt, 1),
          power_CHE_satt,
          method = "CHE-Model+Satt"
        ) %>%
          bind_rows(res, .)

      }

      if ("RVE" %in% var_df){

        # Equation 16 in Vembye, Pustejovsky, & Pigott (2022)
        df_CHE_app <- Satt_df(wj)

        # Equation 15 in Vembye, Pustejovsky, & Pigott (2022)
        power_CHE_RVE <- power_t(df = df_CHE_app, lambda = lambda, alpha = alpha)

        res <-
          tibble(
            var_b,
            df = df_CHE_app,
            power_CHE_RVE,
            method = "CHE-RVE"
          ) %>%
          bind_rows(res, .)

      }


    }

    #####################
    # MLMA models
    #####################

    if ("MLMA" %in% model) {

      # See Supplementary Material to Vembye, Pustejovsky, & Pigott (2022)
      tau_omega_tilde <- find_tau_omega(tau = sqrt(tau2), omega = sqrt(omega2),
                                        phi = rho, rho = 0,
                                        k_j = kj, sigmasq_j = sigma2j)
      # _t = tilde
      tau2_t <- tau_omega_tilde$tau_tilde^2
      omega2_t <- tau_omega_tilde$omega_tilde^2

      # Equation 21 in Vembye, Pustejovsky, & Pigott (2022)
      wj_t <- kj / (kj * tau2_t + omega2_t + sigma2j)
      W_t <- sum(wj_t)

      # Equation 22 in Vembye, Pustejovsky, & Pigott (2022)
      S_t <- 1/W_t^2 * sum(wj_t^2 * (tau2 + rho * sigma2j + (omega2 + (1 - rho) * sigma2j) / kj) )

      lambda_MLMA <- (beta - d) / sqrt(S_t)


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
          power_MLMA_model,
          method = "MLMA-Model"
        ) %>%
          bind_rows(res, .)

      }

      if ("Satt" %in% var_df){

        # Equation 23 in Vembye, Pustejovsky, & Pigott (2022)
        power_MLMA_satt <- power_t(df = df_MLMA_satt, lambda = lambda_MLMA, alpha = alpha, g = g)

        res <- tibble(
          var_b = S_t,
          df = df_MLMA_satt,
          power_MLMA_satt,
          method = "MLMA-Model+Satt"
        ) %>%
          bind_rows(res, .)

      }

      if ("RVE" %in% var_df){

        # Equation 16 in Vembye, Pustejovsky, & Pigott (2022)
        # w_j substituted with w^tilde_j
        df_MLMA_app <- Satt_df(wj_t)

        # Equation 23 in Vembye, Pustejovsky, & Pigott (2022)
        power_MLMA_RVE <- power_t(df = df_MLMA_app, lambda = lambda_MLMA, alpha = alpha)

        res <-
          tibble(
            var_b = S_t,
            df = df_MLMA_app,
            power_MLMA_RVE,
            method = "MLMA-RVE"
          ) %>%
            bind_rows(res, .)

      }

    }

    ###################
    # CE-RVE model
    ###################

    if ("CE" %in% model & "RVE" %in% var_df){

      tau2_e <- tau2 + omega2 * (1 - sum(1 / (kj * sigma2j^2))) / (1 - sum(1 / sigma2j^2))

      # _d = dotdot
      wj_dd <- 1 / (sigma2j + tau2_e)

      W_dd <- sum(wj_dd)

      S_dd <- 1/W_dd^2 * sum(wj_dd^2 * (tau2 + rho * sigma2j + (1 / kj) * (omega2 + (1 - rho) * sigma2j)) )

      lambda_CE <- (beta - d) / sqrt(S_dd)

      # Equation 16 in Vembye, Pustejovsky, & Pigott (2022)
      # w_j substituted with w^dotdot_j
      df_CE_app <- Satt_df(wj_dd)

      power_CE <- power_t(df = df_CE_app, lambda = lambda_CE, alpha = alpha)

      res <-
        tibble(
          var_b = S_dd,
          df = df_CE_app,
          power_CE,
          method = "CE-RVE"
        ) %>%
          bind_rows(res, .)

    }

    res

  }














































