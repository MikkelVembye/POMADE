# Test

power_MADE <-
  function(
    J, tau2, omega2, beta, rho,
    model = c("CHE", "MLMA", "CE"),
    var_df = c("Model", "Satt", "RVE"),
    sigma2_dist = NULL,
    n_ES_dist = NULL,
    alpha = .05,
    d = 0
  ){

  if (length(sigma2_dist) == 1){

    sigma2j <- rep(sigma2_dist, length.out = J)

  }

  if (length(n_ES_dist) == 1) kj <- rep(n_ES_dist, length.out = J)

  sigma2j <- sigma2_dist

  wj <- kj / (kj * tau2 + kj * rho * sigma2j + omega2 + (1 - rho) * sigma2j)
  W <- sum(wj)

  var_b <- 1 / W

  lambda <- (beta - d) / sqrt(var_b)

  df_CHE_app <- Satt_df(wj)

  # Eq 16
  power_CHE_RVE <- power_t(df = df_CHE_app, lambda = lambda, alpha = alpha)

  res <-
    tibble(
      var_b,
      df = df_CHE_app,
      power_CHE_RVE,
      method = paste(model, var_df, sep = "-")
    )

  res

}

