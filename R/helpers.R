#--------------------------------------------------------------------------
# Helper functions
#--------------------------------------------------------------------------

#' @importFrom stats qt
#' @importFrom stats pt


# Satterthwaite df approximation

Satt_df <- function(wj) {
  wj2 <- wj^2
  wj3 <- wj^3
  W <- sum(wj)

  df_inv <- sum(wj2 / (W - wj)^2) - 2 / W *  sum(wj3 / (W - wj)^2) + 1 / W^2 * sum(wj2 / (W - wj))^2

  1 / df_inv

}

# Power calculation

power_t <- function(df, lambda, alpha, df_test = df, g = 1) {
  crit <- g * qt(1 - alpha / 2, df = df_test)
  pwr_lower <- pt(-crit, df = df, ncp = lambda, lower.tail = TRUE)
  pwr_upper <- pt(crit, df = df, ncp = lambda, lower.tail = FALSE)
  pwr <- pwr_lower + pwr_upper
  pwr
}


# Helper function described in supplementary material of Vembye, Pustejovsky, & Pigott (2022)

CHE_KL <- function(to, tau, omega, phi, rho, k_j, sigmasq_j) {

  trs_j <- to[1]^2 + rho * sigmasq_j
  ors_j <- to[2]^2 + (1 - rho) * sigmasq_j
  w_j <- k_j / (k_j * trs_j + ors_j)
  W <- sum(w_j)

  tausq_ps_j <- tau^2 + phi * sigmasq_j
  omegasq_ps_j <- omega^2 + (1 - phi) * sigmasq_j
  wj_star <- k_j / (k_j * tausq_ps_j + omegasq_ps_j)

  A1 <- sum((k_j - 1) * omegasq_ps_j / ors_j)
  A2 <- sum(w_j / wj_star)
  A3 <- sum(w_j^2 / wj_star) / W
  B <- sum((k_j - 1) * log(ors_j) - log(w_j / k_j))
  C <- log(W)

  A1 + A2 - A3 + B + C

}


find_tau_omega <- function(tau, omega, phi, rho, k_j, sigmasq_j) {

  res <- stats::optim(par = c(tau + 0.001, omega + 0.001), fn = CHE_KL,
                      tau = tau, omega = omega, phi = phi, rho = rho,
                      k_j = k_j, sigmasq_j = sigmasq_j,
                      lower = c(0,0), method = "L-BFGS-B")

  data.frame(tau_tilde = res$par[1], omega_tilde = res$par[2])
}


check_power <- function(J, tau2, omega2, rho,
                        model = "CHE",
                        var_df = "RVE",
                        alpha = .05,
                        target_power = .80,
                        sigma2_dist = NULL, n_ES_dist = NULL,
                        iterations = 100L,
                        seed = NULL) {

  mdes <- MDES_MADE(
    J = J,
    tau2 = tau2,
    omega2 = omega2,
    rho = rho,
    sigma2_dist = sigma2_dist,
    n_ES_dist = n_ES_dist,
    model = model,
    var_df = var_df,
    alpha = alpha,
    target_power = target_power,
    iterations = iterations,
    seed = seed
  ) %>%
    mutate(
      var_df = stringr::str_sub(stringr::str_extract(model, "-.+$"),2,-1),
      model = stringr::str_sub(stringr::str_extract(model, "^.+-"), 1, -2),
      var_df = recode(var_df, "Model+Satt" = "Satt")
    ) %>%
    select(J, mu = MDES, tau2, omega2, rho, d, alpha, iterations, model, var_df, target_power)

  power <-
    mdes |>
    select(-target_power) |>
    purrr::pmap_dfr(
    .f = power_MADE_engine,
    sigma2_dist = sigma2_dist,
    n_ES_dist = n_ES_dist,
    average_power = TRUE,
    seed = seed
  )

  mdes %>%
    select(-model, -var_df, -alpha, -iterations) %>%
    bind_cols(power)
}


# Manage dplyr behavior
utils::globalVariables(
  c("samp_method", "method", "vectorof", "var_b", "es",
    "res", "tau", "omega", "power", "Power", "label", "MDES", "pilot_dat",
    "sigma2_method", "mdes_data", "J_needed", "tau_name", "d", "mcse", "sd", ".")
)
