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

#' @importFrom utils globalVariables

# Manage dplyr behavior
utils::globalVariables(
  c("samp_method", "method", "vectorof", "var_b", "es",
    "res", "tau", "omega", "power", "Power", "label", "MDES", "pilot_dat",
    "sigma2_method", "mdes_data", "studies_needed", "tau_name", "d", "mcse", "sd", ".",
    "J", "plan", "samp_method_sigma2", "sequential", "x", "rho", "alpha", "target_power",
    "model", "cap", "y_lab", "mu", "cor", "qnorm", "lower"
    )
)









