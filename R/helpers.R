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
  names(pwr) <- paste0("power_sig", substr(formatC(alpha, digits = 2, format = "f"), 3, 4))
  do.call(data.frame, args = as.list(pwr))
}


# Sampling from pilot data function

n_ES_empirical <- function(dat, J) {
  dat[sample(NROW(dat), size = J, replace = TRUE),]
}


# Manage dplyr behavior
utils::globalVariables(c("samp_method", "method", "vectorof", "var_b", "es"))
