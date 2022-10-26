
check_power <- function(J, tau, omega, rho,
                        model = "CHE",
                        var_df = "RVE",
                        alpha = .05,
                        target_power = .80,
                        sigma2_dist = NULL, n_ES_dist = NULL,
                        iterations = 100L,
                        warning = TRUE,
                        seed = NULL) {

  mdes <- mdes_MADE(
    J = J,
    tau = tau,
    omega = omega,
    rho = rho,
    sigma2_dist = sigma2_dist,
    n_ES_dist = n_ES_dist,
    model = model,
    var_df = var_df,
    alpha = alpha,
    target_power = target_power,
    iterations = iterations,
    warning = warning,
    seed = seed
  ) %>%
    mutate(
      var_df = stringr::str_sub(stringr::str_extract(model, "-.+$"),2,-1),
      model = stringr::str_sub(stringr::str_extract(model, "^.+-"), 1, -2),
      var_df = recode(var_df, "Model+Satt" = "Satt")
    ) %>%
    select(J, mu = MDES, tau, omega, rho, d, alpha, iterations, model, var_df, target_power)

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

check_J <- function(mu, tau, omega, rho,
                    model = "CHE",
                    var_df = "RVE",
                    alpha = .05,
                    target_power = .80,
                    sigma2_dist = NULL, n_ES_dist = NULL,
                    iterations = 100L,
                    warning = TRUE,
                    seed = NULL) {

  J_min <- min_studies_MADE(
    mu = mu,
    tau = tau,
    omega = omega,
    rho = rho,
    sigma2_dist = sigma2_dist,
    n_ES_dist = n_ES_dist,
    model = model,
    var_df = var_df,
    alpha = alpha,
    target_power = target_power,
    iterations = iterations,
    warning = warning,
    seed = seed
  ) %>%
    mutate(
      var_df = stringr::str_sub(stringr::str_extract(model, "-.+$"),2,-1),
      model = stringr::str_sub(stringr::str_extract(model, "^.+-"), 1, -2),
      var_df = recode(var_df, "Model+Satt" = "Satt")
    ) %>%
    select(studies_needed, mu, tau, omega, rho, d, alpha, iterations, model, var_df, target_power)

  J_min_pm <-
    bind_rows(
      mutate(J_min, J = studies_needed - 1, x = "less"),
      mutate(J_min, J = studies_needed, x = "more")
    )

  J_min_pm |>
    select(-target_power, -x, -studies_needed) |>
    purrr::pmap_dfr(
      .f = power_MADE_engine,
      sigma2_dist = sigma2_dist,
      n_ES_dist = n_ES_dist,
      average_power = TRUE,
      seed = seed
    ) |>
    select(samp_method_sigma2, samp_method_sigma2, power) |>
    bind_cols(J_min_pm) |>
    select(-J) |>
    tidyr::pivot_wider(names_from = x, values_from = power)

}

check_with_future <- function(f, ..., workers = future::availableCores(), check_time = FALSE) {

  future::plan(future::sequential)

  tm_seq <- system.time(
    res_seq <- f(...)
  )

  future::plan(future::multisession, workers = workers)

  tm_par <- system.time(
    res_par <- f(...)
  )

  future::plan(future::sequential)

  list(tm_seq = tm_seq[["elapsed"]], tm_par = tm_par[["elapsed"]], res_seq = res_seq, res_par = res_par)
}

