
pop_size <- 1000L
sigma2_emp <- rgamma(pop_size, shape = 5, rate = 12)
n_ES_emp <- 1 + rpois(pop_size, 4.5 - 1)

power_tol <- 0.001

test_that("find_J_MADE() works with single parameter values.", {

  expect_warning(
    J_min <- check_J(
      mu = 0.15,
      tau2 = 0.2^2,
      omega2 = 0.1^2,
      rho = 0.7,
      sigma2_dist = 4 / 100,
      n_ES_dist = 5.5,
      model = "CHE",
      var_df = "Satt",
      alpha = 0.05,
      target_power = 0.9,
      seed = 20221010
    )
  )

  expect_equal(nrow(J_min), 1L)
  expect_lt(max(J_min$less - J_min$target_power), power_tol)
  expect_gte(min(J_min$more - J_min$target_power), -power_tol)

  J_min <- check_J(
    mu = 0.3,
    tau2 = 0.2^2,
    omega2 = 0.1^2,
    rho = 0.7,
    sigma2_dist = 4 / 100,
    n_ES_dist = n_ES_emp,
    model = c("CHE","MLMA"),
    alpha = 0.05,
    target_power = 0.75,
    seed = 20221011,
    warning = FALSE
  )

  expect_equal(nrow(J_min), 2L)
  expect_lt(max(J_min$less - J_min$target_power), power_tol)
  expect_gte(min(J_min$more - J_min$target_power), -power_tol)

  J_min <- check_J(
    mu = 0.2,
    tau2 = 0.2^2,
    omega2 = 0.1^2,
    rho = 0.7,
    sigma2_dist = sigma2_emp,
    n_ES_dist = n_ES_emp,
    alpha = c(.01, 0.025, .1),
    iterations = 100L,
    seed = 20221013
  )

  expect_equal(nrow(J_min), 3L)
  expect_lt(max(J_min$less - J_min$target_power), power_tol)
  expect_gte(min(J_min$more - J_min$target_power), -power_tol)

  J_min <- check_J(
    mu = 0.1,
    tau2 = 0.2^2,
    omega2 = 0.1^2,
    rho = 0.7,
    sigma2_dist = \(x) rgamma(x, shape = 5, rate = 10),
    n_ES_dist = \(x) 1 + rpois(x, 5.5 - 1),
    model = c("CHE", "MLMA", "CE"),
    var_df = c("Model", "Satt", "RVE"),
    alpha = .1,
    target_power = 0.45,
    iterations = 50,
    seed = 20221014
  )

  expect_equal(nrow(J_min), 7L)
  expect_lt(max(J_min$less - J_min$target_power), power_tol)
  expect_gte(min(J_min$more - J_min$target_power), -power_tol)

})


test_that("find_J_MADE() works with multiple parameter values.", {

  skip_on_cran()

  expect_warning(
    J_min <- check_J(
      mu = c(0.17,0.22),
      tau2 = 0.2^2,
      omega2 = 0.1^2,
      rho = 0.3,
      sigma2_dist = 4 / 100,
      n_ES_dist = 5.5,
      model = c("CHE","MLMA", "CE"),
      var_df = c("Model", "Satt", "RVE"),
      alpha = 0.05,
      target_power = c(0.4,0.72),
      seed = 20221014
    )
  )

  expect_equal(nrow(J_min), 2L * 7L * 2L)
  expect_lt(max(J_min$less - J_min$target_power), power_tol)
  expect_gte(min(J_min$more - J_min$target_power), -power_tol)

  # constant MDES for balanced designs
  J_min %>%
    filter(!(model %in% c("MLMA-Model","MLMA-Model+Satt"))) %>%
    group_by(mu, tau2, omega2, rho, target_power) %>%
    summarise(
      models = n(),
      J_needed = diff(range(J_needed)),
      .groups = "drop"
    ) %>%
    summarise(
      J_needed = max(J_needed)
    ) %>%
    pull() %>%
    expect_lt(1e-5)

  J_min <- check_J(
    mu = 0.33,
    tau2 = c(0.1, 0.2, 0.4)^2,
    omega2 = 0.1^2,
    rho = c(0.2,0.7),
    sigma2_dist = 4 / 100,
    n_ES_dist = n_ES_emp,
    model = c("CHE","MLMA"),
    alpha = 0.05,
    iterations = 5,
    warning = FALSE,
    seed = 20221015
  )

  expect_equal(nrow(J_min), 3L * 2L * 2L)
  expect_lt(max(J_min$less - J_min$target_power), power_tol)
  expect_gte(min(J_min$more - J_min$target_power), -power_tol)

  J_min <- check_J(
    mu = c(0.05, 0.08),
    tau2 = 0.2^2,
    omega2 = 0.1^2,
    rho = 0.7,
    sigma2_dist = sigma2_emp,
    n_ES_dist = n_ES_emp,
    alpha = c(.025, .07, .10),
    target_power = c(0.40, 0.72),
    iterations = 10,
    seed = 20221016
  )

  expect_equal(nrow(J_min), 2L * 3L * 2L)
  expect_lt(max(J_min$less - J_min$target_power), power_tol)
  expect_gte(min(J_min$more - J_min$target_power), -power_tol)

  J_min <- check_J(
    mu = 0.4,
    tau2 = c(0.08, 0.16)^2,
    omega2 = c(0.1, 0.2)^2,
    rho = c(0.4,0.8),
    sigma2_dist = \(x) rgamma(x, shape = 5, rate = 10),
    n_ES_dist = \(x) 1 + rpois(x, 5.5 - 1),
    model = c("CHE", "CE"),
    var_df = c("Model", "Satt", "RVE"),
    alpha = c(.03, 0.7),
    iterations = 1L,
    seed = 20221017,
    warning = FALSE
  )

  expect_equal(nrow(J_min), 2L * 2L * 2L * 4L * 2L)

})


test_that("MDES_MADE() returns minimum J when target_power = alpha.", {

  skip_on_cran()

  res_RVE <-
    find_J_MADE(
      mu = 0.3,
      tau2 = c(0.1, 0.3)^2,
      omega2 = c(0.1, 0.2)^2,
      rho = 0.7,
      sigma2_dist = \(x) rgamma(x, shape = 5, rate = 10),
      n_ES_dist = \(x) 1 + rpois(x, 5.5 - 1),
      model = c("CHE", "MLMA", "CE"),
      var_df = "RVE",
      alpha = c(.01, 0.05),
      target_power = 0.05 + 1e-5,
      iterations = 4,
      seed = 20221018
    )

  # J at minimum when alpha ~= power
  res_RVE %>%
    filter(target_power - alpha < .001) %>%
    summarise(J_needed = max(J_needed)) %>%
    pull(J_needed) %>%
    expect_equal(5L)

  # J greater than minimum when alpha < power
  res_RVE %>%
    filter(target_power - alpha > .001) %>%
    summarise(J_needed = min(J_needed)) %>%
    pull(J_needed) %>%
    expect_gte(5L)

  res_Satt <-
    find_J_MADE(
      mu = 0.2,
      tau2 = c(0.1, 0.3)^2,
      omega2 = c(0.1, 0.2)^2,
      rho = c(0.4,0.7,0.9),
      sigma2_dist = \(x) rgamma(x, shape = 5, rate = 10),
      n_ES_dist = \(x) 1 + rpois(x, 5.5 - 1),
      model = "CHE",
      var_df = "Satt",
      alpha = c(.1, .3),
      target_power = .3 + 1e-5,
      iterations = 3,
      seed = 20221019,
      interval = c(7, 100)
    )

  # J at minimum when alpha ~= power
  res_Satt %>%
    filter(target_power - alpha < .001) %>%
    summarise(J_needed = max(J_needed)) %>%
    pull(J_needed) %>%
    expect_equal(7L)

  # J greater than minimum when alpha < power
  res_Satt %>%
    filter(target_power - alpha > .001) %>%
    summarise(J_needed = min(J_needed)) %>%
    pull(J_needed) %>%
    expect_gte(7L)

  res_balanced <-
    find_J_MADE(
      mu = c(0.05, 0.13),
      tau2 = c(0.1, 0.3)^2,
      omega2 = c(0.1, 0.2)^2,
      rho = c(0.4,0.9),
      sigma2_dist = 4 / 48,
      n_ES_dist = 3,
      model = c("CHE","MLMA", "CE"),
      var_df = c("Model", "Satt", "RVE"),
      alpha = c(.1, .4),
      target_power = .4 + 1e-5,
      warning = FALSE,
      interval = c(6, 100)
    )

  # J at minimum when alpha ~= power
  res_balanced %>%
    filter(target_power - alpha < .001) %>%
    summarise(J_needed = max(J_needed)) %>%
    pull(J_needed) %>%
    expect_equal(6L)

  # J greater than minimum when alpha < power
  res_balanced %>%
    filter(target_power - alpha > .001) %>%
    summarise(J_needed = min(J_needed)) %>%
    pull(J_needed) %>%
    expect_gte(6L)


  # constant MDES for balanced designs
  res_balanced %>%
    filter(!(model %in% c("MLMA-Model","MLMA-Model+Satt"))) %>%
    group_by(mu, tau2, omega2, rho, alpha) %>%
    summarise(
      models = n(),
      J_needed = diff(range(J_needed)),
      .groups = "drop"
    ) %>%
    summarise(
      J_needed = max(J_needed)
    ) %>%
    pull() %>%
    expect_lt(1e-5)

})
