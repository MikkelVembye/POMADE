
pop_size <- 1000L
sigma2_emp <- rgamma(pop_size, shape = 5, rate = 10)
n_ES_emp <- 1 + stats::rpois(pop_size, 5.5 - 1)

test_that("power_MADE() works with single parameter values, averaged power.", {

  power_MADE(
    J = 40,
    tau2 = 0.2^2,
    omega2 = 0.1^2,
    beta = 0.1,
    rho = 0.7,
    sigma2_dist = 4 / 100,
    n_ES_dist = 5.5,
    model = "CHE",
    var_df = "Satt",
    alpha = 0.05,
    average_power = TRUE
  ) %>%
    nrow() %>%
    expect_equal(1L)

  power_MADE(
    J = 40,
    tau2 = 0.2^2,
    omega2 = 0.1^2,
    beta = 0.1,
    rho = 0.7,
    sigma2_dist = 4 / 100,
    n_ES_dist = n_ES_emp,
    model = c("CHE","MLMA"),
    alpha = 0.05,
    average_power = TRUE
  ) %>%
    nrow() %>%
    expect_equal(2L)

  power_MADE(
    J = 40,
    tau2 = 0.2^2,
    omega2 = 0.1^2,
    beta = 0.1,
    rho = 0.7,
    sigma2_dist = sigma2_emp,
    n_ES_dist = n_ES_emp,
    alpha = c(.01, 0.025, .1),
    average_power = TRUE,
    iterations = 4
  ) %>%
    nrow() %>%
    expect_equal(3L)

  power_MADE(
    J = 40,
    tau2 = 0.2^2,
    omega2 = 0.1^2,
    beta = 0.1,
    rho = 0.7,
    sigma2_dist = \(x) rgamma(x, shape = 5, rate = 10),
    n_ES_dist = \(x) 1 + stats::rpois(x, 5.5 - 1),
    model = c("CHE", "MLMA", "CE"),
    var_df = c("Model", "Satt", "RVE"),
    alpha = c(.01, 0.025, .1),
    average_power = TRUE,
    iterations = 7
  ) %>%
    nrow() %>%
    expect_equal(21L)

})

test_that("power_MADE() works with single parameter values, raw power.", {

  power_MADE(
    J = 40,
    tau2 = 0.2^2,
    omega2 = 0.1^2,
    beta = 0.1,
    rho = 0.7,
    sigma2_dist = 4 / 100,
    n_ES_dist = 5.5,
    model = "CHE",
    var_df = "Satt",
    alpha = 0.05,
    average_power = FALSE
  ) %>%
    nrow() %>%
    expect_equal(1L)

  power_MADE(
    J = 40,
    tau2 = 0.2^2,
    omega2 = 0.1^2,
    beta = 0.1,
    rho = 0.7,
    sigma2_dist = 4 / 100,
    n_ES_dist = n_ES_emp,
    model = c("CHE","MLMA"),
    alpha = 0.05,
    average_power = FALSE,
    iterations = 10
  ) %>%
    nrow() %>%
    expect_equal(20L)

  power_MADE(
    J = 40,
    tau2 = 0.2^2,
    omega2 = 0.1^2,
    beta = 0.1,
    rho = 0.7,
    sigma2_dist = sigma2_emp,
    n_ES_dist = n_ES_emp,
    alpha = c(.01, 0.025, .1),
    average_power = FALSE,
    iterations = 4
  ) %>%
    nrow() %>%
    expect_equal(12L)

  power_MADE(
    J = 40,
    tau2 = 0.2^2,
    omega2 = 0.1^2,
    beta = 0.1,
    rho = 0.7,
    sigma2_dist = \(x) rgamma(x, shape = 5, rate = 10),
    n_ES_dist = \(x) 1 + stats::rpois(x, 5.5 - 1),
    model = c("CHE", "MLMA", "CE"),
    var_df = c("Model", "Satt", "RVE"),
    alpha = c(.01, 0.025, .1),
    average_power = FALSE,
    iterations = 7
  ) %>%
    nrow() %>%
    expect_equal(21L * 7L)

})


test_that("power_MADE() works with multiple parameter values, averaged power.", {

  power_MADE(
    J = c(10,20,40),
    tau2 = 0.2^2,
    omega2 = 0.1^2,
    beta = 0.1,
    rho = 0.3,
    sigma2_dist = 4 / 100,
    n_ES_dist = 5.5,
    model = "CHE",
    var_df = "Satt",
    alpha = 0.05,
    average_power = TRUE
  ) %>%
    nrow() %>%
    expect_equal(3L)

  power_MADE(
    J = 40,
    tau2 = c(0.1, 0.2, 0.3)^2,
    omega2 = 0.1^2,
    beta = 0.1,
    rho = c(0.2,0.7),
    sigma2_dist = 4 / 100,
    n_ES_dist = n_ES_emp,
    model = c("CHE","MLMA"),
    alpha = 0.05,
    average_power = TRUE,
    iterations = 5
  ) %>%
    nrow() %>%
    expect_equal(12L)

  power_MADE(
    J = seq(10,30,10),
    tau2 = 0.2^2,
    omega2 = 0.1^2,
    beta = seq(0.1,0.6,0.1),
    rho = 0.7,
    sigma2_dist = sigma2_emp,
    n_ES_dist = n_ES_emp,
    alpha = c(.01, 0.025, .1),
    average_power = TRUE,
    iterations = 4
  ) %>%
    nrow() %>%
    expect_equal(3L * 3L * 6L)

  power_MADE(
    J = c(20,40),
    tau2 = c(0.1, 0.2, 0.3)^2,
    omega2 = c(0.1, 0.2)^2,
    beta = seq(0.0,0.8,0.2),
    rho = c(0.4,0.7,0.9),
    sigma2_dist = \(x) rgamma(x, shape = 5, rate = 10),
    n_ES_dist = \(x) 1 + stats::rpois(x, 5.5 - 1),
    model = c("CHE", "CE"),
    var_df = c("Model", "Satt", "RVE"),
    alpha = c(.01, 0.025, .1),
    average_power = TRUE,
    iterations = 2
  ) %>%
    nrow() %>%
    expect_equal(2L * 3L * 2L * 5L * 3L * 4L * 3L)

})

test_that("power_MADE() works with multiple parameter values, raw power.", {

  power_MADE(
    J = c(10,20,40),
    tau2 = 0.2^2,
    omega2 = 0.1^2,
    beta = 0.1,
    rho = 0.3,
    sigma2_dist = 4 / 100,
    n_ES_dist = 5.5,
    model = "CHE",
    var_df = "Satt",
    alpha = 0.05,
    average_power = FALSE
  ) %>%
    nrow() %>%
    expect_equal(3L)

  power_MADE(
    J = 40,
    tau2 = c(0.1, 0.2, 0.3)^2,
    omega2 = 0.1^2,
    beta = 0.1,
    rho = c(0.2,0.7),
    sigma2_dist = 4 / 100,
    n_ES_dist = n_ES_emp,
    model = c("CHE","MLMA"),
    alpha = 0.05,
    average_power = FALSE
  ) %>%
    nrow() %>%
    expect_equal(12L * 100L)

  power_MADE(
    J = seq(10,30,10),
    tau2 = 0.2^2,
    omega2 = 0.1^2,
    beta = seq(0.1,0.6,0.1),
    rho = 0.7,
    sigma2_dist = sigma2_emp,
    n_ES_dist = n_ES_emp,
    alpha = c(.01, 0.025, .1),
    average_power = FALSE,
    iterations = 4
  ) %>%
    nrow() %>%
    expect_equal(3L * 3L * 6L * 4L)

  power_MADE(
    J = c(20,40),
    tau2 = c(0.1, 0.2, 0.3)^2,
    omega2 = c(0.1, 0.2)^2,
    beta = seq(0.0,0.8,0.2),
    rho = c(0.4,0.7,0.9),
    sigma2_dist = \(x) rgamma(x, shape = 5, rate = 10),
    n_ES_dist = \(x) 1 + stats::rpois(x, 5.5 - 1),
    model = c("CHE", "CE"),
    var_df = c("Model", "Satt", "RVE"),
    alpha = c(.01, 0.025, .1),
    average_power = FALSE,
    iterations = 3
  ) %>%
    nrow() %>%
    expect_equal(2L * 3L * 2L * 5L * 3L * 4L * 3L * 3L)

})


test_that("power_MADE() returns alpha when null is true.", {

  res_RVE <-
    power_MADE(
      J = c(20,40),
      tau2 = c(0.1, 0.2, 0.3)^2,
      omega2 = c(0.1, 0.2)^2,
      beta = 0,
      rho = c(0.4,0.7,0.9),
      sigma2_dist = \(x) rgamma(x, shape = 5, rate = 10),
      n_ES_dist = \(x) 1 + stats::rpois(x, 5.5 - 1),
      model = c("CHE", "MLMA","CE"),
      var_df = "RVE",
      alpha = seq(.01, 0.4, length.out = 10),
      average_power = FALSE,
      iterations = 3
    )

  expect_equal(res_RVE$alpha, res_RVE$power)

  res_Satt <-
    power_MADE(
      J = c(20,40),
      tau2 = c(0.1, 0.2, 0.3)^2,
      omega2 = c(0.1, 0.2)^2,
      beta = 0,
      rho = c(0.4,0.7,0.9),
      sigma2_dist = \(x) rgamma(x, shape = 5, rate = 10),
      n_ES_dist = \(x) 1 + stats::rpois(x, 5.5 - 1),
      model = "CHE",
      var_df = "Satt",
      alpha = seq(.01, 0.4, length.out = 10),
      average_power = FALSE,
      iterations = 3
    )

  expect_equal(res_Satt$alpha, res_Satt$power)

  res_balanced <-
    power_MADE(
      J = c(20,40),
      tau2 = c(0.1, 0.2, 0.3)^2,
      omega2 = c(0.1, 0.2)^2,
      beta = 0.0,
      rho = c(0.4,0.7,0.9),
      sigma2_dist = 4 / 48,
      n_ES_dist = 3,
      model = c("CHE","MLMA", "CE"),
      var_df = c("Model", "Satt", "RVE"),
      alpha = seq(0.04, 0.40, length.out = 9),
      average_power = FALSE,
      iterations = 3
    )

  expect_equal(res_balanced$alpha, res_balanced$power, tolerance = 5e-5)

})

test_that("power_MADE() returns constant power for balanced designs.", {

  res <-
    power_MADE(
      J = c(20,40),
      tau2 = c(0.1, 0.2, 0.3)^2,
      omega2 = c(0.1, 0.2)^2,
      beta = 0.07,
      rho = c(0.4,0.7,0.9),
      sigma2_dist = 4 / 48,
      n_ES_dist = 2,
      model = c("CHE", "MLMA","CE"),
      var_df = c("Model","Satt","RVE"),
      alpha = seq(.01, 0.4, length.out = 10),
      average_power = FALSE,
    )

  res_comparison <-
    res %>%
    group_by(J, tau2, omega2, beta, rho, alpha) %>%
    summarise(
      models = n(),
      across(c(power,var_b, df), ~ diff(range(.x)))
    )

  expect_lt(max(res_comparison$power), 1e-5)
  expect_lt(max(res_comparison$var_b), 1e-5)
  expect_lt(max(res_comparison$df), 1e-5)

})
