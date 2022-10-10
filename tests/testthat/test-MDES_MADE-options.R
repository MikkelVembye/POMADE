
pop_size <- 1000L
sigma2_emp <- rgamma(pop_size, shape = 4, rate = 12)
n_ES_emp <- 1 + stats::rpois(pop_size, 3.5 - 1)


test_that("MDES_MADE() works with single parameter values.", {

  mdes <- check_power(
    J = 40,
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

  expect_equal(nrow(mdes), 1L)
  expect_equal(mdes$target_power, mdes$power, tolerance = 0.001)

  mdes <- check_power(
    J = 40,
    tau2 = 0.2^2,
    omega2 = 0.1^2,
    rho = 0.7,
    sigma2_dist = 4 / 100,
    n_ES_dist = n_ES_emp,
    model = c("CHE","MLMA"),
    alpha = 0.05,
    target_power = 0.75,
    seed = 20221011
  )

  expect_equal(nrow(mdes), 2L)
  expect_equal(mdes$target_power, mdes$power, tolerance = 0.001)

  mdes <- check_power(
    J = 40,
    tau2 = 0.2^2,
    omega2 = 0.1^2,
    rho = 0.7,
    sigma2_dist = sigma2_emp,
    n_ES_dist = n_ES_emp,
    alpha = c(.01, 0.025, .1),
    iterations = 100L,
    seed = 20221012
  )

  expect_equal(nrow(mdes), 3L)
  expect_equal(mdes$target_power, mdes$power, tolerance = 0.005)

  mdes <- check_power(
    J = 40,
    tau2 = 0.2^2,
    omega2 = 0.1^2,
    rho = 0.7,
    sigma2_dist = \(x) rgamma(x, shape = 5, rate = 10),
    n_ES_dist = \(x) 1 + stats::rpois(x, 5.5 - 1),
    model = c("CHE", "MLMA", "CE"),
    var_df = c("Model", "Satt", "RVE"),
    alpha = c(.01, 0.025, .1),
    target_power = 0.45,
    iterations = 100,
    seed = 20221013
  )

  expect_equal(nrow(mdes), 21L)
  expect_equal(mdes$target_power, mdes$power, tolerance = 0.005)

})


test_that("power_MADE() works with multiple parameter values.", {

  skip_on_cran()

  mdes <- check_power(
    J = c(10,20,40),
    tau2 = 0.2^2,
    omega2 = 0.1^2,
    rho = 0.3,
    sigma2_dist = 4 / 100,
    n_ES_dist = 5.5,
    model = c("CHE","MLMA", "CE"),
    var_df = c("Model", "Satt", "RVE"),
    alpha = 0.05,
    target_power = c(0.4,0.72)
  )

  expect_equal(nrow(mdes), 6L * 7L)
  expect_equal(mdes$target_power, mdes$power, tolerance = 0.005)


  # constant MDES for balanced designs
  mdes %>%
    filter(!(model %in% c("MLMA-Model","MLMA-Model+Satt"))) %>%
    group_by(J, tau2, omega2, rho, target_power) %>%
    summarise(
      models = n(),
      MDES = diff(range(mu)),
      .groups = "drop"
    ) %>%
    summarise(
      MDES = max(MDES)
    ) %>%
    pull() %>%
    expect_lt(1e-5)

  mdes <- check_power(
    J = 40,
    tau2 = c(0.1, 0.2, 0.3)^2,
    omega2 = 0.1^2,
    rho = c(0.2,0.7),
    sigma2_dist = 4 / 100,
    n_ES_dist = n_ES_emp,
    model = c("CHE","MLMA"),
    alpha = 0.05,
    iterations = 5
  )

  expect_equal(nrow(mdes), 12L)
  expect_equal(mdes$target_power, mdes$power, tolerance = 0.005)

  mdes <- check_power(
    J = seq(10,30,10),
    tau2 = 0.2^2,
    omega2 = 0.1^2,
    rho = 0.7,
    sigma2_dist = sigma2_emp,
    n_ES_dist = n_ES_emp,
    alpha = c(.01, 0.025, .1),
    target_power = c(0.4,0.72),
    iterations = 150
  )

  expect_equal(nrow(mdes), 18L)
  expect_equal(mdes$target_power, mdes$power, tolerance = 0.005)

  mdes <- check_power(
    J = c(20,40),
    tau2 = c(0.1, 0.2, 0.3)^2,
    omega2 = c(0.1, 0.2)^2,
    rho = c(0.4,0.7,0.9),
    sigma2_dist = \(x) rgamma(x, shape = 5, rate = 10),
    n_ES_dist = \(x) 1 + stats::rpois(x, 5.5 - 1),
    model = c("CHE", "CE"),
    var_df = c("Model", "Satt", "RVE"),
    alpha = c(.01, 0.025, .1),
    iterations = 2
  )

  expect_equal(nrow(mdes), 2L * 3L * 2L * 3L * 4L * 3L)
})


test_that("MDES_MADE() returns 0 when target_power = alpha.", {

  skip_on_cran()

  res_RVE <-
    MDES_MADE(
      J = 20,
      tau2 = c(0.1, 0.2, 0.3)^2,
      omega2 = c(0.1, 0.2)^2,
      rho = 0.7,
      sigma2_dist = \(x) rgamma(x, shape = 5, rate = 10),
      n_ES_dist = \(x) 1 + stats::rpois(x, 5.5 - 1),
      model = c("CHE", "MLMA", "CE"),
      var_df = "RVE",
      alpha = c(.01, 0.05),
      target_power = 0.05 + 1e-5,
      iterations = 2
    )

  # MDES near zero when alpha ~= power
  res_RVE %>%
    filter(target_power - alpha < .001) %>%
    summarise(MDES = max(MDES)) %>%
    pull(MDES) %>%
    expect_lt(.01)

  # MDES greater than zero when alpha < power
  res_RVE %>%
    filter(target_power - alpha > .001) %>%
    summarise(MDES = min(MDES)) %>%
    pull(MDES) %>%
    expect_gt(.01)

  res_Satt <-
    MDES_MADE(
      J = c(20,40),
      tau2 = c(0.1, 0.2, 0.3)^2,
      omega2 = c(0.1, 0.2)^2,
      rho = c(0.4,0.7,0.9),
      sigma2_dist = \(x) rgamma(x, shape = 5, rate = 10),
      n_ES_dist = \(x) 1 + stats::rpois(x, 5.5 - 1),
      model = "CHE",
      var_df = "Satt",
      alpha = c(.1, .4),
      target_power = .4 + 1e-5,
      iterations = 3
    )

  # MDES near zero when alpha ~= power
  res_Satt %>%
    filter(target_power - alpha < .001) %>%
    summarise(MDES = max(MDES)) %>%
    pull(MDES) %>%
    expect_lt(.01)

  # MDES greater than zero when alpha < power
  res_Satt %>%
    filter(target_power - alpha > .001) %>%
    summarise(MDES = min(MDES)) %>%
    pull(MDES) %>%
    expect_gt(.01)

  res_balanced <-
    MDES_MADE(
      J = c(20,40),
      tau2 = c(0.1, 0.2, 0.3)^2,
      omega2 = c(0.1, 0.2)^2,
      rho = c(0.4,0.7,0.9),
      sigma2_dist = 4 / 48,
      n_ES_dist = 3,
      model = c("CHE","MLMA", "CE"),
      var_df = c("Model", "Satt", "RVE"),
      alpha = c(.1, .4),
      target_power = .4 + 1e-5
    )

  # MDES near zero when alpha ~= power
  res_balanced %>%
    filter(target_power - alpha < .001) %>%
    summarise(MDES = max(MDES)) %>%
    pull(MDES) %>%
    expect_lt(.01)

  # MDES greater than zero when alpha < power
  res_balanced %>%
    filter(target_power - alpha > .001) %>%
    summarise(MDES = min(MDES)) %>%
    pull(MDES) %>%
    expect_gt(.01)

  # constant MDES for balanced designs
  res_balanced %>%
    filter(!(model %in% c("MLMA-Model","MLMA-Model+Satt"))) %>%
    group_by(N_studies, tau2, omega2, rho, alpha) %>%
    summarise(
      models = n(),
      MDES = diff(range(MDES)),
      .groups = "drop"
    ) %>%
    summarise(
      MDES = max(MDES)
    ) %>%
    pull() %>%
    expect_lt(1e-5)

})
