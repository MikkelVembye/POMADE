
pop_size <- 1000L
sigma2_emp <- rgamma(pop_size, shape = 4, rate = 12)
n_ES_emp <- 1 + stats::rpois(pop_size, 3.5 - 1)

test_that("power_MADE() works with future parallelization.", {

  skip_on_cran()

  power1 <- check_with_future(
    f = power_MADE,
    J = c(10,20,40),
    tau2 = 0.2^2,
    omega2 = 0.1^2,
    mu = 0.1,
    rho = 0.3,
    sigma2_dist = 4 / 100,
    n_ES_dist = 5.5,
    model = "CHE",
    var_df = "Satt",
    alpha = 0.05,
    average_power = TRUE,
    warning = FALSE
  )

  expect_identical(power1$res_seq, power1$res_par)

  power2 <- check_with_future(
    f = power_MADE,
    J = 40,
    tau2 = c(0.1, 0.2, 0.3)^2,
    omega2 = 0.1^2,
    mu = 0.1,
    rho = c(0.2,0.7),
    sigma2_dist = 4 / 100,
    n_ES_dist = n_ES_emp,
    model = c("CHE","MLMA"),
    alpha = 0.05,
    average_power = TRUE,
    iterations = 5,
    warning = FALSE,
    seed = 20221011
  )

  expect_identical(power2$res_seq, power2$res_par)

  power3 <- check_with_future(
    f = power_MADE,
    J = seq(10,30,10),
    tau2 = 0.2^2,
    omega2 = 0.1^2,
    mu = seq(0.1,0.6,0.1),
    rho = 0.7,
    sigma2_dist = sigma2_emp,
    n_ES_dist = n_ES_emp,
    alpha = c(.01, 0.025, .1),
    average_power = TRUE,
    iterations = 4,
    seed = 20221012
  )

  expect_identical(power3$res_seq, power3$res_par)

  power4 <- check_with_future(
    f = power_MADE,
    J = c(20,40),
    tau2 = c(0.1, 0.2, 0.3)^2,
    omega2 = c(0.1, 0.2)^2,
    mu = seq(0.0,0.8,0.2),
    rho = c(0.4,0.7,0.9),
    sigma2_dist = \(x) rgamma(x, shape = 5, rate = 10),
    n_ES_dist = \(x) 1 + stats::rpois(x, 5.5 - 1),
    model = c("CHE", "CE"),
    var_df = c("Model", "Satt", "RVE"),
    alpha = c(.01, 0.025, .1),
    average_power = TRUE,
    iterations = 2,
    seed = 20221013
  )

  expect_identical(power4$res_seq, power4$res_par)

  power5 <- check_with_future(
    f = power_MADE,
    J = c(20,40),
    tau2 = c(0.1, 0.2, 0.3)^2,
    omega2 = c(0.1, 0.2)^2,
    mu = seq(0.0,0.8,0.2),
    rho = c(0.4,0.7,0.9),
    sigma2_dist = \(x) rgamma(x, shape = 5, rate = 10),
    n_ES_dist = \(x) 1 + stats::rpois(x, 5.5 - 1),
    model = c("CHE", "CE"),
    var_df = c("Model", "Satt", "RVE"),
    alpha = c(.01, 0.025, .1),
    average_power = TRUE,
    iterations = 2
  )

  expect_false(identical(power5$res_seq, power5$res_par))

})

test_that("MDES_MADE() works with future parallelization.", {

  skip_on_cran()

  mdes1 <- check_with_future(
    f = MDES_MADE,
    J = c(10,20,40),
    tau2 = 0.2^2,
    omega2 = 0.1^2,
    rho = 0.3,
    sigma2_dist = 4 / 100,
    n_ES_dist = 5.5,
    model = c("CHE","MLMA", "CE"),
    var_df = c("Model", "Satt", "RVE"),
    alpha = 0.05,
    target_power = c(0.4,0.72),
    seed = 20221014,
    warning = FALSE,
    workers = 2L
  )

  expect_identical(mdes1$res_seq, mdes1$res_par)
  expect_gt(mdes1$tm_seq, mdes1$tm_par)

  mdes2 <- check_with_future(
    f = MDES_MADE,
    J = 40,
    tau2 = c(0.1, 0.2, 0.3)^2,
    omega2 = 0.1^2,
    rho = c(0.2,0.7),
    sigma2_dist = 4 / 100,
    n_ES_dist = n_ES_emp,
    model = c("CHE","MLMA"),
    alpha = 0.05,
    iterations = 5,
    warning = FALSE,
    seed = 20221015,
    workers = 2L
  )

  expect_identical(mdes2$res_seq, mdes2$res_par)
  expect_gt(mdes2$tm_seq, mdes2$tm_par)

  mdes3 <- check_with_future(
    f = MDES_MADE,
    J = seq(10,30,10),
    tau2 = 0.2^2,
    omega2 = 0.1^2,
    rho = 0.7,
    sigma2_dist = sigma2_emp,
    n_ES_dist = n_ES_emp,
    alpha = c(.01, 0.025, .1),
    target_power = c(0.4,0.72),
    iterations = 150,
    seed = 20221016,
    workers = 2L
  )

  expect_identical(mdes3$res_seq, mdes3$res_par)
  expect_gt(mdes3$tm_seq, mdes3$tm_par)

  mdes4 <- check_with_future(
    f = MDES_MADE,
    J = c(20,40),
    tau2 = c(0.1, 0.2, 0.3)^2,
    omega2 = c(0.1, 0.2)^2,
    rho = c(0.4,0.7,0.9),
    sigma2_dist = \(x) rgamma(x, shape = 5, rate = 10),
    n_ES_dist = \(x) 1 + stats::rpois(x, 5.5 - 1),
    model = c("CHE", "CE"),
    var_df = c("Model", "Satt", "RVE"),
    alpha = c(.01, 0.025, .1),
    iterations = 2,
    warning = FALSE,
    seed = 20221017,
    workers = 2L
  )

  expect_identical(mdes4$res_seq, mdes4$res_par)
  expect_gt(mdes4$tm_seq, mdes4$tm_par)

  mdes5 <- check_with_future(
    f = MDES_MADE,
    J = c(20,40),
    tau2 = c(0.1, 0.6)^2,
    omega2 = 0.1^2,
    rho = 0.5,
    sigma2_dist = \(x) rgamma(x, shape = 5, rate = 10),
    n_ES_dist = \(x) 1 + stats::rpois(x, 5.5 - 1),
    model = c("CHE","CE"),
    var_df = c("Satt", "RVE"),
    alpha = c(.03),
    iterations = 4,
    warning = FALSE,
    workers = 2L
  )

  expect_false(identical(mdes5$res_seq, mdes5$res_par))
  expect_gt(mdes5$tm_seq, mdes5$tm_par)

})

test_that("find_J_MADE() works with future parallelization.", {

  skip_on_cran()

  J1 <- check_with_future(
    f = find_J_MADE,
    mu = c(0.1,0.2),
    tau2 = 0.2^2,
    omega2 = 0.1^2,
    rho = 0.3,
    sigma2_dist = 4 / 100,
    n_ES_dist = 5.5,
    model = c("CHE","MLMA"),
    var_df = c("Satt", "RVE"),
    alpha = 0.05,
    target_power = 0.6,
    seed = 20221014,
    warning = FALSE,
    workers = 2L
  )

  expect_identical(J1$res_seq, J1$res_par)
  expect_gt(J1$tm_seq, J1$tm_par)

  J2 <- check_with_future(
    f = find_J_MADE,
    mu = 0.36,
    tau2 = 0.2^2,
    omega2 = 0.1^2,
    rho = 0.7,
    sigma2_dist = sigma2_emp,
    n_ES_dist = n_ES_emp,
    alpha = c(.01, .1),
    target_power = c(0.4,0.72),
    iterations = 10,
    seed = 20221016,
    workers = 2L
  )

  expect_identical(J2$res_seq, J2$res_par)
  expect_gt(J2$tm_seq, J2$tm_par)

  J3 <- check_with_future(
    f = find_J_MADE,
    mu = c(0.2, 0.4),
    tau2 = c(0.05, 0.09)^2,
    omega2 = 0.05^2,
    rho = 0.5,
    sigma2_dist = \(x) rgamma(x, shape = 5, rate = 10),
    n_ES_dist = \(x) 1 + stats::rpois(x, 5.5 - 1),
    model = c("CHE","CE"),
    var_df = c("Model", "RVE"),
    alpha = .03,
    iterations = 3,
    warning = FALSE,
    workers = 2L
  )

  expect_false(identical(J3$res_seq, J3$res_par))
  expect_gt(J3$tm_seq, J3$tm_par)

})
