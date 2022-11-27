
test_that("plot_MADE.power() returns multiple plots.", {

  power_res <-
    power_MADE(
      J = seq(10,40,10),
      mu = c(0.1,0.2,0.3),
      tau = c(0.1, 0.2),
      omega = c(.05, 0.1),
      rho = c(0.2,0.7),
      sigma2_dist = 4 / 100,
      n_ES_dist = 5.5,
      model = "CHE",
      var_df = c("RVE","Satt"),
      alpha = 0.05,
      average_power = TRUE,
      warning = FALSE
    )

  expect_warning(p_all <- plot_MADE(power_res))
  expect_identical(length(p_all), 6L)

  p_single <-
    subset(power_res, model == "CHE-RVE") |>
    plot_MADE()

  expect_identical(length(p_single), 3L)

  tlp <- plot_MADE(
    power_res, warning = FALSE,
    traffic_light_assumptions = c("unlikely", "likely", "expected", "likely")
  )

  expect_identical(length(tlp), 6L)

})
