
test_that("plot_MADE.power() returns one or multiple plots.", {

  res <-
    power_MADE(
      J = seq(10,40,10),
      mu = c(0.1,0.2,0.3),
      tau = c(0.1, 0.2),
      omega = c(.05, 0.1),
      rho = c(0.2,0.7,0.9),
      sigma2_dist = 4 / 100,
      n_ES_dist = 5.5,
      model = "CHE",
      var_df = c("RVE","Satt"),
      alpha = 0.05,
      average_power = TRUE,
      warning = FALSE
    )

  res_tibble <- as_tibble(res)
  expect_error(plot_MADE(res_tibble))

  expect_warning(p_all <- plot_MADE(res))
  expect_identical(length(p_all), 6L)
  expect_s3_class(p_all[[1]], "ggplot")

  p_model <- plot_MADE(res, warning = FALSE, model_comparison = TRUE)
  expect_identical(length(p_model), 9L)
  expect_s3_class(p_model[[1]], "ggplot")

  p_subset <-
    subset(res, model == "CHE-RVE") |>
    plot_MADE()

  expect_identical(length(p_subset), 3L)
  expect_s3_class(p_subset[[1]], "ggplot")

  p_single <-
    subset(res, model == "CHE-RVE" & mu == 0.1) |>
    plot_MADE()

  expect_s3_class(p_single, "ggplot")

  p_single_model <-
    subset(res, mu == 0.1 & rho == 0.7) |>
    plot_MADE(model_comparison = TRUE)

  expect_s3_class(p_single_model, "ggplot")

  tlp <- plot_MADE(
    res, warning = FALSE,
    traffic_light_assumptions = c("unlikely", "likely", "expected", "likely")
  )

  expect_identical(length(tlp), 6L)
  for (i in seq_along(tlp)) expect_s3_class(tlp[[i]], "trafficlightplot")

  tlp_model <- plot_MADE(
    res, model_comparison = TRUE,
    traffic_light_assumptions = c("unlikely", "likely", "expected", "likely")
  )

  expect_identical(length(tlp_model), 9L)
  for (i in seq_along(tlp_model)) expect_s3_class(tlp_model[[i]], "trafficlightplot")

})


test_that("plot_MADE.mdes() returns one or multiple plots.", {

  res <-
    mdes_MADE(
      J = seq(10,40,10),
      tau = c(0.1, 0.2),
      omega = c(.05, 0.1),
      rho = c(0.2,0.7),
      target_power = c(0.8,0.9),
      sigma2_dist = 4 / 100,
      n_ES_dist = 5.5,
      model = "CHE",
      var_df = c("RVE","Satt"),
      alpha = 0.05,
      warning = FALSE
    )

  res_tibble <- as_tibble(res)
  expect_error(plot_MADE(res_tibble))


  expect_warning(p_all <- plot_MADE(res))
  expect_identical(length(p_all), 4L)
  expect_s3_class(p_all[[1]], "ggplot")

  p_subset <-
    subset(res, model == "CHE-RVE") |>
    plot_MADE()

  expect_identical(length(p_subset), 2L)
  expect_s3_class(p_subset[[1]], "ggplot")

  p_single <-
    subset(res, model == "CHE-RVE" & target_power == 0.9) |>
    plot_MADE()

  expect_s3_class(p_single, "ggplot")

  tlp <- plot_MADE(
    res, warning = FALSE,
    traffic_light_assumptions = c("unlikely", "likely", "expected", "likely")
  )

  expect_identical(length(tlp), 4L)
  for (i in seq_along(tlp)) expect_s3_class(tlp[[i]], "trafficlightplot")


})

test_that("plot_MADE.min_studies() returns one or multiple plots.", {

  res <-
    min_studies_MADE(
      mu = seq(0.15,0.45,0.10),
      tau = c(0.1, 0.2),
      omega = c(.05, 0.1),
      rho = 0.9,
      target_power = c(0.8,0.9),
      sigma2_dist = 4 / 100,
      n_ES_dist = 5.5,
      model = "CHE",
      var_df = c("RVE","Satt"),
      alpha = 0.05,
      warning = FALSE
    )

  res_tibble <- as_tibble(res)
  expect_error(plot_MADE(res_tibble))

  expect_warning(p_all <- plot_MADE(res))
  expect_identical(length(p_all), 4L)
  expect_s3_class(p_all[[1]], "ggplot")

  p_subset <-
    subset(res, model == "CHE-RVE") |>
    plot_MADE()

  expect_identical(length(p_subset), 2L)
  expect_s3_class(p_subset[[1]], "ggplot")

  p_omega <-
    subset(res, mu == 0.25) |>
    plot_MADE(warning = FALSE)

  expect_identical(length(p_omega), 4L)
  expect_s3_class(p_omega[[1]], "ggplot")

  p_single <-
    subset(res, model == "CHE-RVE" & target_power == 0.9) |>
    plot_MADE()

  expect_s3_class(p_single, "ggplot")

  tlp <- plot_MADE(
    res, warning = FALSE,
    traffic_light_assumptions = c("unlikely", "likely", "expected", "likely")
  )

  expect_identical(length(tlp), 4L)
  for (i in seq_along(tlp)) expect_s3_class(tlp[[i]], "trafficlightplot")

  tlp_omega <-
    subset(res, mu == 0.25) |>
    plot_MADE(
    warning = FALSE,
    traffic_light_assumptions = c("unlikely", "likely")
  )

  expect_identical(length(tlp_omega), 4L)
  for (i in seq_along(tlp_omega)) expect_s3_class(tlp_omega[[i]], "trafficlightplot")


})
