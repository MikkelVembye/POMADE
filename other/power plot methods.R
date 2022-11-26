power_res <-
  power_MADE(
    J = seq(10,60,10),
    mu = c(0.1,0.2),
    tau = c(0.1, 0.2, 0.3),
    omega = c(.05, 0.1,0.2),
    rho = c(0.2,0.5,0.7),
    sigma2_dist = 4 / 100,
    n_ES_dist = 5.5,
    model = "CHE",
    var_df = c("RVE","Satt"),
    alpha = 0.05,
    average_power = TRUE,
    warning = FALSE
  )

debugonce(plot_MADE.power)
# debugonce(plot_MADE_engine)

r <- plot_MADE(power_res, warning = FALSE)
r
