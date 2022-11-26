power_res <-
  power_MADE(
    J = seq(10,60,10),
    mu = 0.1,
    tau = c(0.1, 0.2, 0.3),
    omega = c(.05, 0.1,0.2),
    rho = c(0.2,0.5,0.7),
    sigma2_dist = 4 / 100,
    n_ES_dist = 5.5,
    model = "CHE",
    var_df = "RVE",
    alpha = 0.05,
    average_power = TRUE,
    warning = FALSE
  )

# basic grid layout
plot_MADE_engine(
  data = power_res,
  x = J,
  y = power,
  x_grid = omega,
  y_grid = tau,
  color = rho
)

# with grid labels
plot_MADE_engine(
  data = power_res,
  x = J,
  y = power,
  x_grid = omega,
  y_grid = tau,
  color = rho,
  shape = rho,
  grid_labs = TRUE,
  labs_size = 5
)

# with horizontal and vertical reference lines
plot_MADE_engine(
  data = power_res,
  x = J,
  y = power,
  x_grid = omega,
  y_grid = tau,
  shape = rho,
  linetype = rho,
  h_lines = 0.8,
  v_lines = c(20,40,60),
  grid_labs = TRUE
)

# with vertical shading
plot_MADE_engine(
  data = power_res,
  x = J,
  y = power,
  x_grid = omega,
  y_grid = tau,
  color = rho,
  shape = rho,
  linetype = rho,
  h_lines = 0.8,
  v_shade = c(20,50),
  grid_labs = TRUE
)

# fix the vertical axis limits
plot_MADE_engine(
  data = power_res,
  x = J,
  y = power,
  x_grid = omega,
  y_grid = tau,
  color = rho,
  shape = rho,
  linetype = rho,
  y_limits = c(0,1),
  y_breaks = seq(0,1,0.2),
  h_lines = 0.8,
  v_shade = c(20,50),
  grid_labs = TRUE
)

# modify the horizontal axis limits
plot_MADE_engine(
  data = power_res,
  x = J,
  y = power,
  x_grid = omega,
  y_grid = tau,
  color = rho,
  shape = rho,
  linetype = rho,
  x_limits = c(0,65),
  x_breaks = seq(0,60,20),
  y_limits = c(0,1),
  y_breaks = seq(0,1,0.2),
  h_lines = 0.8,
  v_shade = c(20,50),
  grid_labs = TRUE
)

