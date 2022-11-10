# Building and testing

#library(usethis)
#library(devtools)
library(dplyr)
library(stringr)
#library(furrr)
#library(tictoc)

options(pillar.sigfig = 4) # ensure tibble include 4 digits
options(tibble.width = Inf)
options(dplyr.print_min = 310)
options(scipen = 10)
options(dplyr.summarise.inform = FALSE)

#Sys.setenv(LANG = "en")

# Creates new coding script
#use_r("power_MADE")
#use_r("MDES_MADE")
#use_r("find_J_MADE")

#load_all()

#install()
#
#library(POMADE)
#
#?POMADE::power_MADE()

#############
# plotting
#############


set.seed(10052510)
sigma2_dist <- rgamma(100, shape = 5, rate = 10)
n_ES_dist <- 1 + stats::rpois(100, 5.5 - 1)

power_dat <-
  power_MADE(
    J = seq(40, 60, 5),
    mu = 0.1,
    tau = c(0.05, 0.1, 0.2),
    omega = c(0.1, 0.2),
    rho = c(0.2, 0.7),
    alpha = c(0.01, 0.05),
    sigma2_dist = sigma2_dist,
    n_ES_dist = n_ES_dist,
    model = c("CHE", "MLMA", "CE"),
    var_df = c("Model", "Satt", "RVE"),
    iterations = 5,
    seed = 10052510
  )

power_dat2 <-
  power_MADE(
    J = seq(40, 60, 5),
    mu = 0.1,
    tau = c(0.05, 0.1, 0.2),
    omega = c(0.1, 0.2),
    rho = c(0.2, 0.7),
    sigma2_dist = sigma2_dist,
    n_ES_dist = n_ES_dist,
    #model = c("CHE", "MLMA", "CE"),
    #var_df = c("Model", "Satt", "RVE"),
    iterations = 5,
    seed = 10052510
  )

plot_MADE_engine(
  data = power_dat2,
  x = J,
  y = power,
  x_grid = omega,
  y_grid = tau,
  color = rho,
  shape = rho,
  linetype = rho,
  color_lab = "Cor",
  shape_lab = "Cor",
  line_lab = "Cor",
  #assumptions = c("unlikely", "likely", "expected", "likely", "expected")
)

power_dat %>%
  filter(rho == 0.7, alpha == 0.05) %>%
  plot_MADE_engine(
    x = J,
    y = power,
    x_grid = omega,
    y_grid = tau,
    color = model,
    shape = model,
    linetype = model,
    color_lab = "Model",
    shape_lab = "Model",
    line_lab = "Model",
    shape_scale = "model"
    #assumptions = c("unlikely", "likely", "expected", "likely", "expected")
  )


plot_MADE(
  data = power_dat,
  power_min = 0.8,
  expected_studies = c(45, 55),
  warning = FALSE,
  caption = TRUE,
  color = TRUE,
  model_comparison = FALSE,
  traffic_light_assumptions = c("unlikely", "likely", "expected", "likely", "expected")
)


plot_MADE(
  data = power_dat2,
  power_min = 0.8,
  expected_studies = c(45, 55),
  warning = FALSE,
  caption = FALSE,
  color = TRUE,
  model_comparison = FALSE,
  traffic_light_assumptions = c("unlikely", "likely", "expected", "likely", "expected")
)


power_dat3 <-
  power_dat2 |>
  rename(cor = rho) |>
  mutate(cor = as.factor(cor))

plot_MADE_engine(
  power_dat3,
  x = J,
  y = power,
  x_grid = omega,
  y_grid = tau,
  color = cor,
  shape = cor,
  linetype = cor,
  color_lab = "Cor",
  shape_lab = "Cor",
  line_lab = "Cor",
  h_lines = 0.8,
  v_line = 50,
  v_shade = c(45, 55),
  x_lab = "Number of Studies (J)",
  y_lab = "Power",
  caption = "Test",
  grid_labs = TRUE,
  y_breaks = seq(0,1,0.2),
  y_limits = c(0,1)
)


#plot_MADE(
#
#)


#plot_MADE(
#  power_dat,
#  expected_studies = c(45, 55),
#  power_min = 0.7,
#  color = TRUE,
#  caption = TRUE,
#  #breaks = seq(40, 60, 2)
#  #numbers = FALSE
#)


J_obj <-
  min_studies_MADE(
    mu = c(0.1, 0.2),
    tau = c(0.1, 0.2),
    omega = seq(0,0.25, 0.05),
    rho = c(0.2, 0.7),
    target_power = .8,

    model = c("CHE", "MLMA"), # default
    var_df = c("Model", "Satt", "RVE"), # default

    sigma2_dist = \(x) rgamma(x, shape = 5, rate = 10),
    n_ES_dist = \(x) 1 + stats::rpois(x, 5.5 - 1),
    seed = 10052510,
    iterations = 5

); J_obj


CHE_J_1 <- J_obj |> filter(str_detect(model, "CHE-RVE"), mu == 0.1)

CHE_J <- J_obj |> filter(str_detect(model, "CHE"), omega == 0.25)


#debug(find_J_MADE)

plot_MADE.min_studies(
  data = CHE_J,
  color = FALSE,
  caption = FALSE,
  #v_shade = c(0.1,0.2),
  traffic_light_assumptions = c("unlikely", "expected", "expected")
  )


min_studies_MADE_engine(
  mu = 0.1,
  tau = 0.1,
  omega = 0.25,
  rho = 0.7,
  target_power = .8,

  #model = "CHE",
  #var_df = "Satt",

  sigma2_dist = sigma2_dist,
  n_ES_dist =  n_ES_dist,
  seed = 10052510
)

library(future)
multisession(multisession, workers = future::availableCores()-1)

#tic()
MDES_dat <-
  mdes_MADE(
    J = seq(60, 90, 10),
    tau = c(0, 0.25),
    omega = c(0, 0.1),
    rho = c(0, 0.7),
    target_power = c(.5, .8),
    alpha = c(0.01, 0.05),
    model = c("CHE"),
    var_df = c("Model", "Satt", "RVE"),
    sigma2_dist = sigma2_dist,
    n_ES_dist = n_ES_dist,
    seed = 10052510,
    iterations = 5,
    #warning = FALSE
)
#toc()


mdes_dat1 <- MDES_dat |> dplyr::filter(model == "CHE-RVE", target_power == 0.8, alpha == 0.05)

plot_MADE(
  data = MDES_dat,
  warning = FALSE,
  traffic_light_assumptions = c("likely", "expected", "unlikely", "expected")
)

#test_dat2 <-
#  test_dat |>
#  unnest(cols = c(data))

#plot_MADE(
#  MDES_dat,
#  expected_studies = c(45, 55),
#  MDES_min = 0.1,
#  color = TRUE,
#  caption = TRUE,
#  #breaks = seq(40, 60, 2)
#  #numbers = FALSE
#)


mdes_obj <-
  mdes_MADE(
    J = c(40),
    tau = 0.2,
    omega = 0.1,
    rho = 0.7,
    model = c("CHE", "MLMA", "CE"),
    var_df = c("Model", "Satt", "RVE"),
    sigma2_dist = \(x) rgamma(x, shape = 5, rate = 10),
    n_ES_dist = \(x) 1 + stats::rpois(x, 5.5 - 1),
    iterations = 5,
    seed = 10052510

); mdes_obj

mdes_MADE_engine(
  J = 40,
  tau = 0.2,
  omega = 0.1,
  rho = 0.7,
  target_power = .8,

  model = "CHE",
  var_df = "Satt",

  sigma2_dist = 4/100,
  n_ES_dist = 5.5,
  seed = 10052510
)



power_obj <-
  power_MADE(
    J = c(40),
    mu = 0.1,
    tau = c(0.1, 0.2),
    omega = 0.1,
    rho = 0.7,
    sigma2_dist = sigma2_dist,
    n_ES_dist = n_ES_dist,
    #model = c("CHE", "MLMA", "CE"),
    #var_df = "Satt",
    iterations = 100,
    alpha = .05,
    seed = 10052510,
    average_power = TRUE,
    warning = TRUE

); power_obj

power_MADE(
  J = c(40, 60),
  tau = 0.2,
  omega = 0.1,
  mu = 0.1,
  rho = 0.7,
  sigma2_dist = \(x) rgamma(x, shape = 5, rate = 10),
  n_ES_dist = \(x) 1 + stats::rpois(x, 5.5 - 1),
  model = c("CHE", "MLMA", "CE"),
  var_df = c("Model", "Satt", "RVE"),
  alpha = .05,
  seed = 10052510,
  average_power = TRUE
)

power_MADE(
  J = c(40),
  tau = 0.2,
  omega = 0.1,
  mu = 0.1,
  rho = 0.7,
  sigma2_dist = 4/100,
  n_ES_dist = 5.5,
  #model = c("CHE", "MLMA", "CE"),
  #var_df = c("Model", "Satt", "RVE"),
  alpha = 0.05,
  seed = 10052510,
  average_power = TRUE
)


power_MADE_single(
  J = 40,
  mu = 0.1,
  tau = 0.2,
  omega = 0.1,
  rho = 0.7,
  sigma2j = rgamma(40, shape = 5, rate = 10),
  kj = 5.5,
  #model = "CE",
  #var_df = "RVE",
  alpha = 0.05,
  d = 0
)

sigma2_dist <- rgamma(100, shape = 5, rate = 10)
n_ES_dist <- 1 + stats::rpois(100, 5.5 - 1)

power_MADE_engine(
  J = 40,
  mu = 0.1,
  tau = 0.2,
  omega = 0.1,
  rho = 0.7,
  sigma2_dist = 4/100,
  n_ES_dist = 5.5,
  model = c("CHE"),
  var_df = c("Satt")
)


power_MADE_engine(
  J = 40,
  tau = 0.2,
  omega = 0.1,
  mu = 0.1,
  rho = 0.7,
  sigma2_dist = \(x) rgamma(x, shape = 5, rate = 10),
  n_ES_dist = \(x) 1 + stats::rpois(x, 5.5 - 1),
  model = c("CHE", "MLMA", "CE"),
  var_df = c("Model", "Satt", "RVE"),
  average_power = TRUE
)

power_MADE_engine(
  J = 40,
  tau = 0.2,
  omega = 0.1,
  mu = 0.1,
  rho = 0.7,
  sigma2_dist = sigma2_dist,
  n_ES_dist = n_ES_dist,
  model = c("CHE", "MLMA", "CE"),
  var_df = c("Model", "Satt", "RVE")
)



