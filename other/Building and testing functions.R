# Building and testing

library(usethis)
library(devtools)

options(pillar.sigfig = 4) # ensure tibble include 4 digits
options(tibble.width = Inf)
options(dplyr.print_min = 310)
options(scipen = 10)
options(dplyr.summarise.inform = FALSE)

# Creates new coding script
#use_r("power_MADE")

load_all()

sigma2_dist <- rgamma(100, shape = 5, rate = 10)
n_ES_dist <- 1 + stats::rpois(100, 5.5 - 1)

power_MADE(
  J = c(40),
  tau2 = 0.2^2,
  omega2 = 0.1^2,
  beta = 0.1,
  rho = 0.7,
  sigma2_dist = \(x) rgamma(x, shape = 5, rate = 10),
  n_ES_dist = \(x) 1 + stats::rpois(x, 5.5 - 1),
  #model = c("CHE", "MLMA", "CE"),
  #var_df = c("Model", "Satt", "RVE"),
  alpha = .05,
  seed = 10052510,
  average_power = TRUE
)

power_MADE(
  J = c(40),
  tau2 = 0.2^2,
  omega2 = 0.1^2,
  beta = 0.1,
  rho = 0.7,
  sigma2_dist = \(x) rgamma(x, shape = 5, rate = 10),
  n_ES_dist = 5.5,
  #model = c("CHE", "MLMA", "CE"),
  #var_df = c("Model", "Satt", "RVE"),
  alpha = c(0.01, 0.05),
  seed = 10052510,
  average_power = TRUE
)


power_MADE_single(
  J = 40,
  tau2 = 0.2^2,
  omega2 = 0.1^2,
  beta = 0.1,
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
  tau2 = 0.2^2,
  omega2 = 0.1^2,
  beta = 0.1,
  rho = 0.7,
  sigma2_dist = 4/100,
  n_ES_dist = 5.5,
  model = c("CHE"),
  var_df = c("Satt")
)


power_MADE_engine(
  J = 40,
  tau2 = 0.2^2,
  omega2 = 0.1^2,
  beta = 0.1,
  rho = 0.7,
  sigma2_dist = \(x) rgamma(x, shape = 5, rate = 10),
  n_ES_dist = \(x) 1 + stats::rpois(x, 5.5 - 1),
  model = c("CHE", "MLMA", "CE"),
  var_df = c("Model", "Satt", "RVE"),
  average_power = TRUE
)

power_MADE_engine(
  J = 40,
  tau2 = 0.2^2,
  omega2 = 0.1^2,
  beta = 0.1,
  rho = 0.7,
  sigma2_dist = sigma2_dist,
  n_ES_dist = n_ES_dist,
  model = c("CHE", "MLMA", "CE"),
  var_df = c("Model", "Satt", "RVE")
)



