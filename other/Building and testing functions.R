# Building and testing

library(devtools)
library(usethis)

# Creates new coding script
#use_r("power_MADE")

load_all()

power_MADE_engine(
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

power_MADE_engine2(
  J = 40,
  tau2 = 0.2^2,
  omega2 = 0.1^2,
  beta = 0.1,
  rho = 0.7,
  sigma2_dist = sigma2_dist,
  n_ES_dist = 5.5,
  model = c("CHE"),
  #var_df = c("Model", "Satt", "RVE")
)


power_MADE(
  J = 40,
  tau2 = 0.2^2,
  omega2 = 0.1^2,
  beta = 0.1,
  rho = 0.7,
  sigma2_dist = 0.02,
  n_ES_dist = 5.5
)


power_MADE(
  J = 40,
  tau2 = 0.2^2,
  omega2 = 0.1^2,
  beta = 0.1,
  rho = 0.7,
  model = "CHE",
  var_df = "RVE",
  sigma2_dist = rgamma(40, shape = 5, rate = 10),
  n_ES_dist = 5.5,

)

usethis::use_build_ignore("other")
