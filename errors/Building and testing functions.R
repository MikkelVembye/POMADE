# Building and testing

library(devtools)

use_r("power_MADE")

load_all()

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

x <- 1:100

sample(x, size = 40, replace = TRUE)
