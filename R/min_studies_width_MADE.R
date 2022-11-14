

min_studies_width_MADE_engine <-
  function(
    mu,
    tau,
    omega,
    rho,
    level = 0.95,
    target_width = 0.2,

    model = "CHE",
    var_df = "RVE",

    sigma2_dist = NULL,
    n_ES_dist = NULL,

    iterations = 5,
    seed = NULL,
    upper = 100,
    extendInt = "yes"

  ){

    if (!is.null(seed) && (is.function(sigma2_dist) | is.function(n_ES_dist))) {
      set.seed(seed)
    }

    ####################################
    # Sampling variance estimates labels
    ####################################

    # Assuming balanced sampling variance estimates across studies
    if (is.numeric(sigma2_dist) && length(sigma2_dist) == 1) {
      samp_method_sigma2 <- "balanced"
      sigma2j <- sigma2_dist
    }

    # Stylized distribution of sampling variance estimates
    if (is.function(sigma2_dist)) {
      samp_method_sigma2 <- "stylized"
      sigma2j <- sigma2_dist(1000)
    }

    # Empirical distribution of sampling variance estimates across studies
    if (is.numeric(sigma2_dist) & length(sigma2_dist) > 1 & length(sigma2_dist) != length(n_ES_dist)) {
      samp_method_sigma2 <- "empirical"
      sigma2j <- sigma2_dist
    }

    #########################################
    # Number of effect sizes per study labels
    #########################################

    if (is.numeric(n_ES_dist) && length(n_ES_dist) == 1) {
      # Assuming that all studies yield the same number of effect sizes
      samp_method_kj <- "balanced"
      kj <- n_ES_dist
    } else if (is.function(n_ES_dist)) {
      # Stylized distribution of the number of effect sizes per study
      samp_method_kj <- "stylized"
      kj <- n_ES_dist(1000)
    } else if (is.numeric(n_ES_dist) && length(n_ES_dist) > 1 && length(sigma2_dist) != length(n_ES_dist)) {
      # Empirical distribution of the number of effect sizes per study
      samp_method_kj <- "empirical"
      kj <- n_ES_dist
    } else if (length(sigma2_dist) > 1 && length(n_ES_dist) > 1 && length(sigma2_dist) == length(n_ES_dist)) {
      # If both sigma2js and kjs are empirically obtained
      samp_method_sigma2 <- "empirical_combi"
      samp_method_kj <- "empirical_combi"
      sigma2j <- sigma2_dist
      kj <- n_ES_dist
    }

    f <- function(J){

      precision_MADE_engine(
        J = J, tau = tau, omega = omega, mu = mu, rho = rho,
        level = level,
        model = model,
        var_df = var_df,
        sigma2_dist = sigma2_dist, n_ES_dist = n_ES_dist,
        iterations = iterations, average_precision = TRUE,
        J_hi = J, seed = seed
      )$width - target_width

    }

    if ("Satt" %in% var_df) var_df <- "Model+Satt"

    J_needed <- round(stats::uniroot(f, interval = c(4, upper), extendInt = extendInt)$root)

    tibble(
      mu = mu,
      tau = tau,
      omega = omega,
      rho = rho,
      level = level,
      target_width = target_width,
      studies_needed = J_needed,
      iterations = iterations,
      model = paste(model, var_df, sep = "-"),
      samp_method_sigma2 = samp_method_sigma2,
      samp_method_kj = samp_method_kj
    )


  }
