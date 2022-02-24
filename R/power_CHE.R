
#' @title Approximate power for the correlated-hierarchical effects (CHE) model
#'
#' @param J Expected number of studies to be found
#' @param tau2 Between-study variance
#' @param omega2 Within-study variance
#' @param beta Smallest effect size of practical/substantial concern
#' @param rho Sample correlation
#' @param var_df Variance and degrees of freedom estimation
#' @param sample_size_method Defines how sample sizes are obtained
#' @param sigma2_method Defines how sampling variance estimates are obtained
#' @param k_mean Average number of effect sizes per study
#' @param N_mean Average (effective) sample size of study
#' @param N_dist Simulated distribution o
#' @param pilot_data_kjN Pilot data containing kjs and effective sample sizes
#' @param sigma2_mean Average sampling variance
#' @param pilot_data_kjsigma2 Pilot data containing kjs and sigma2js
#' @param alpha statistical significance level
#' @param iterations Number of iterations
#' @param average_power Average power across the x iterated power estimations
#' @param seed Set seed to ensure reproducibility of the iterated power approximations
#'
#' @return Returns a data frame with information about the sampling methods, type of model, smallest effect size
#' considered to be of practical concern, the estimate variance for the effect size, degrees of freedom
#' the power estimate, and the number of iterations
#' @importFrom magrittr %>%
#' @importFrom stats df
#' @import dplyr
#'
#' @export
#'
#' @examples
#' balanced_CHE_power <-
#'  power_CHE(
#'   J = 50,
#'   tau2 = 0.2^2,
#'   omega2 = 0.1^2,
#'   beta = 0.1,
#'   rho = 0.7,
#'   var_df = "RVE",
#'   sample_size_method = "balanced",
#'   k_mean = 4,
#'   N_mean = 50,
#'   alpha = 0.05,
#'   seed = 240222
#'  )
#'
#' balanced_CHE_power
#'

power_CHE <-
  function(J, tau2, omega2, beta, rho,

           var_df = c("Model", "Satt", "RVE"),

           sample_size_method = c("balanced", "stylized", "empirical"),
           sigma2_method = c("balanced", "empirical"),

           k_mean = NULL,

           # Sample sample sizes operations
           N_mean = NULL,
           N_dist = NULL,
           pilot_data_kjN = NULL,

           # Sample variance operations
           sigma2_mean = NULL,
           #sigma2_dist = NULL,
           pilot_data_kjsigma2 = NULL,

           alpha = c(.01, .05, .1),
           iterations = 100,
           average_power = TRUE,
           seed = NULL) {

    if (!is.null(seed)) set.seed(seed)

    final_res <- tibble()


    if (!is.null(N_mean) | !is.null(N_dist) | !is.null(pilot_data_kjN)){

      kjs <- list()
      sample_sizes <- list()

      # Average sample sizes and number of effect sizes
      if ("balanced" %in% sample_size_method) {

        if (is.null(N_mean) | is.null(k_mean)) stop("Must specify values for N_mean and k_mean.")

        sample_sizes <- c(sample_sizes, balanced = N_mean)
        kjs <- c(kjs, balanced = k_mean)
      }

      # Stylized sample size and n ES distributions
      if ("stylized" %in% sample_size_method) {

        if (is.null(N_dist) | is.null(k_mean)) stop("Must specify values for sigma2_dist and k_mean.")

        styled_Ns <- purrr::rerun(iterations, pmax(10, stats::rgamma(J, shape = N_dist$estimate[1], rate = N_dist$estimate[2])))
        styled_kjs <- purrr::rerun(iterations, 1 + stats::rpois(J, k_mean - 1))

        sample_sizes <- c(sample_sizes, stylized = styled_Ns)
        kjs <- c(kjs, stylized = styled_kjs)
      }

      # Empirical sample sizes and k ES distributions
      if ("empirical" %in% sample_size_method) {

        if (is.null(pilot_data_kjN)) stop("Must specify a dataset with pilot_data.")

        pilot_sample <- purrr::rerun(iterations, n_ES_empirical(pilot_data_kjN, J))
        sample_sizes <- c(sample_sizes, empirical = purrr::map(pilot_sample, ~ .x$N))
        kjs <- c(kjs, empirical =  purrr::map(pilot_sample, ~ .x$kj))
      }

      res <- tibble()

      if ("Model" %in% var_df){

        res1 <- purrr::map2_dfr(
          .x = sample_sizes, .y = kjs, .f = power_CHE_engine,
          J = J, tau2 = tau2, omega2 = omega2, beta = beta, rho = rho,
          alpha = alpha, sigma2js = NULL, var_df = "Model",
          .id = "samp_method"
        )

        res <-  bind_rows(res, res1)

      }

      if ("Satt" %in% var_df){

        res2 <- purrr::map2_dfr(
          .x = sample_sizes, .y = kjs, .f = power_CHE_engine,
          J = J, tau2 = tau2, omega2 = omega2, beta = beta, rho = rho,
          alpha = alpha, sigma2js = NULL, var_df = "Satt",
          .id = "samp_method"
        )

        res <- bind_rows(res, res2)


      }

      if ("RVE" %in% var_df){

        res3 <- purrr::map2_dfr(
          .x = sample_sizes, .y = kjs, .f = power_CHE_engine,
          J = J, tau2 = tau2, omega2 = omega2, beta = beta, rho = rho,
          alpha = alpha, sigma2js = NULL, var_df = "RVE",
          .id = "samp_method"
        )

        res <- bind_rows(res, res3)


      }


      if (average_power) {

        final_res1 <- res %>%
          mutate(samp_method = stringr::str_remove(samp_method, "[:digit:]+")) %>%
          group_by(samp_method, method) %>%
          summarise(
            samp_method = paste(unique(samp_method), unique(vectorof)),
            across(c(var_b, df, dplyr::starts_with("power_sig")), mean),
            .groups = "drop"
          ) %>%
          mutate(
            es = beta,
            iterations = iterations
          ) %>%
          relocate(es, .before = var_b)

        final_res <- bind_rows(final_res, final_res1)

      } else {

        final_res1 <- res %>%
          mutate(samp_method = stringr::str_remove(samp_method, "[:digit:]+"))

        final_res <- bind_rows(final_res, final_res1)
      }

    }

    if (!is.null(sigma2_mean) | !is.null(pilot_data_kjsigma2)){

      kjs <- list()
      sigma2js <- list()

      # Average es sampling variance and number of effect sizes
      if ("balanced" %in% sigma2_method) {

        if (is.null(sigma2_mean) | is.null(k_mean)) stop("Must specify values for sigma2_mean and k_mean.")

        sigma2js <- c(sigma2js, balanced = sigma2_mean)
        kjs <- c(kjs, balanced = k_mean)
      }

      # Add stylized sampling variance if needed

      # Empirical es sampling variances and k ES distributions
      if ("empirical" %in% sigma2_method) {

        if (is.null(pilot_data_kjsigma2)) stop("Must specify a dataset with pilot_data.")

        pilot_sigma <- purrr::rerun(iterations, n_ES_empirical(pilot_data_kjsigma2, J))
        sigma2js <- c(sigma2js, empirical = purrr::map(pilot_sigma, ~ .x$sigma2j))
        kjs <- c(kjs, empirical =  purrr::map(pilot_sigma, ~ .x$kj))
      }

      res <- tibble()

      if ("Model" %in% var_df){

        res4 <- purrr::map2_dfr(
          .x = sigma2js, .y = kjs, .f = power_CHE_engine,
          J = J, tau2 = tau2, omega2 = omega2, beta = beta, rho = rho,
          alpha = alpha, sample_sizes = NULL, var_df = "Model",
          .id = "samp_method"
        )

        res <- bind_rows(res, res4)


      }

      if ("Satt" %in% var_df){

        res5 <- purrr::map2_dfr(
          .x = sigma2js, .y = kjs, .f = power_CHE_engine,
          J = J, tau2 = tau2, omega2 = omega2, beta = beta, rho = rho,
          alpha = alpha, sample_sizes = NULL, var_df = "Satt",
          .id = "samp_method"
        )

        res <- bind_rows(res, res5)


      }

      if ("RVE" %in% var_df){

        res6 <- purrr::map2_dfr(
          .x = sigma2js, .y = kjs, .f = power_CHE_engine,
          J = J, tau2 = tau2, omega2 = omega2, beta = beta, rho = rho,
          alpha = alpha, sample_sizes = NULL, var_df = "RVE",
          .id = "samp_method"
        )

        res <- bind_rows(res, res6)


      }


      if (average_power) {

        final_res2 <- res %>%
          mutate(samp_method = stringr::str_remove(samp_method, "[:digit:]+")) %>%
          group_by(samp_method, method) %>%
          summarise(
            samp_method = paste(unique(samp_method), unique(vectorof)),
            across(c(var_b, df, dplyr::starts_with("power_sig")), mean),
            .groups = "drop"
          ) %>%
          mutate(
            es = beta,
            iterations = iterations
          ) %>%
          relocate(es, .before = var_b)

        final_res <- bind_rows(final_res, final_res2)


      } else {

        final_res2 <- res %>%
          mutate(samp_method = stringr::str_remove(samp_method, "[:digit:]+"))

        final_res <- bind_rows(final_res, final_res2)
      }


    }

    final_res

  }




#--------------------------------------------------------------------------
# Power function engine for the CHE model with RVE (HELPER FUNCTION)
#--------------------------------------------------------------------------

# TO DO: allow individual vector for Nt and Nc to be included as well.

power_CHE_engine <-
  function(
    J, tau2, omega2,
    beta, rho,
    var_df = c("Model", "Satt", "RVE"),
    sample_sizes = NULL,
    sigma2js = NULL,
    kj,
    alpha = c(.01, .05, .1),
    d = 0
  ) {

    if (!is.null(sample_sizes) & !is.null(sigma2js)) stop("Either use sample sizes or sigma2js")

    res <- tibble()

    if (!is.null(sample_sizes)){

      vectorof <- "sample sizes"

      if (length(sample_sizes) != J) sample_sizes <- rep(sample_sizes, length.out = J)
      if (length(kj) != J) kj <- rep(kj, length.out = J)

      sigma2j <- 4 / sample_sizes + beta^2 / (2 * (sample_sizes - 2))

    }

    if (!is.null(sigma2js)){

      vectorof <- "sigma2js"

      if (length(sigma2js) != J) sigma2j <- rep(sigma2js, length.out = J)
      if (length(kj) != J) kj <- rep(kj, length.out = J)

      sigma2j <- sigma2js

    }

    wj <- kj / (kj * tau2 + kj * rho * sigma2j + omega2 + (1 - rho) * sigma2j)
    W <- sum(wj)

    var_b <- 1 / W

    lambda <- (beta - d) / sqrt(var_b)


    if ("Model" %in% var_df) {

      # Changed df_CHE_satt from df. Why should it be df_CHE_satt there?
      power_CHE_naive <- power_t(df = J - 1, lambda = lambda, alpha = alpha, df_test = J - 1)

      res1 <- tibble(
        var_b = var_b,
        df = J - 1,
        power_CHE_naive,
        method = "CHE-Model"
      )

      res <- bind_rows(res, res1)

    }

    if ("Satt" %in% var_df) {

      x <- sum(wj^2) / W
      y <- sum(wj^2 / kj) / W

      a <- x^2 + W * x - 2 * sum(wj^3) / W
      b <- y^2 + sum(wj^2 / kj^2) + sum((kj - 1) / (omega2 + (1 - rho) * sigma2j)^2) - 2 * sum(wj^3 / kj^2) / W
      c <- x * y + W * y - 2 * sum(wj^3 / kj) / W

      df_CHE_satt <- (a * b - c^2) / (a * y^2 + b * x^2 - 2 * c * x * y)

      power_CHE_satt <- power_t(df = df_CHE_satt, lambda = lambda, alpha = alpha)

      res2 <- tibble(
        var_b = var_b,
        df = df_CHE_satt,
        power_CHE_satt,
        method = "CHE-Model+Satt"
      )

      res <-  bind_rows(res, res2)


    }

    if ("RVE" %in% var_df){

      df_CHE_app <- Satt_df(wj)

      # Eq 16
      power_CHE_RVE <- power_t(df = df_CHE_app, lambda = lambda, alpha = alpha)

      res3 <-
        tibble(
          var_b,
          df = df_CHE_app,
          power_CHE_RVE,
          method = "CHE-RVE"
        )

      res <- bind_rows(res, res3)


    }

    res %>% mutate(vectorof = vectorof)

  }

utils::globalVariables(c("samp_method", "method", "vectorof", "var_b", "es"))
