#' @title Approximate power for the Multil-Level Meta-Analysis (MLMA) model
#'
#' @template power-arg
#'
#' @return Returns a \code{tibble} with information about the sampling methods, type of model, smallest effect size
#' considered to be of practical concern, the estimate variance for the effect size, degrees of freedom
#' the power estimate, and the number of iterations used to calculated the average power
#' @importFrom magrittr %>%
#' @importFrom stats df
#' @import dplyr
#'
#' @export
#'
#' @examples
#'
#' balanced_MLMA_RVE_power <-
#'  power_MLMA(
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
#' balanced_MLMA_RVE_power
#'

power_MLMA <-
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
          .x = sample_sizes, .y = kjs, .f = power_MLMA_engine,
          J = J, tau2 = tau2, omega2 = omega2, beta = beta, rho = rho,
          alpha = alpha, sigma2js = NULL, var_df = "Model",
          .id = "samp_method"
        )

        res <-  bind_rows(res, res1)

      }

      if ("Satt" %in% var_df){

        res2 <- purrr::map2_dfr(
          .x = sample_sizes, .y = kjs, .f = power_MLMA_engine,
          J = J, tau2 = tau2, omega2 = omega2, beta = beta, rho = rho,
          alpha = alpha, sigma2js = NULL, var_df = "Satt",
          .id = "samp_method"
        )

        res <- bind_rows(res, res2)


      }

      if ("RVE" %in% var_df){

        res3 <- purrr::map2_dfr(
          .x = sample_sizes, .y = kjs, .f = power_MLMA_engine,
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
          .x = sigma2js, .y = kjs, .f = power_MLMA_engine,
          J = J, tau2 = tau2, omega2 = omega2, beta = beta, rho = rho,
          alpha = alpha, sample_sizes = NULL, var_df = "Model",
          .id = "samp_method"
        )

        res <- bind_rows(res, res4)


      }

      if ("Satt" %in% var_df){

        res5 <- purrr::map2_dfr(
          .x = sigma2js, .y = kjs, .f = power_MLMA_engine,
          J = J, tau2 = tau2, omega2 = omega2, beta = beta, rho = rho,
          alpha = alpha, sample_sizes = NULL, var_df = "Satt",
          .id = "samp_method"
        )

        res <- bind_rows(res, res5)


      }

      if ("RVE" %in% var_df){

        res6 <- purrr::map2_dfr(
          .x = sigma2js, .y = kjs, .f = power_MLMA_engine,
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
# Power function engine for the MLMA model (HELPER FUNCTION)
#--------------------------------------------------------------------------
power_MLMA_engine <-
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

      if (length(sigma2js) != J){

        sigma2j <- rep(sigma2js, length.out = J)

      } else if (length(sigma2js) == J) {

        sigma2j <- sigma2js

      }

      if (length(kj) != J) kj <- rep(kj, length.out = J)

    }

    tau_omega_tilde <- find_tau_omega(tau = sqrt(tau2), omega = sqrt(omega2),
                                      phi = rho, rho = 0,
                                      k_j = kj, sigmasq_j = sigma2j)

    tau2_t <- tau_omega_tilde$tau_tilde^2
    omega2_t <- tau_omega_tilde$omega_tilde^2

    wj <- kj / (kj * tau2_t + omega2_t + sigma2j)

    W <- sum(wj)

    var_b <- 1/W^2 * sum(wj^2 * (tau2 + rho * sigma2j + (omega2 + (1 - rho) * sigma2j) / kj) )

    lambda <- (beta - d) / sqrt(var_b)


    x <- sum(wj^2) / W
    y <- sum(wj^2 / kj) / W

    a <- x^2 + W * x - 2 * sum(wj^3) / W
    b <- y^2 + sum(wj^2 / kj^2) + sum((kj - 1) / (omega2_t + sigma2j)^2) - 2 * sum(wj^3 / kj^2) / W
    c <- x * y + W * y - 2 * sum(wj^3 / kj) / W

    df_MLMA_satt <- (a * b - c^2) / (a * y^2 + b * x^2 - 2 * c * x * y)

    g <- 1 / sqrt(W * var_b)


    if ("Model" %in% var_df) {

      power_MLMA_naive <- power_t(df = df_MLMA_satt, lambda = lambda, alpha = alpha, df_test = J - 1, g = g)

      res1 <- tibble(
        var_b = var_b,
        df = J - 1,
        power_MLMA_naive,
        method = "MLMA-Model"
      )

      res <- bind_rows(res, res1)

    }

    if ("Satt" %in% var_df) {

      power_MLMA_satt <- power_t(df = df_MLMA_satt, lambda = lambda, alpha = alpha, g = g)

      res2 <- tibble(
        var_b = var_b,
        df = df_MLMA_satt,
        power_MLMA_satt,
        method = "MLMA-Model+Satt"
      )

      res <-  bind_rows(res, res2)


    }

    if ("RVE" %in% var_df){

      df_MLMA_app <- Satt_df(wj)

      power_MLMA_RVE <- power_t(df = df_MLMA_app, lambda = lambda, alpha = alpha)

      res3 <-
        tibble(
          var_b,
          df = df_MLMA_app,
          power_MLMA_RVE,
          method = "MLMA-RVE"
        )

      res <- bind_rows(res, res3)


    }

    res %>% mutate(vectorof = vectorof)

  }




# Helper function described in supplementary material of Vembye, Pustejovsky, & Pigott (2022)

CHE_KL <- function(to, tau, omega, phi, rho, k_j, sigmasq_j) {

  trs_j <- to[1]^2 + rho * sigmasq_j
  ors_j <- to[2]^2 + (1 - rho) * sigmasq_j
  w_j <- k_j / (k_j * trs_j + ors_j)
  W <- sum(w_j)

  tausq_ps_j <- tau^2 + phi * sigmasq_j
  omegasq_ps_j <- omega^2 + (1 - phi) * sigmasq_j
  wj_star <- k_j / (k_j * tausq_ps_j + omegasq_ps_j)

  A1 <- sum((k_j - 1) * omegasq_ps_j / ors_j)
  A2 <- sum(w_j / wj_star)
  A3 <- sum(w_j^2 / wj_star) / W
  B <- sum((k_j - 1) * log(ors_j) - log(w_j / k_j))
  C <- log(W)

  A1 + A2 - A3 + B + C

}


find_tau_omega <- function(tau, omega, phi, rho, k_j, sigmasq_j) {

  res <- stats::optim(par = c(tau + 0.001, omega + 0.001), fn = CHE_KL,
               tau = tau, omega = omega, phi = phi, rho = rho,
               k_j = k_j, sigmasq_j = sigmasq_j,
               lower = c(0,0), method = "L-BFGS-B")

  data.frame(tau_tilde = res$par[1], omega_tilde = res$par[2])
}
