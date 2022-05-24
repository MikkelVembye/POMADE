
#' @title Approximate power for the correlated effects (CE) model
#'
#' @template power-arg
#'
#' @return Returns a \code{tibble} with information about the sampling methods, type of model, smallest effect size
#' considered to be of practical concern, the estimate variance for the effect size, degrees of freedom
#' the power estimate, and the number of iterations
#' @importFrom magrittr %>%
#' @importFrom stats df
#' @import dplyr
#'
#' @export
#'
#' @examples
#' balanced_CE_RVE_power <-
#'  power_CE(
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
#' balanced_CE_RVE_power
#'
#'Test


power_CE <-
  function(J, tau2, omega2, beta, rho,

           var_df = "RVE",

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


      if ("RVE" %in% var_df){

        res1 <- purrr::map2_dfr(
          .x = sample_sizes, .y = kjs, .f = power_CE_engine,
          J = J, tau2 = tau2, omega2 = omega2, beta = beta, rho = rho,
          alpha = alpha, sigma2js = NULL, var_df = "RVE",
          .id = "samp_method"
        )

        res <- bind_rows(res, res1)


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

      if ("RVE" %in% var_df){

        res2 <- purrr::map2_dfr(
          .x = sigma2js, .y = kjs, .f = power_CE_engine,
          J = J, tau2 = tau2, omega2 = omega2, beta = beta, rho = rho,
          alpha = alpha, sample_sizes = NULL, var_df = "RVE",
          .id = "samp_method"
        )

        res <- bind_rows(res, res2)


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
# Power function for the CE model with Satterthwaite d.f.
#--------------------------------------------------------------------------

power_CE_engine <-
  function(
    J, tau2, omega2,
    beta, rho,
    var_df = "RVE",
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

    tau2_e <- tau2 + omega2 * (1 - sum(1 / (kj * sigma2j^2))) / (1 - sum(1 / sigma2j^2))

    wj <- 1 / (sigma2j + tau2_e)

    W <- sum(wj)

    var_b <- 1/W^2 * sum(wj^2 * (tau2 + rho * sigma2j + (1 / kj) * (omega2 + (1 - rho) * sigma2j)) )

    lambda_CE <- (beta-d) / sqrt(var_b)

    df_CE_app <- Satt_df(wj)


    if ("RVE" %in% var_df){

      power_CE <- power_t(df = df_CE_app, lambda = lambda_CE, alpha = alpha)

      res1 <-
        tibble(
          var_b,
          df = df_CE_app,
          power_CE,
          method = "CE-RVE"
        )

      res <- bind_rows(res, res1)


    } else {

      stop("Made for RVE, only")

    }

    res %>% mutate(vectorof = vectorof)

  }
