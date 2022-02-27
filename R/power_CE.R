
#' @importFrom magrittr %>%
#' @importFrom stats df
#' @import dplyr

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
