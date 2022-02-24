#' @importFrom magrittr %>%

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

    res <- dplyr::tibble()

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

      res1 <- dplyr::tibble(
        var_b = var_b,
        df = J - 1,
        power_CHE_naive,
        method = "CHE-Model"
      )

      res <- dplyr::bind_rows(res, res1)

    }

    if ("Satt" %in% var_df) {

      x <- sum(wj^2) / W
      y <- sum(wj^2 / kj) / W

      a <- x^2 + W * x - 2 * sum(wj^3) / W
      b <- y^2 + sum(wj^2 / kj^2) + sum((kj - 1) / (omega2 + (1 - rho) * sigma2j)^2) - 2 * sum(wj^3 / kj^2) / W
      c <- x * y + W * y - 2 * sum(wj^3 / kj) / W

      df_CHE_satt <- (a * b - c^2) / (a * y^2 + b * x^2 - 2 * c * x * y)

      power_CHE_satt <- power_t(df = df_CHE_satt, lambda = lambda, alpha = alpha)

      res2 <- dplyr::tibble(
        var_b = var_b,
        df = df_CHE_satt,
        power_CHE_satt,
        method = "CHE-Model+Satt"
      )

      res <-  dplyr::bind_rows(res, res2)


    }

    if ("RVE" %in% var_df){

      df_CHE_app <- Satt_df(wj)

      # Eq 16
      power_CHE_RVE <- power_t(df = df_CHE_app, lambda = lambda, alpha = alpha)

      res3 <-
        dplyr::tibble(
          var_b,
          df = df_CHE_app,
          power_CHE_RVE,
          method = "CHE-RVE"
        )

      res <- dplyr::bind_rows(res, res3)


    }

    res %>% dplyr::mutate(vectorof = vectorof)

  }


