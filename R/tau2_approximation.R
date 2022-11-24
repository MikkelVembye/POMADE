

#' @title Between-Study Variance Approximation Function
#'
#' @description Rough approximation of the between-study variance based on
#' assumption about the typical sample size of studies included in the
#' synthesis
#'
#' @param sample_size Typical sample size of studies
#' @param es Smallest effect size of practical concern
#' @param df_minus2 If degrees of freedom should be df-2 or just df
#'
#' @return A \code{tibble} with small, medium, and large magnitudes of tau2
#' @export
#'
#' @examples
#'
#' tau2_approximation(
#' sample_size = 50,
#' es = 0.1,
#' df_minus2 = TRUE
#' )
#'
#'



tau2_approximation <-
  function(sample_size = 100, es,
           df_minus2 = TRUE){

    if (length(sample_size) > 2) stop("Insert sample size for treatment and control group only")
    if (sum(sample_size) > 500) warning("This function is made for effetive sample size only")

    # Determines if -2 appears in the denominator of the scale precistion term
    # of the var calculation
    if (df_minus2){

      # Determines if equal size interventions groups is assumed
      if (length(sample_size) == 1){

        df <- sample_size - 1

        var <- 4/sample_size + es^2/(2*df-2)

      }

      if (length(sample_size) > 1){

        df <- sum(sample_size - 1)

        grp1 <- sample_size[1]
        grp2 <- sample_size[2]

        var <- (1/grp1 + 1/grp2) + es^2/(2*df-2)

      }
    } else {

      if (length(sample_size) == 1){

        df <- sample_size - 1

        var <- 4/sample_size + es^2/(2*df)

      }

      if (length(sample_size) > 1){

        df <- sum(sample_size - 1)

        grp1 <- sample_size[1]
        grp2 <- sample_size[2]

        var <- (1/grp1 + 1/grp2) + es^2/(2*df)

      }

    }

    dat <-
      data.frame(
        tau2 = c(round(var * (1/3), 3), round(var, 3), round(var * 3, 3))
      )

    row.names(dat) <- c("small", "medium", "large")

    dat

  }
