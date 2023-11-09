#' tbeta Random
#'
#' @description Returns N random estimates for truncated beta distribution given vectors for mean and SD for the untruncated distribution of system capacity.
#'
#' @details Function returns returns one random value for each mean and sd value. Will not run with 0 or 1 values.
#'
#' @param mn_est Vector of mean values.
#' @param sd_est Vector of standard deviations.
#' @param low.limit Numeric. Defaults to 0.
#' @param up.limit Numeric. Defaults to 1 .
#'
#' @keywords internal
tbeta_rnd <- function(mn_est,
                      sd_est,
                      low.limit = 0,
                      up.limit = 1) {
  rnd.est <- numeric()

  # Issue loading package
  # https://bertcarnell.github.io/truncateddist/reference/tbeta.html
  truncateddist_rtbeta <- function(n,
                                   alpha,
                                   beta,
                                   a = 0,
                                   b = 1) {

    stopifnot(n > 0 & all(beta > 0) & all(alpha > 0))
    x <- stats::runif(n)
    Fa <- stats::pbeta(a, alpha, beta)
    Fb <- stats::pbeta(b, alpha, beta)
    y <- (1 - x) * Fa + x * Fb
    return(stats::qbeta(y, alpha, beta))
  }

  for (i in 1:length(mn_est)) {
    if (sd_est[i] == 0 | is.na(sd_est[i])) {
      # no variance in response
      rnd.est[i] <- mn_est[i] # simply return the mean estimates
    } else {
      f_beta <- beta_param(mn_est[i], sd_est[i])

      # Need to account for margins
      if (!(all(all(f_beta[2] > 0) & all(f_beta[1] > 0)))) {
        rnd.est[i] <- mn_est[i] # simply return the mean estimates
      } else {
        suppressWarnings({
          rnd.est[i] <- truncateddist_rtbeta(
            1,
            alpha = f_beta[1]$alpha,
            beta = f_beta[2]$beta,
            a = ifelse(is.na(low.limit[i]), 0, low.limit[i]),
            b = ifelse(is.na(up.limit[i]), 1, up.limit[i])
          )
        })
      }
    }
  }
  return(rnd.est)
}


#' Cumulative System Capacity Calculation
#'
#' @details Function to calculate cumulative system capacity mean and sd across simulations for each HUC from a series of Monte Carlo sims
#'
#' @param df A dataframe
#' @keywords internal
mn.sd.huc <- function(df) {
  mn <- mean(df$CE)
  sd <- stats::sd(df$CE)
  return(data.frame(mean = mn, sd = sd))
}



#' Apply CSC Across dataframe
#'
#' @details ddply function to calculate cumulative system capacity, the df contains system capacity for each stressor for a given HUC and simulation
#'
#' @param df A dataframe
#' @keywords internal
ce.func <- function(df) {

  # separate stressors without a minimum interaction
  # MJB added for NA error
  df$int.type <- ifelse(is.na(df$int.type), "no_int", df$int.type)
  df$int.type <- ifelse(df$int.type == "NA", "no_int", df$int.type)

  sys.cap.no.int <- df$sys.cap[df$int.type != "Minimum" & df$int.type != "Maximum"]

  # for those with a minimum interaction take the minimum
  sys.cap.min <- tapply(
    df$sys.cap[df$int.type == "Minimum"],
    df$link[df$int.type == "Minimum"],
    min
  )

  # MJB added June 3 2023
  # for those with a maximum interaction take the maximum
  sys.cap.max <- tapply(
    df$sys.cap[df$int.type == "Maximum"],
    df$link[df$int.type == "Maximum"],
    max
  )

  sys.cap.max <- ifelse(length(sys.cap.max) == 0, 1, sys.cap.max)
  sys.cap.min <- ifelse(length(sys.cap.min) == 0, 1, sys.cap.min)

  if(length(sys.cap.no.int) == 0) {
    sys.cap.no.int <- 1
  }
  # sys.cap.no.int <- ifelse(length(sys.cap.no.int) == 0, 1, sys.cap.no.int)

  # AT THIS POINT NO OTHER INTERACTIONS ARE CONSIDERED
  # NOTE - Total mortality is addressed prior to calculation of system capacity
  # Calculate the product across all cumulative effects
  # accounting for interactions
  ce.df <- data.frame(CE = prod(c(sys.cap.no.int, sys.cap.min, sys.cap.max)))

  return(ce.df)
}



#' rtnorm from Truncated Distributions
#'
#' @details rtnorm from Truncated Distributions
#'
#' @keywords internal
rtnorm_TruncatedDistributions <- function(n,
                                          mean = 0,
                                          sd = 1,
                                          a = -Inf,
                                          b = Inf) {

  # All zeros
  if (mean == 0 & sd == 0) {
    all_zeros <- rep(0, n)
    return(all_zeros)
  }

  # Send values back from sample
  x <- stats::runif(n)
  Fa <- stats::pnorm(a, mean, sd)
  Fb <- stats::pnorm(b, mean, sd)
  y <- (1 - x) * Fa + x * Fb

  ret <- stats::qnorm(y, mean, sd)

  return(ret)
}
