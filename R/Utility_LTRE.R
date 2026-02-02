#' LTRE Utility Functions
#'
#' @description Helper functions for LTRE range analysis including distribution
#' sampling and PRCC calculation.
#'
#' @name Utility_LTRE
NULL


#' Sample from PERT Distribution
#'
#' @description Sample from a PERT (Program Evaluation and Review Technique)
#' distribution, which is a scaled Beta distribution useful for expert elicitation.
#'
#' @param n Integer. Number of samples to draw.
#' @param min Numeric. Minimum value (lower bound).
#' @param mode Numeric. Most likely value (mode).
#' @param max Numeric. Maximum value (upper bound).
#' @param shape Numeric. Shape parameter (lambda), default = 4. Higher values
#'   concentrate samples around the mode.
#'
#' @returns Numeric vector of n samples from the PERT distribution.
#'
#' @details The PERT distribution is parameterized using the modified Beta
#' distribution where:
#' \itemize{
#'   \item alpha = 1 + shape * (mode - min) / (max - min)
#'   \item beta = 1 + shape * (max - mode) / (max - min)
#' }
#' Samples are then scaled from [0,1] to [min, max].
#'
#' @keywords internal
#'
rpert <- function(n, min, mode, max, shape = 4) {
  # Validate inputs

  if (min >= max) {
    stop("min must be less than max")
  }
  if (mode < min || mode > max) {
    stop("mode must be between min and max")
  }

  range_val <- max - min
  alpha <- 1 + shape * (mode - min) / range_val
  beta_param <- 1 + shape * (max - mode) / range_val

  samples <- stats::rbeta(n, alpha, beta_param)
  min + samples * range_val
}


#' Sample from Truncated Normal Distribution
#'
#' @description Sample from a normal distribution truncated to specified bounds
#' using the inverse CDF method.
#'
#' @param n Integer. Number of samples to draw.
#' @param mean Numeric. Mean of the underlying normal distribution.
#' @param sd Numeric. Standard deviation of the underlying normal distribution.
#' @param lower Numeric. Lower truncation bound.
#' @param upper Numeric. Upper truncation bound.
#'
#' @returns Numeric vector of n samples from the truncated normal distribution.
#'
#' @details Uses the inverse CDF method: samples uniform values between
#' pnorm(lower) and pnorm(upper), then applies qnorm to obtain truncated
#' normal samples.
#'
#' @keywords internal
#'
rtruncnorm <- function(n, mean, sd, lower, upper) {
  # Validate inputs
  if (lower >= upper) {
    stop("lower must be less than upper")
  }
  if (sd <= 0) {
    stop("sd must be positive")
  }

  # Use inverse CDF method with truncation

  lower_p <- stats::pnorm(lower, mean, sd)
  upper_p <- stats::pnorm(upper, mean, sd)

  # Check for extreme truncation

  if (upper_p - lower_p < 1e-10) {
    warning("Truncation bounds are very extreme relative to mean/sd. ",
            "Returning samples at the mode.")
    return(rep(mean, n))
  }

  u <- stats::runif(n, lower_p, upper_p)
  stats::qnorm(u, mean, sd)
}


#' Sample from Truncated Beta Distribution
#'
#' @description Sample from a beta distribution with truncation to specified bounds.
#' Uses the existing beta_param() function to convert mean/sd to shape parameters.
#'
#' @param n Integer. Number of samples to draw.
#' @param mean Numeric. Mean value (must be between 0 and 1 for standard beta).
#' @param sd Numeric. Standard deviation.
#' @param lower Numeric. Lower bound for truncation.
#' @param upper Numeric. Upper bound for truncation.
#'
#' @returns Numeric vector of n samples from the truncated beta distribution.
#'
#' @details Uses the beta_param() function to estimate alpha and beta parameters
#' from mean and sd, then samples and truncates to bounds. For values outside
#' [0,1], the function adjusts the sampling appropriately.
#'
#' @keywords internal
#'
rbeta_trunc <- function(n, mean, sd, lower, upper) {
  # Validate inputs
  if (lower >= upper) {
    stop("lower must be less than upper")
  }
  if (mean < 0 || mean > 1) {
    stop("mean must be between 0 and 1 for beta distribution")
  }

  # Use existing beta_param() to get shape parameters
  params <- beta_param(mean, sd)
  samples <- stats::rbeta(n, params$alpha, params$beta)

  # Truncate to bounds
  pmax(pmin(samples, upper), lower)
}


#' Calculate Partial Rank Correlation Coefficients
#'
#' @description Calculate Partial Rank Correlation Coefficients (PRCC) for
#' global sensitivity analysis. PRCC measures the correlation between each
#' input parameter and the output while controlling for the effects of all
#' other parameters.
#'
#' @param X Matrix. Parameter samples with dimensions N x K, where N is the
#'   number of samples and K is the number of parameters. Column names should
#'   be parameter names.
#' @param y Numeric vector. Outcome values (e.g., lambda or log(lambda)) of
#'   length N.
#'
#' @returns Data frame with columns:
#' \describe{
#'   \item{parameter}{Parameter name from column names of X}
#'   \item{prcc}{PRCC value ranging from -1 to +1}
#'   \item{p_value}{Statistical significance of the correlation}
#'   \item{abs_prcc}{Absolute value of PRCC for ranking importance}
#' }
#'
#' @details PRCC is calculated by:
#' \enumerate{
#'   \item Rank-transforming all input columns and the outcome
#'   \item For each parameter k, regressing out the effect of other parameters
#'     from both the parameter and the outcome
#'   \item Computing the Pearson correlation between the residuals
#' }
#'
#' PRCC is robust to non-linear but monotonic relationships between parameters
#' and outcomes.
#'
#' @references
#' Marino, S., Hogue, I. B., Ray, C. J., & Kirschner, D. E. (2008). A methodology
#' for performing global uncertainty and sensitivity analysis in systems biology.
#' Journal of theoretical biology, 254(1), 178-196.
#'
#' @keywords internal
#'
calculate_prcc <- function(X, y) {
  # Validate inputs
  if (!is.matrix(X)) {
    X <- as.matrix(X)
  }
  if (nrow(X) != length(y)) {
    stop("Number of rows in X must equal length of y")
  }
  if (ncol(X) < 1) {
    stop("X must have at least one column")
  }

  # Rank-transform all columns and outcome
  X_ranked <- apply(X, 2, rank)
  y_ranked <- rank(y)

  K <- ncol(X)
  prcc_values <- numeric(K)
  p_values <- numeric(K)

  for (k in 1:K) {
    # Get indices of other columns
    other_cols <- setdiff(1:K, k)

    if (length(other_cols) > 0) {
      # Regress X_k on other X columns to get residuals
      X_other <- X_ranked[, other_cols, drop = FALSE]

      # Residuals of X_k after removing effect of other parameters
      resid_x <- stats::residuals(stats::lm(X_ranked[, k] ~ X_other))

      # Residuals of y after removing effect of other parameters
      resid_y <- stats::residuals(stats::lm(y_ranked ~ X_other))
    } else {
      # Only one parameter, no need to partial out anything
      resid_x <- X_ranked[, k]
      resid_y <- y_ranked
    }

    # Correlation of residuals is the partial correlation
    # Handle edge case where residuals have zero variance (too few samples)
    if (stats::sd(resid_x) < 1e-10 || stats::sd(resid_y) < 1e-10) {
      prcc_values[k] <- NA_real_
      p_values[k] <- NA_real_
    } else {
      cor_test <- stats::cor.test(resid_x, resid_y)
      prcc_values[k] <- cor_test$estimate
      p_values[k] <- cor_test$p.value
    }
  }

  # Build result data frame
  result <- data.frame(
    parameter = colnames(X),
    prcc = prcc_values,
    p_value = p_values,
    abs_prcc = abs(prcc_values),
    stringsAsFactors = FALSE
  )

  # Remove rownames

  rownames(result) <- NULL

  result
}
