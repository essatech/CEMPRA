#' Population Model LTRE Range Analysis
#'
#' @description Run an uncertainty-based LTRE (Life Table Response Experiment)
#' using Monte Carlo sampling and Partial Rank Correlation Coefficients (PRCC)
#' for global sensitivity analysis.
#'
#' @details Unlike \code{\link{pop_model_ltre}} which perturbs parameters by a
#' fixed step size, this function samples from user-defined probability
#' distributions to propagate uncertainty and identify which parameters most
#' influence lambda (population growth rate).
#'
#' The input data frame must contain the following columns:
#' \describe{
#'   \item{Parameters}{Parameter description}
#'   \item{Name}{Parameter name (e.g., "SE", "S0", "surv_1")}
#'   \item{Value}{Parameter value. Used as the mean for "normal" and "beta",
#'     the mode (most likely value) for "pert", and ignored for "uniform".}
#'   \item{Notes}{Optional notes}
#'   \item{Test_Parameter}{TRUE/FALSE indicating if parameter should be sampled}
#'   \item{Distribution}{Distribution type: "normal", "beta", "pert", or
#'     "uniform" (case-insensitive). See the Distributions section below.}
#'   \item{SD}{Standard deviation (required for "normal" and "beta"; ignored
#'     for "pert" and "uniform")}
#'   \item{Low_Limit}{Lower bound for sampling (required for every tested
#'     parameter)}
#'   \item{Up_Limit}{Upper bound for sampling (required for every tested
#'     parameter)}
#' }
#'
#' @section Distributions:
#' Every tested parameter is sampled independently, and every draw is
#' guaranteed to fall inside \code{[Low_Limit, Up_Limit]}. The four
#' distribution types differ in which columns they read and in the shape of
#' the draws:
#' \describe{
#'   \item{\code{"normal"}}{Truncated normal. \code{Value} is the mean and
#'     \code{SD} the standard deviation of the underlying normal; draws are
#'     restricted to the limits by inverse-CDF sampling (no clumping at the
#'     bounds). Best when you have a central estimate with symmetric,
#'     roughly bell-shaped uncertainty. Suitable for any parameter scale
#'     (e.g. fecundity in the thousands). \code{Value} should lie inside the
#'     limits; if it does not, every draw collapses to the mean and the
#'     parameter's PRCC is NA (a warning is issued).}
#'   \item{\code{"beta"}}{Beta on the 0-1 scale, intended for survival,
#'     maturity and other proportions. \code{Value} is the mean and
#'     \code{SD} the standard deviation; the two are converted to beta shape
#'     parameters with \code{beta_param()} (which caps very large SD
#'     values). \code{Value} must be strictly between 0 and 1. Draws are
#'     then clamped to the limits, so narrow limits relative to SD will pile
#'     mass at the bounds; use limits mainly as a safety net and let SD
#'     carry the spread.}
#'   \item{\code{"pert"}}{PERT (a rescaled beta used in expert elicitation).
#'     \code{Low_Limit} is the minimum, \code{Value} the most likely value
#'     (mode) and \code{Up_Limit} the maximum; \code{SD} is ignored. The
#'     density is zero at both limits, peaks at \code{Value}, and can be
#'     skewed when \code{Value} sits off-centre. The shape parameter is
#'     fixed at 4, giving a mean of \code{(Low + 4*Value + Up)/6}. Best when
#'     an expert can state a minimum, a most likely value and a maximum
#'     but not a standard deviation. \code{Value} must lie within the limits
#'     (hard error otherwise).}
#'   \item{\code{"uniform"}}{Uniform ("random between"). Every value in
#'     \code{[Low_Limit, Up_Limit]} is equally likely; both \code{Value} and
#'     \code{SD} are ignored for sampling. Use this when only a plausible
#'     range is known, or to give every parameter an equal-footing range
#'     screen for PRCC ranking. The parameter's \code{Value} still appears in
#'     \code{parameter_specs} for reference but has no influence on the
#'     draws.}
#' }
#'
#' @param life_cycle_params Data frame. Life cycle parameters with extended
#'   columns for distribution specifications. See Details for required columns.
#' @param n_samples Integer. Number of Monte Carlo samples to draw. Default is 500.
#' @param seed Integer or NULL. Optional random seed for reproducibility.
#'
#' @returns A list containing:
#' \describe{
#'   \item{prcc_rankings}{Data frame of PRCC results sorted by absolute importance,
#'     with columns: parameter, prcc, p_value, abs_prcc}
#'   \item{lambda_summary}{List with summary statistics: mean, median, sd,
#'     quantiles (2.5%, 5%, 10%, 25%, 50%, 75%, 90%, 95%, 97.5%),
#'     prob_lambda_gt_1, prob_lambda_lt_1, n_valid, n_failed}
#'   \item{samples}{Data frame with all sample data including sample_id, lambda,
#'     log_lambda, and all sampled parameter values}
#'   \item{parameter_specs}{Data frame of parameter specifications for sampled
#'     parameters}
#'   \item{metadata}{List with run metadata: n_samples, n_parameters_tested, seed}
#' }
#'
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' library(CEMPRA)
#'
#' # Load the sample input file
#' lc_file <- system.file("extdata/ltre/life_cycles_input_ltre.csv", package = "CEMPRA")
#' life_cycle_params <- read.csv(lc_file)
#'
#' # Run the analysis
#' result <- pop_model_ltre_range(life_cycle_params, n_samples = 1000, seed = 42)
#'
#' # View top influential parameters
#' head(result$prcc_rankings, 10)
#'
#' # How many simulations had ngative growth rates
#' # Lambda less than 1
#' sum(result$samples$lambda < 1) / nrow(result$samples)
#'
#' # View lambda distribution summary
#' result$lambda_summary
#'
#' # Plot lambda distribution
#' hist(result$samples$lambda, main = "Lambda Distribution", xlab = "Lambda")
#' abline(v = 1, col = "black", lty = 1)
#' abline(v = result$lambda_summary$median, col = "red", lty = 2, lwd = 2)
#'
#' # ============================================
#' # Horizontal bar chart of PRCC rankings
#' # ============================================
#'
#' library(ggplot2)
#'
#' # Prepare data for plotting (already sorted by abs_prcc in output)
#' prcc_data <- result$prcc_rankings
#'
#' # Reorder factor levels by absolute PRCC (highest at top, lowest at bottom)
#' prcc_data$parameter <- factor(
#'   prcc_data$parameter,
#'   levels = rev(prcc_data$parameter)
#' )
#'
#' # Define significance based on p_value
#' prcc_data$significant <- prcc_data$p_value < 0.01
#'
#' ggplot(prcc_data, aes(x = abs_prcc, y = parameter, fill = significant)) +
#'   geom_col(width = 0.7) +
#'   scale_fill_manual(
#'     values = c("TRUE" = "steelblue", "FALSE" = "coral"),
#'     labels = c("TRUE" = "(p < 0.01)", "FALSE" = "(p > 0.01)"),
#'     name = "Statistical Significance"
#'   ) +
#'   scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
#'   labs(
#'     x = "Absolute PRCC with Lambda",
#'     y = "Model Parameter",
#'     title = "Parameter Sensitivity Rankings",
#'     subtitle = paste0("Based on ", result$metadata$n_samples, " Monte Carlo samples")
#'   ) +
#'   theme_minimal(base_size = 12) +
#'   theme(
#'     panel.grid.major.y = element_blank(),
#'     panel.grid.minor = element_blank(),
#'     legend.position = "bottom"
#'   )
#' }
#'
#' @export
#'
pop_model_ltre_range <- function(life_cycle_params,
                                  n_samples = 500,
                                  seed = NULL) {

  # ============================================
  # 1. Set seed if provided
  # ============================================
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # ============================================
  # 2. Validate and parse input
  # ============================================

  # Check required standard columns
  required_cols <- c("Parameters", "Name", "Value")
  missing_std <- setdiff(required_cols, names(life_cycle_params))
  if (length(missing_std) > 0) {
    stop("Missing required columns: ", paste(missing_std, collapse = ", "))
  }

  # Check extended columns for LTRE analysis
  extended_cols <- c("Test_Parameter", "Distribution", "SD", "Low_Limit", "Up_Limit")
  missing_ext <- setdiff(extended_cols, names(life_cycle_params))
  if (length(missing_ext) > 0) {
    stop("Missing LTRE columns: ", paste(missing_ext, collapse = ", "),
         ". These columns are required for the range-based LTRE analysis.")
  }

  # ============================================
  # 3. Identify parameters to sample
  # ============================================

  # Parse Test_Parameter column - handle various TRUE representations
  test_param_values <- life_cycle_params$Test_Parameter

  # Convert to logical, handling various input formats
  is_test_param <- vapply(test_param_values, function(x) {
    if (is.logical(x)) return(isTRUE(x))
    if (is.numeric(x)) return(x == 1)
    if (is.character(x)) return(tolower(trimws(x)) %in% c("true", "1"))
    return(FALSE)
  }, logical(1))

  test_params <- which(is_test_param)
  fixed_params <- setdiff(seq_len(nrow(life_cycle_params)), test_params)

  if (length(test_params) == 0) {
    stop("No parameters marked for testing (Test_Parameter = TRUE). ",
         "At least one parameter must be tested.")
  }

  message("Found ", length(test_params), " parameters to sample: ",
          paste(life_cycle_params$Name[test_params], collapse = ", "))

  # ============================================
  # 4. Validate distribution specifications
  # ============================================

  valid_distributions <- c("normal", "beta", "pert", "uniform")

  for (i in test_params) {
    param_name <- life_cycle_params$Name[i]
    dist_type <- tolower(trimws(as.character(life_cycle_params$Distribution[i])))
    mean_val <- as.numeric(life_cycle_params$Value[i])
    sd_val <- as.numeric(life_cycle_params$SD[i])
    low <- as.numeric(life_cycle_params$Low_Limit[i])
    high <- as.numeric(life_cycle_params$Up_Limit[i])

    # Check distribution type
    if (!(dist_type %in% valid_distributions)) {
      stop("Invalid distribution type '", dist_type, "' for parameter '",
           param_name, "'. Must be one of: ", paste(valid_distributions, collapse = ", "))
    }

    # Check bounds exist
    if (is.na(low) || is.na(high)) {
      stop("Low_Limit and Up_Limit are required for parameter '", param_name, "'")
    }

    # Check bounds are valid
    if (low >= high) {
      stop("Low_Limit must be less than Up_Limit for parameter '", param_name, "'")
    }

    # Check value is within bounds (uniform ignores Value entirely, so
    # an out-of-range Value is harmless there)
    if (dist_type != "uniform" &&
        !is.na(mean_val) && (mean_val < low || mean_val > high)) {
      if (dist_type == "pert") {
        # rpert() hard-crashes downstream with a cryptic "mode must be
        # between min and max" - stop up front with a clear message
        stop("Value (", mean_val, ") is outside [Low_Limit, Up_Limit] = [",
             low, ", ", high, "] for pert-distributed parameter '",
             param_name, "'. The pert distribution requires ",
             "Low_Limit <= Value <= Up_Limit.", call. = FALSE)
      }
      warning("Value (", mean_val, ") is outside [Low_Limit, Up_Limit] = [",
              low, ", ", high, "] for parameter '", param_name,
              "'. Every sample for this parameter will sit at the ",
              "out-of-bounds mean, so its PRCC sensitivity will be NA. ",
              "Fix the Value or widen the limits.", call. = FALSE)
    }

    # Check SD for normal and beta distributions
    if (dist_type %in% c("normal", "beta")) {
      if (is.na(sd_val) || sd_val <= 0) {
        stop("SD must be a positive number for distribution type '", dist_type,
             "' (parameter '", param_name, "')")
      }
    }

    # Check beta distribution constraints
    if (dist_type == "beta") {
      if (mean_val <= 0 || mean_val >= 1) {
        stop("For beta distribution, Value must be between 0 and 1 (parameter '",
             param_name, "')")
      }
    }
  }

  # ============================================
  # 5. Create sample matrix (N x K)
  # ============================================

  n_test_params <- length(test_params)
  sample_matrix <- matrix(NA_real_, nrow = n_samples, ncol = n_test_params)
  colnames(sample_matrix) <- life_cycle_params$Name[test_params]

  for (i in seq_along(test_params)) {
    row_idx <- test_params[i]
    dist_type <- tolower(trimws(as.character(life_cycle_params$Distribution[row_idx])))
    mean_val <- as.numeric(life_cycle_params$Value[row_idx])
    sd_val <- as.numeric(life_cycle_params$SD[row_idx])
    low <- as.numeric(life_cycle_params$Low_Limit[row_idx])
    high <- as.numeric(life_cycle_params$Up_Limit[row_idx])

    sample_matrix[, i] <- switch(dist_type,
      "normal" = rtruncnorm(n_samples, mean_val, sd_val, low, high),
      "beta" = rbeta_trunc(n_samples, mean_val, sd_val, low, high),
      "pert" = rpert(n_samples, low, mean_val, high, shape = 4),
      "uniform" = stats::runif(n_samples, low, high)
    )
  }

  # ============================================
  # 6. Run matrix calculations for each sample
  # ============================================

  lambda_values <- numeric(n_samples)
  log_lambda_values <- numeric(n_samples)

  # Prepare base life cycles data frame (standard columns only)
  base_cols <- c("Parameters", "Name", "Value", "Notes")
  # Handle missing Notes column gracefully
  if (!("Notes" %in% names(life_cycle_params))) {
    life_cycle_params$Notes <- ""
  }

  message("Running ", n_samples, " Monte Carlo simulations...")

  # Progress tracking
  progress_interval <- max(1, floor(n_samples / 10))

  for (s in 1:n_samples) {
    # Create life_cycles dataframe for this sample
    lc_sample <- life_cycle_params[, base_cols, drop = FALSE]

    # Update variable parameters with sampled values
    for (i in seq_along(test_params)) {
      lc_sample$Value[test_params[i]] <- sample_matrix[s, i]
    }

    # Run pop_model_setup and pop_model_matrix_elements
    tryCatch({
      pop_setup <- pop_model_setup(life_cycles = lc_sample)
      # s0_calibrate = FALSE: lambda comes from the pre-calibration
      # projection_matrix anyway; skipping the optimize() saves time
      pop_mat <- pop_model_matrix_elements(pop_mod_setup = pop_setup,
                                           s0_calibrate = FALSE)
      lambda_values[s] <- popbio::lambda(pop_mat$projection_matrix)
      log_lambda_values[s] <- log(lambda_values[s])
    }, error = function(e) {
      lambda_values[s] <<- NA
      log_lambda_values[s] <<- NA
    })

    # Progress message
    if (s %% progress_interval == 0) {
      message("  Completed ", s, " of ", n_samples, " samples (",
              round(100 * s / n_samples), "%)")
    }
  }

  # ============================================
  # 7. Calculate PRCC
  # ============================================

  valid_idx <- !is.na(lambda_values)
  n_valid <- sum(valid_idx)
  n_failed <- sum(!valid_idx)

  if (n_valid < 10) {
    stop("Too few valid samples (", n_valid, "). ",
         "Check your parameter specifications for invalid combinations.")
  }

  if (n_failed > 0) {
    warning(n_failed, " samples failed (", round(100 * n_failed / n_samples, 1),
            "%). PRCC calculated on ", n_valid, " valid samples.")
  }

  # Calculate PRCC using log(lambda) as recommended for multiplicative processes
  prcc_results <- calculate_prcc(
    X = sample_matrix[valid_idx, , drop = FALSE],
    y = log_lambda_values[valid_idx]
  )

  # ============================================
  # 8. Calculate summary statistics
  # ============================================

  lambda_summary <- list(
    mean = mean(lambda_values, na.rm = TRUE),
    median = stats::median(lambda_values, na.rm = TRUE),
    sd = stats::sd(lambda_values, na.rm = TRUE),
    quantiles = stats::quantile(
      lambda_values,
      probs = c(0.025, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.975),
      na.rm = TRUE
    ),
    prob_lambda_gt_1 = mean(lambda_values > 1, na.rm = TRUE),
    prob_lambda_lt_1 = mean(lambda_values < 1, na.rm = TRUE),
    n_valid = n_valid,
    n_failed = n_failed
  )

  # ============================================
  # 9. Build return object
  # ============================================

  # Full sample data for custom plotting
  samples_df <- data.frame(
    sample_id = 1:n_samples,
    lambda = lambda_values,
    log_lambda = log_lambda_values,
    sample_matrix,
    stringsAsFactors = FALSE
  )

  # Parameter specifications for reference
  param_specs <- life_cycle_params[test_params,
    c("Name", "Value", "Distribution", "SD", "Low_Limit", "Up_Limit"),
    drop = FALSE
  ]
  rownames(param_specs) <- NULL

  ret_obj <- list(
    # PRCC results ranked by absolute importance
    prcc_rankings = prcc_results[order(-prcc_results$abs_prcc), ],

    # Lambda distribution summary
    lambda_summary = lambda_summary,

    # Full sample data for custom plotting
    samples = samples_df,

    # Parameter specifications for reference
    parameter_specs = param_specs,

    # Metadata
    metadata = list(
      n_samples = n_samples,
      n_parameters_tested = n_test_params,
      seed = seed
    )
  )

  # Reset row names on PRCC rankings
  rownames(ret_obj$prcc_rankings) <- NULL

  message("LTRE range analysis complete.")
  message("  Lambda mean: ", round(lambda_summary$mean, 4),
          ", median: ", round(lambda_summary$median, 4))
  message("  Pr(lambda > 1) = ", round(lambda_summary$prob_lambda_gt_1, 3),
          ", Pr(lambda < 1) = ", round(lambda_summary$prob_lambda_lt_1, 3))

  return(ret_obj)
}
