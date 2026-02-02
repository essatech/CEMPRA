test_that("pop_model_ltre_range runs with sample input file", {

  # Load the sample input file
  lc_file <- system.file("extdata/ltre/life_cycles_input_ltre.csv", package = "CEMPRA")
  life_cycle_params <- read.csv(lc_file, stringsAsFactors = FALSE)

  # Run with small n_samples for testing speed
  result <- pop_model_ltre_range(life_cycle_params, n_samples = 50, seed = 123)

  head(result)

  # Check return structure
  expect_true("prcc_rankings" %in% names(result))
  expect_true("lambda_summary" %in% names(result))
  expect_true("samples" %in% names(result))
  expect_true("parameter_specs" %in% names(result))
  expect_true("metadata" %in% names(result))

  # Check PRCC values are in valid range [-1, 1]
  expect_true(all(result$prcc_rankings$prcc >= -1 & result$prcc_rankings$prcc <= 1))

  # Check lambda values are positive (where not NA)
  valid_lambdas <- result$samples$lambda[!is.na(result$samples$lambda)]
  expect_true(all(valid_lambdas > 0))

  # Check samples data frame has expected columns
  expect_true("sample_id" %in% names(result$samples))
  expect_true("lambda" %in% names(result$samples))
  expect_true("log_lambda" %in% names(result$samples))

  # Check number of samples matches

  expect_equal(nrow(result$samples), 50)
  expect_equal(result$metadata$n_samples, 50)

  # Check lambda summary has expected components
  expect_true("mean" %in% names(result$lambda_summary))
  expect_true("median" %in% names(result$lambda_summary))
  expect_true("sd" %in% names(result$lambda_summary))
  expect_true("quantiles" %in% names(result$lambda_summary))
  expect_true("prob_lambda_gt_1" %in% names(result$lambda_summary))
  expect_true("prob_lambda_lt_1" %in% names(result$lambda_summary))

  # Probabilities should be between 0 and 1
  expect_true(result$lambda_summary$prob_lambda_gt_1 >= 0 &&
              result$lambda_summary$prob_lambda_gt_1 <= 1)
  expect_true(result$lambda_summary$prob_lambda_lt_1 >= 0 &&
              result$lambda_summary$prob_lambda_lt_1 <= 1)

})


test_that("input validation catches missing required columns", {

  # Test with missing standard columns
  bad_input <- data.frame(Name = "test", Value = 1)
  expect_error(pop_model_ltre_range(bad_input), "Missing required columns")

  # Test with missing LTRE columns
  incomplete_input <- data.frame(
    Parameters = "Test param",
    Name = "test",
    Value = 0.5,
    Notes = ""
  )
  expect_error(pop_model_ltre_range(incomplete_input), "Missing LTRE columns")

})


test_that("input validation catches invalid distribution types", {

  # Create input with invalid distribution
  bad_dist_input <- data.frame(
    Parameters = c("Nstage", "Test param"),
    Name = c("Nstage", "test"),
    Value = c(3, 0.5),
    Notes = c("", ""),
    Test_Parameter = c(FALSE, TRUE),
    Distribution = c("", "invalid_dist"),
    SD = c(NA, 0.1),
    Low_Limit = c(NA, 0.1),
    Up_Limit = c(NA, 0.9),
    stringsAsFactors = FALSE
  )

  expect_error(pop_model_ltre_range(bad_dist_input), "Invalid distribution type")

})


test_that("input validation catches missing bounds", {

  # Create input with missing bounds
  missing_bounds_input <- data.frame(
    Parameters = c("Nstage", "Test param"),
    Name = c("Nstage", "test"),
    Value = c(3, 0.5),
    Notes = c("", ""),
    Test_Parameter = c(FALSE, TRUE),
    Distribution = c("", "normal"),
    SD = c(NA, 0.1),
    Low_Limit = c(NA, NA),  # Missing lower bound
    Up_Limit = c(NA, 0.9),
    stringsAsFactors = FALSE
  )

  expect_error(pop_model_ltre_range(missing_bounds_input), "Low_Limit and Up_Limit are required")

})


test_that("rpert distribution samples within bounds", {

  set.seed(42)
  samples <- rpert(1000, min = 0.2, mode = 0.5, max = 0.8)

  # All samples should be within bounds
  expect_true(all(samples >= 0.2))
  expect_true(all(samples <= 0.8))

  # Mean should be close to mode for symmetric case
  # For asymmetric PERT, mean = (min + 4*mode + max) / 6
  expected_mean <- (0.2 + 4 * 0.5 + 0.8) / 6
  expect_true(abs(mean(samples) - expected_mean) < 0.05)

})


test_that("rtruncnorm samples within bounds", {

  set.seed(42)
  samples <- rtruncnorm(1000, mean = 0.5, sd = 0.2, lower = 0.2, upper = 0.8)

  # All samples should be within bounds
  expect_true(all(samples >= 0.2))
  expect_true(all(samples <= 0.8))

  # Mean should be close to specified mean (adjusted for truncation)
  # For symmetric truncation around mean, should be close to mean
  expect_true(abs(mean(samples) - 0.5) < 0.1)

})


test_that("rbeta_trunc samples within bounds", {

  set.seed(42)
  samples <- rbeta_trunc(1000, mean = 0.5, sd = 0.1, lower = 0.2, upper = 0.8)

  # All samples should be within bounds
  expect_true(all(samples >= 0.2))
  expect_true(all(samples <= 0.8))

  # Mean should be close to specified mean
  expect_true(abs(mean(samples) - 0.5) < 0.1)

})


test_that("calculate_prcc returns valid results", {

  set.seed(42)

  # Create synthetic data with known relationships
  n <- 100
  x1 <- runif(n, 0, 1)
  x2 <- runif(n, 0, 1)
  x3 <- runif(n, 0, 1)

  # y is strongly positively correlated with x1, negatively with x2, independent of x3
  y <- 2 * x1 - 1.5 * x2 + rnorm(n, 0, 0.3)

  X <- cbind(x1 = x1, x2 = x2, x3 = x3)

  prcc_result <- calculate_prcc(X, y)

  # Check structure
  expect_equal(nrow(prcc_result), 3)
  expect_true("parameter" %in% names(prcc_result))
  expect_true("prcc" %in% names(prcc_result))
  expect_true("p_value" %in% names(prcc_result))
  expect_true("abs_prcc" %in% names(prcc_result))

  # PRCC values should be in valid range
  expect_true(all(prcc_result$prcc >= -1 & prcc_result$prcc <= 1))

  # x1 should have positive PRCC
  expect_true(prcc_result$prcc[prcc_result$parameter == "x1"] > 0.5)

  # x2 should have negative PRCC
  expect_true(prcc_result$prcc[prcc_result$parameter == "x2"] < -0.5)

  # x3 should have PRCC close to zero (independent)
  expect_true(abs(prcc_result$prcc[prcc_result$parameter == "x3"]) < 0.3)

})


test_that("seed produces reproducible results", {

  lc_file <- system.file("extdata/ltre/life_cycles_input_ltre.csv", package = "CEMPRA")
  life_cycle_params <- read.csv(lc_file, stringsAsFactors = FALSE)

  # Run twice with same seed
  result1 <- pop_model_ltre_range(life_cycle_params, n_samples = 20, seed = 999)
  result2 <- pop_model_ltre_range(life_cycle_params, n_samples = 20, seed = 999)

  # Lambda values should be identical
  expect_equal(result1$samples$lambda, result2$samples$lambda)

  # PRCC values should be identical
  expect_equal(result1$prcc_rankings$prcc, result2$prcc_rankings$prcc)

})


test_that("no test parameters gives error", {

  # Create input with no parameters marked for testing
  no_test_input <- data.frame(
    Parameters = c("Nstage", "Test param"),
    Name = c("Nstage", "test"),
    Value = c(3, 0.5),
    Notes = c("", ""),
    Test_Parameter = c(FALSE, FALSE),  # None marked for testing
    Distribution = c("", "normal"),
    SD = c(NA, 0.1),
    Low_Limit = c(NA, 0.1),
    Up_Limit = c(NA, 0.9),
    stringsAsFactors = FALSE
  )

  expect_error(pop_model_ltre_range(no_test_input), "No parameters marked for testing")

})


test_that("Test_Parameter handles various TRUE representations", {

  lc_file <- system.file("extdata/ltre/life_cycles_input_ltre.csv", package = "CEMPRA")
  life_cycle_params <- read.csv(lc_file, stringsAsFactors = FALSE)

  # Count expected test parameters
  expected_count <- sum(life_cycle_params$Test_Parameter == TRUE |
                        life_cycle_params$Test_Parameter == "TRUE" |
                        life_cycle_params$Test_Parameter == 1)

  result <- pop_model_ltre_range(life_cycle_params, n_samples = 30, seed = 42)

  # Number of tested parameters should match
  expect_equal(result$metadata$n_parameters_tested, expected_count)

})
