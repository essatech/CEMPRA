# LTRE fixes (2026-09-22): issues 4.8/4.8b/4.9.
#
# 4.8  - per-stage eps means fecundity perturbations reach the matrix
#        (root cause fixed in pop_model_setup; immature eps rows = 0 are
#        skipped by the LTRE's zero-value filter).
# 4.8b - all LTRE runs use s0_calibrate = FALSE: previously the fry
#        survival calibration re-tuned every perturbed run's lambda back
#        toward 1, cancelling the perturbation and producing consistent
#        WRONG-SIGN sensitivities (verified: eps_4 reported -0.022 when
#        the true deterministic effect was +0.019).
# 4.9  - the `stressors` argument is now forwarded to the population-model
#        runs (it was silently ignored).
# Plus: common random numbers - status-quo/increase/decrease runs share a
# seed so Monte-Carlo noise cancels in the sensitivity differences.

test_that("LTRE sensitivities have the correct sign and magnitude", {
  lc <- read.csv(system.file("extdata", "life_cycles.csv", package = "CEMPRA"))

  set.seed(4242)
  suppressMessages(
    ret <- pop_model_ltre(step_size = 0.05,
                          dose = NA, sr_wb_dat = NA,
                          life_cycle_params = lc,
                          HUC_ID = NA, n_reps = 50)
  )

  sens_of <- function(param) {
    sm <- ret$ltre_summary_baseline
    sm$lambda[sm$this_param == param & sm$direction == "increase"] -
      sm$lambda[sm$this_param == param & sm$direction == "decrease"]
  }

  # Deterministic truth for +/-5% eps_4 on this profile is +0.0185.
  # With common random numbers the stochastic estimate lands close;
  # pre-fix (calibration active) this came out NEGATIVE (~ -0.022).
  expect_gt(sens_of("eps_4"), 0.01)
  expect_lt(sens_of("eps_4"), 0.03)

  # early survival must also be positive (was inverted pre-fix)
  expect_gt(sens_of("surv_1"), 0.005)

  # S0's raw value now genuinely matters (calibration used to overwrite it)
  expect_gt(sens_of("S0"), 0.005)

  # immature stages (eps = 0 after the maturity-based spread) are skipped
  expect_false(any(grepl("^eps_[123]$", ret$ltre_summary_baseline$this_param)))

  # reported lambdas are un-calibrated (near the profile's true lambda of
  # ~1.21, not re-tuned to ~1.0)
  expect_gt(mean(ret$ltre_summary_baseline$lambda), 1.1)
})


test_that("LTRE is reproducible under an outer seed (common random numbers)", {
  lc <- read.csv(system.file("extdata", "life_cycles.csv", package = "CEMPRA"))

  run_once <- function() {
    set.seed(99)
    suppressMessages(
      pop_model_ltre(step_size = 0.05, dose = NA, sr_wb_dat = NA,
                     life_cycle_params = lc, HUC_ID = NA, n_reps = 20)
    )
  }
  r1 <- run_once()
  r2 <- run_once()
  expect_identical(r1$ltre_summary_baseline, r2$ltre_summary_baseline)
})


test_that("LTRE forwards the stressors argument to the population model", {
  # Nanaimo demo: anadromous profile + stressor workbooks
  dose <- StressorMagnitudeWorkbook(filename = system.file(
    "extdata/nanaimo/stressor_magnitude_nanaimo.xlsx", package = "CEMPRA"))
  sr_wb_dat <- StressorResponseWorkbook(filename = system.file(
    "extdata/nanaimo/stressor_response_nanaimo.xlsx", package = "CEMPRA"))
  habitat_dd_k <- read.csv(system.file(
    "extdata/nanaimo/habitat_capacities_nanaimo.csv", package = "CEMPRA"),
    stringsAsFactors = FALSE)
  lc <- read.csv(system.file(
    "extdata/nanaimo/species_profiles/nanaimo_comp_ocean_summer.csv",
    package = "CEMPRA"))

  run_ltre <- function(stressor_subset) {
    set.seed(7)
    suppressMessages(suppressWarnings(
      pop_model_ltre(step_size = 0.05, dose = dose, sr_wb_dat = sr_wb_dat,
                     life_cycle_params = lc, HUC_ID = 1, n_reps = 10,
                     stressors = stressor_subset,
                     habitat_dd_k = habitat_dd_k)
    ))
  }

  r_all <- run_ltre(NA)
  r_sub <- run_ltre(unique(dose$Stressor)[1])

  # Pre-fix, the stressors argument was silently dropped, so both runs
  # used all stressors and were IDENTICAL under the same outer seed.
  # Post-fix the subset changes the CE runs (and, through shared RNG
  # consumption, downstream draws). Any difference proves forwarding.
  expect_false(identical(r_all$ltre_summary_ce, r_sub$ltre_summary_ce))
  expect_true(nrow(r_all$ltre_summary_ce) > 0 && nrow(r_sub$ltre_summary_ce) > 0)
})
