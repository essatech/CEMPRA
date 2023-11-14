test_that("Joe Model Setup", {

  # ----------------------------------------
  # Import of stressor response and magnitude workbook
  filename_rm <- system.file("extdata", "stressor_magnitude_unc_ARTR.xlsx", package = "CEMPRA")
  filename_sr <- system.file("extdata", "stressor_response_fixed_ARTR.xlsx", package = "CEMPRA")

  dose <- StressorMagnitudeWorkbook(filename = filename_rm, scenario_worksheet = "natural_unc")
  sr_wb_dat <- StressorResponseWorkbook(filename = filename_sr)


  # ----------------------------------------------------------
  # Run the Basic Joe Model
  # ----------------------------------------------------------

  nsims <- 10

  jmr <- JoeModel_Run(dose = dose, sr_wb_dat = sr_wb_dat, MC_sims = nsims)


  # ----------------------------------------------------------
  # Run tests on outputs

  # Check ce.df object
  # Any missing watersheds?
  diff1 <- setdiff(unique(jmr$ce.df$HUC), unique(dose$HUC_ID))
  expect_true(length(diff1) == 0)

  diff2 <- setdiff(unique(jmr$sc.dose.df$HUC), unique(dose$HUC_ID))
  expect_true(length(diff2) == 0)

  # Correct number of simulations
  check_nsim <- unique(jmr$sc.dose.df$simulation) %>% length()
  expect_true(check_nsim == nsims)

  check_nsim <- unique(jmr$ce.df$simulation) %>% length()
  expect_true(check_nsim == nsims)

  # That CE values for watershed make sense
  expect_true(max(jmr$ce.df$CE) <= 1 & max(jmr$ce.df$CE) >= 0.5)

  # That System Capacity Values Make Sense
  expect_true(min(jmr$sc.dose.df$sys.cap) == 0)
  # expect_true(max(jmr$sc.dose.df$sys.cap) == 1)
  expect_true(sd(jmr$sc.dose.df$sys.cap) > 0)




  # ----------------------------------------------------------
  # Test Joe Model with simple variable sets
  # ----------------------------------------------------------

  # Import of stressor response and magnitude workbook
  filename_rm <- system.file("extdata/simple_test",
                             "stressor_magnitude_simple.xlsx",
                             package = "CEMPRA")
  filename_sr <- system.file("extdata/simple_test",
                             "stressor_response_simple.xlsx",
                             package = "CEMPRA")

  dose <- StressorMagnitudeWorkbook(filename = filename_rm)
  sr_wb_dat <- StressorResponseWorkbook(filename = filename_sr)

  # ----------------------------------------------------------
  # Run the Basic Joe Model
  # ----------------------------------------------------------
  MC_sims <- 10
  stressors <- sr_wb_dat$stressor_names
  jmr <- JoeModel_Run(dose = dose, sr_wb_dat = sr_wb_dat, stressors = stressors, MC_sims = MC_sims)

  # Test conditions MAX int
  s1 <- jmr$ce.df
  s2 <- s1[s1$HUC == 1, ]
  mcheck <- median(s2$CE)
  # A 14 = 0.6 x
  # B 7 = 1.0 ***
  # C 12 = 0.2 ***
  # D 3.8 = 0.4 x
  # E 5 = 1.0
  # RESULT: 1.0 * 0.2 * 1.0
  # expect 0.2
  expect_true(round(mcheck,2) == 0.2)

  # Test conditions HUC2
  s1 <- jmr$ce.df
  s2 <- s1[s1$HUC == 2, ]
  mcheck <- median(s2$CE)
  # A 8 = 1.0 ***
  # B 0 = 0.1 x
  # C 20 = 1.0 x
  # D 900 = 0.2 **
  # E 0 = 0.4
  # RESULT: 1.0 * 0.2 * 0.4
  # expect 0.08
  expect_true(round(mcheck, 2) == 0.08)


  # ----------------------------------------------------------
  # Load and test FREP dataset
  # Import of stressor response and magnitude workbook
  filename_rm <- system.file("extdata", "./frep/FREP_Stressor_Magnitude.xlsx", package = "CEMPRA")
  filename_sr <- system.file("extdata", "./frep/FREP_Stressor_Response.xlsx", package = "CEMPRA")
  dose <- StressorMagnitudeWorkbook(filename = filename_rm)
  sr_wb_dat <- StressorResponseWorkbook(filename = filename_sr)
  # Run the Basic Joe Model
  nsims <- 5
  jmr <- JoeModel_Run(dose = dose, sr_wb_dat = sr_wb_dat, MC_sims = nsims)

  # Check for realistic values in run
  expect_true(mean(jmr$ce.df$CE) > 0)
  expect_true(mean(jmr$ce.df$CE) < 1)
  expect_true(mean(jmr$sc.dose.df$sys.cap, na.rm = TRUE) > 0)
  expect_true(mean(jmr$sc.dose.df$sys.cap, na.rm = TRUE) < 1)

  jmr <- JoeModel_Run(dose = dose[dose$HUC_ID == 168976, ], sr_wb_dat = sr_wb_dat, MC_sims = nsims, adult_sys_cap = FALSE)

  # Check for realistic values in run
  expect_true(round(mean(jmr$ce.df$CE), 3) == 0)
  expect_true(mean(jmr$sc.dose.df$sys.cap, na.rm = TRUE) > 0)
  expect_true(mean(jmr$sc.dose.df$sys.cap, na.rm = TRUE) < 1)



})
