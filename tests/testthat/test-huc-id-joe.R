test_that("test-huc-id-joe", {

  # ----------------------------------------
  # Import of stressor response and magnitude workbook
  filename_sr <- system.file("extdata", "./simple_test/test_huc_id/test_sr.xlsx", package = "CEMPRA")
  filename_sm <- system.file("extdata", "./simple_test/test_huc_id/test_sm.xlsx", package = "CEMPRA")

  dose <- StressorMagnitudeWorkbook(filename = filename_sm)
  sr_wb_dat <- StressorResponseWorkbook(filename = filename_sr)

  # ----------------------------------------------------------
  # Run the Basic Joe Model
  # ----------------------------------------------------------

  nsims <- 10

  jmr1 <- JoeModel_Run(
    dose = dose,
    sr_wb_dat = sr_wb_dat,
    MC_sims = 100,
    stressors = "MWAT Rearing",
    adult_sys_cap = FALSE
  )
  resp <- jmr1$sc.dose.df
  plot(resp$dose ~ resp$HUC)
  # Look at HUC_ID 4
  points(dose$Mean ~ dose$HUC_ID, col = "red", pch = 19, cex = 2)

  should_be_16 <- mean(resp$dose[resp$HUC == 4], na.rm = TRUE)

  should_be_24 <- mean(resp$dose[resp$HUC == 1], na.rm = TRUE)

  # Test that stressor magnitude was processed correctly through the Joe Model
  res <- abs(1 - should_be_24/24)
  expect_true(res < 0.05)

  # Test that stressor magnitude was processed correctly through the Joe Model
  res <- abs(1 - should_be_16/16)
  expect_true(res < 0.05)


})
