test_that("Test the Nicola Demo", {

  # ----------------------------------------
  # Import of stressor response and magnitude workbook
  filename_rm <- system.file("extdata", "./simple_test/nicola_test/stressor_magnitude.xlsx", package = "CEMPRA")
  filename_sr <- system.file("extdata", "./simple_test/nicola_test/stressor_response.xlsx", package = "CEMPRA")

  dose <- StressorMagnitudeWorkbook(filename = filename_rm)
  sr_wb_dat <- StressorResponseWorkbook(filename = filename_sr)


  # ----------------------------------------------------------
  # Run the Basic Joe Model
  # ----------------------------------------------------------
  nsims <- 10
  jmr <- JoeModel_Run(dose = dose, sr_wb_dat = sr_wb_dat, MC_sims = nsims)

  expect_true(names(jmr)[1] == "ce.df")

  mmin  <- min(jmr$ce.df$CE)
  mmax  <- max(jmr$ce.df$CE)
  mmean <- mean(jmr$ce.df$CE)

  expect_true(!(is.na(mmean)))
  expect_true(mmax > mmin)

  check <- max(jmr$ce.df$simulation)
  expect_true(check == nsims)

  expect_true(names(jmr)[2] == "sc.dose.df")

  check <- mean(jmr$sc.dose.df$sys.cap)
  expect_true(!(is.na(check)))
  expect_true(check > 0)
  expect_true(check < 1)

  check1 <- nrow(jmr$sc.dose.df)
  check2 <- nrow(sr_wb_dat$main_sheet) * nsims * length(unique(dose$HUC_ID))

  expect_true(check2 == check1)

})
