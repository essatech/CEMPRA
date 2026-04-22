test_that("test-missing-mismatch-datad", {

  # ----------------------------------------
  # Import of stressor response and magnitude workbook
  filename_rm <- system.file("extdata", "./parkinson_2016/Stressor_Magnitude_Parkinson_2016.xlsx", package = "CEMPRA")
  filename_sr <- system.file("extdata", "./parkinson_2016/Stressor_Response_Parkinson_2016.xlsx", package = "CEMPRA")

  dose <- StressorMagnitudeWorkbook(filename = filename_rm)
  sr_wb_dat <- StressorResponseWorkbook(filename = filename_sr)
  MC_sims <- 10
  adult_sys_cap <- FALSE
  socioeconomic_inputs <- NULL

  names(sr_wb_dat)

  # What if there is NA values in the header of the stressor-response main worksheet
  sr_wb_dat$main_sheet$Interaction <- NA
  sr_wb_dat$main_sheet$Linked <- NA

  jm <- JoeModel_Run(
    dose = dose,
    sr_wb_dat = sr_wb_dat,
    MC_sims = MC_sims,
    adult_sys_cap = adult_sys_cap,
    socioeconomic_inputs = socioeconomic_inputs
  )

  # ----------------------------------------
  # Import of stressor response and magnitude workbook
  filename_rm <- system.file("extdata", "./simple_test/test_sr_workbook/stressor_magnitude.xlsx", package = "CEMPRA")
  filename_sr <- system.file("extdata", "./simple_test/test_sr_workbook/stressor_response.xlsx", package = "CEMPRA")

  dose <- StressorMagnitudeWorkbook(filename = filename_rm)
  sr_wb_dat <- StressorResponseWorkbook(filename = filename_sr)
  MC_sims <- 100
  adult_sys_cap <- FALSE
  socioeconomic_inputs <- NULL
  stressors <- NA

  jm <- JoeModel_Run(
    dose = dose,
    sr_wb_dat = sr_wb_dat,
    MC_sims = MC_sims,
    adult_sys_cap = adult_sys_cap,
    socioeconomic_inputs = socioeconomic_inputs
  )

  # ----------------------------------------



  expect_true({1 == 1})



})
