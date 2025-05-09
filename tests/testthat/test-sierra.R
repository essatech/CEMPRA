test_that("sierra_file", {

  filename_rm <-
    system.file("extdata", "/simple_test/test_sierra/berland_baseline_V2.xlsx", package = "CEMPRA")
  filename_sr <-
    system.file("extdata", "/simple_test/test_sierra/stressor-response_ARTR_V1.xlsx", package = "CEMPRA")


  # load stressor-response functions from workbook
  berland_ARTR <- StressorResponseWorkbook(filename = filename_sr)

  # load stressor-magnitude values from workbook
  berland_baseline <- StressorMagnitudeWorkbook(filename = filename_rm)

  #### Run the Basic Joe Model ####
  jmr1 <- JoeModel_Run(dose = berland_baseline, # Stressor Magnitude Object
                       sr_wb_dat = berland_ARTR, # Stressor Response Object
                       MC_sims = 10 # Number of Monte Carlo simulations
  )


  #JoeModel_Run system capacities
  check_this <- jmr1$ce.df %>%
    dplyr::filter(simulation == 1)

  check_value <- check_this$CE[check_this$HUC == 1701030106] #check system capacity for HUC 170130106
  # check_value # used to be 0.40979 (wrong)
  # needs to be closer to 0.0956

  expect_true(check_value < 0.4)
  expect_true(check_value > 0.05)

  #for HUC 170130106
  check2 <- 0.81 * 0.99 * 0.89 * 0.6 * 0.33 * 0.95 # sys.cap of minimum interactions multiplied

  expect_true(round(check_value, 1) == round(check2, 1))



})
