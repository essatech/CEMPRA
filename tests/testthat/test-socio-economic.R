test_that("Basic Socio Economic", {

  # ---------------------------------------------------
  # Import of stressor response and magnitude workbook
  # ---------------------------------------------------

  filename_rm <- system.file("extdata", "socio_economic/se_stressor_magnitude.xlsx", package = "CEMPRA")
  filename_sr <- system.file("extdata", "socio_economic/se_stressor_response.xlsx", package = "CEMPRA")

  dose <- StressorMagnitudeWorkbook(filename = filename_rm)
  sr_wb_dat <- StressorResponseWorkbook(filename = filename_sr)


  # ----------------------------------------------------------
  # Run the Basic Joe Model without SE
  # ----------------------------------------------------------
  nsims <- 10
  jmr <- JoeModel_Run(dose = dose, sr_wb_dat = sr_wb_dat, MC_sims = nsims)

  # Make sure Joe Model Works ok on its own
  check <- mean(jmr$ce.df$CE)
  expect_true(check > 0)

  check <- mean(jmr$sc.dose.df$sys.cap)
  expect_true(check > 0)

  dsub <- jmr$sc.dose.df
  dsub <- dsub[, c("Stressor", "sys.cap")]
  dsub <- dsub %>% dplyr::group_by(Stressor) %>% dplyr::summarise(mean = mean(sys.cap))


  # ----------------------------------------------------------
  # Load the Socio Economic Workbook
  # ----------------------------------------------------------

  filename_se <- system.file("extdata", "socio_economic/socio_economic_input.xlsx", package = "CEMPRA")
  socioeconomic_inputs <- SocioEconomicWorkbook(filename = filename_se)

  expect_true(socioeconomic_inputs$error_state == "Ready to go!")
  expect_true(socioeconomic_inputs$import_pass == TRUE)

  # Add data summary tables
  socioeconomic_inputs <- SocioEconomicRun(socioeconomic_inputs = socioeconomic_inputs)

  # Check worksheets exist
  expect_true(nrow(socioeconomic_inputs$cost_summary) > 0)
  expect_true(nrow(socioeconomic_inputs$stressor_reductions) > 0)

  # There should be some reductions
  reds <- socioeconomic_inputs$stressor_reductions$stressor_reductions
  expect_true(sum(abs(reds)) > 0)

  # There should be a non-zero cost
  reds <- socioeconomic_inputs$cost_summary$total_cost
  expect_true(sum(abs(reds)) > 10000)

  print(sum(abs(reds)))


  # ----------------------------------------------------------
  # Run the Joe Model with SE
  # ----------------------------------------------------------

  # Turn off stochasticity
  names(socioeconomic_inputs)
  socioeconomic_inputs$`Management Actions`$`SD of Cost per Unit` <- 0
  socioeconomic_inputs$`Location Implementation`$`SD Number of Units` <- 0
  dose$SD <- 0

  jmr_se <- JoeModel_Run(dose = dose, sr_wb_dat = sr_wb_dat,
                         MC_sims = nsims,
                         socioeconomic_inputs = socioeconomic_inputs)


  # We should see an increase in the mean system capacity
  check <- mean(jmr$ce.df$CE)
  check_se <- mean(jmr_se$ce.df$CE)
  expect_true(check_se > check)

  # ----------------------------------------------------------
  # Run Advanced Tests with no stochasticity
  # ----------------------------------------------------------

  # import data

  filename_rm <- system.file("extdata", "socio_economic/no_stochast/se_stressor_magnitude.xlsx", package = "CEMPRA")
  filename_sr <- system.file("extdata", "socio_economic/no_stochast/se_stressor_response.xlsx", package = "CEMPRA")
  dose <- StressorMagnitudeWorkbook(filename = filename_rm)
  sr_wb_dat <- StressorResponseWorkbook(filename = filename_sr)



  filename_se <- system.file("extdata", "socio_economic/no_stochast/socio_economic_input.xlsx", package = "CEMPRA")
  socioeconomic_inputs <- SocioEconomicWorkbook(filename = filename_se)


  expect_true(socioeconomic_inputs$error_state == "Ready to go!")
  expect_true(socioeconomic_inputs$import_pass == TRUE)


  socioeconomic_inputs$`Management Actions`$`SD of Cost per Unit` <- 0
  socioeconomic_inputs$`Location Implementation`$`SD Number of Units` <- 0


  # With no stochasticity we should get the same value on each run
  jmr_se <- JoeModel_Run(dose = dose, sr_wb_dat = sr_wb_dat,
                         MC_sims = nsims,
                         socioeconomic_inputs = socioeconomic_inputs)
  check_se <- mean(jmr_se$ce.df$CE)

  jmr_se2 <- JoeModel_Run(dose = dose, sr_wb_dat = sr_wb_dat,
                         MC_sims = nsims,
                         socioeconomic_inputs = socioeconomic_inputs)
  check_se2 <- mean(jmr_se2$ce.df$CE)

  # With no stochasticity we should get the same value on each run
  expect_true(round(check_se, 2) == round(check_se2, 2))


  # There is are linked stressor
  # "August Stream Temperature", "Sediment" - link minimum

  jmr_se <- JoeModel_Run(dose = dose, sr_wb_dat = sr_wb_dat,
                          stressors = c("August Stream Temperature", "Sediment"),
                          MC_sims = nsims,
                          socioeconomic_inputs = socioeconomic_inputs)

  df <- jmr_se$ce.df
  df <- df[df$HUC %in% c("E", "F") & df$simulation == 1, ]

  df2 <- jmr_se$sc.dose.df
  df2 <- df2[df2$HUC %in% c("E", "F") & df2$simulation == 1, ]

  # Sediment should override
  check1 <- df$CE[df$HUC == "E"]
  check2 <- df2$sys.cap[df2$Stressor == "Sediment" & df2$HUC == "E"]
  expect_true(round(check1, 2) == round(check2, 2))

  # Temperature should override
  check1 <- df$CE[df$HUC == "F"]
  check2 <- df2$sys.cap[df2$Stressor == "August Stream Temperature" & df2$HUC == "F"]
  expect_true(round(check1, 2) == round(check2, 2))



  # ----------------------------------------------------------
  # Test a second example developed by Alex
  # ----------------------------------------------------------

  filename_rm <- system.file("extdata", "socio_economic/example_2/ex2_sm.xlsx", package = "CEMPRA")
  filename_sr <- system.file("extdata", "socio_economic/example_2/ex2_sr.xlsx", package = "CEMPRA")
  dose <- StressorMagnitudeWorkbook(filename = filename_rm)
  sr_wb_dat <- StressorResponseWorkbook(filename = filename_sr)

  # Run the Basic Joe Model without SE Inputs
  nsims <- 10
  jmr <- JoeModel_Run(dose = dose, sr_wb_dat = sr_wb_dat, MC_sims = nsims)

  check <- mean(jmr$ce.df$CE)
  expect_true(check > 0)

  # Load custom SE data
  filename_se <- system.file("extdata", "socio_economic/example_2/ex2_se.xlsx", package = "CEMPRA")
  socioeconomic_inputs <- SocioEconomicWorkbook(filename = filename_se)

  # Loads ok
  expect_true(socioeconomic_inputs$error_state == "Ready to go!")
  expect_true(socioeconomic_inputs$import_pass)

  # Add data summary tables
  socioeconomic_inputs <- SocioEconomicRun(socioeconomic_inputs = socioeconomic_inputs)

  # Then run the Joe model with the new SE workbook
  dose$SD <- 0
  socioeconomic_inputs$`Location Implementation`$`SD Number of Units` <- 0

  jmr_se <- JoeModel_Run(dose = dose, sr_wb_dat = sr_wb_dat,
                         MC_sims = nsims,
                         socioeconomic_inputs = socioeconomic_inputs)

  check2 <- mean(jmr_se$ce.df$CE)
  expect_true(check2 >= check)




})
