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



  # ----------------------------------------------------------
  # Turn stressors on and off in the Joe Model and make
  # sure filters are working properly
  # ----------------------------------------------------------
  unique(dose$Stressor)

  filename_sr <- system.file("extdata", "./simple_test/test_huc_id/stressor_response.xlsx", package = "CEMPRA")
  filename_sm <- system.file("extdata", "./simple_test/test_huc_id/stressor_magnitude.xlsx", package = "CEMPRA")

  dose <- StressorMagnitudeWorkbook(filename = filename_sm)
  sr_wb_dat <- StressorResponseWorkbook(filename = filename_sr)
  unique(dose$Stressor)

  blist <- list()
  mvars <- c("One", "All", "Fines", "Stream Temp.", "Low Flows")


  for(m in 1:length(mvars)) {

    this_var <- mvars[m]

    if(this_var == "One") {
      s_list <- c("Cap Rearing")
    }

    if(this_var == "All") {
      s_list <- c("Fines", "MWAT Rearing", "MWAT Prespawn", "Cap Spawning", "Cap Rearing")
    }

    if(this_var == "Fines") {
      s_list <- c("MWAT Rearing", "MWAT Prespawn", "Cap Spawning", "Cap Rearing")
    }

    if(this_var == "Stream Temp.") {
      s_list <- c("Fines", "Cap Spawning", "Cap Rearing")
    }

    if(this_var == "Low Flows") {
      s_list <- c("Fines", "MWAT Rearing", "MWAT Prespawn")
    }

    # Run Joe Model with all Stressors
    jmr0 <- JoeModel_Run(
      dose = dose,
      sr_wb_dat = sr_wb_dat,
      MC_sims = 5,
      stressors = s_list,
      adult_sys_cap = FALSE
    )

    # Make sure these vectors are the same
    check_1 <- as.character(sort(s_list))
    check_2 <- as.character(sort(unique(jmr0$sc.dose.df$Stressor)))

    check_diff1 <- setdiff(check_2, check_1)
    check_diff2 <- setdiff(check_1, check_2)

    expect_true(length(check_diff1) == 0)
    expect_true(length(check_diff2) == 0)


    jmr00 <- jmr0$ce.df
    jmr00$group <- mvars[m]
    blist[[m]] <- jmr00
    print(m)

  }






})
