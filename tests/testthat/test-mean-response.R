test_that("Mean Response", {

  # ----------------------------------------
  # Import of stressor response and magnitude workbook
  filename_rm <- system.file("extdata", "stressor_magnitude_unc_ARTR.xlsx", package = "CEMPRA")
  filename_sr <- system.file("extdata", "stressor_response_fixed_ARTR.xlsx", package = "CEMPRA")

  dose <- StressorMagnitudeWorkbook(filename = filename_rm, scenario_worksheet = "natural_unc")
  sr_wb_dat <- StressorResponseWorkbook(filename = filename_sr)


  # ---------------------------------------------------------
  # Ensure that worksheet names and main tab line up properly
  n1 <- names(sr_wb_dat$sr_dat)
  n2 <- sr_wb_dat$main_sheet$Stressors
  expect_true(all(n1 == n2))





  # -----------------------------------------------------
  # Run the Joe curves to generate the response functions
  # -----------------------------------------------------

  mean.resp.list <- mean_Response(
    n.stressors = nrow(sr_wb_dat$main_sheet),
    str.list = sr_wb_dat$sr_dat,
    main = sr_wb_dat$main_sheet
  )

  stressors <- sr_wb_dat$stressor_names

  # Loop through stressors to confirm functionality
  for (p in 1:length(stressors)) {

    # Doese response data
    this_stressor <- stressors[p]
    m_index <- which(names(sr_wb_dat$sr_dat) == this_stressor)
    dr_data <- sr_wb_dat$sr_dat[[m_index]]


    # ============================================
    # CHECK MEAN VALUES
    # ============================================
    f1 <- mean.resp.list[[m_index]][[1]] # Mean


    # CHECK LEFT LIMIT
    check_resp <- f1(dr_data$value[1])
    check_resp <- check_resp * 100
    expect_true(check_resp == dr_data$mean_system_capacity[1])


    # CHECK RIGHT LIMIT
    check_resp <- f1(dr_data$value[nrow(dr_data)])
    check_resp <- check_resp * 100
    expect_true(check_resp == dr_data$mean_system_capacity[nrow(dr_data)])


    # CHECK A MIDDLE VALUE
    check_resp <- f1(dr_data$value[2])
    check_resp <- check_resp * 100
    expect_true(check_resp == dr_data$mean_system_capacity[2])


    # EXTRAPOLATE LEFT BY ONE STEP
    mdat <- data.frame(val = dr_data$value, index = seq(1, nrow(dr_data), by = 1))
    z <- lm(val ~ index, data = mdat)
    pdat <- data.frame(index = -100)
    preds <- predict(z, newdata = pdat)[[1]]
    check_resp <- f1(preds)
    check_resp <- check_resp * 100
    expect_true(check_resp == dr_data$mean_system_capacity[1])


    # EXTRAPOLATE RIGHT BY ONE STEP
    mdat <- data.frame(val = dr_data$value, index = seq(1, nrow(dr_data), by = 1))
    z <- lm(val ~ index, data = mdat)
    pdat <- data.frame(index = max(mdat$index) + 100)
    preds <- predict(z, newdata = pdat)[[1]]
    check_resp <- f1(preds)
    check_resp <- check_resp * 100
    expect_true(check_resp == dr_data$mean_system_capacity[nrow(dr_data)])


    # ============================================
    # CHECK STANDAR DEVIATIONS
    # ============================================
    f2 <- mean.resp.list[[m_index]][[2]] # SD
    check_resp <- f2(dr_data$sd[1]) * 100
    expect_true(check_resp == dr_data$sd[1])

    f2 <- mean.resp.list[[m_index]][[2]] # SD
    check_resp <- f2(dr_data$sd[nrow(dr_data)]) * 100
    expect_true(check_resp == dr_data$sd[nrow(dr_data)])


    # ============================================
    # CHECK LIMITS - LOWER LIMIT
    # ============================================
    f3 <- mean.resp.list[[m_index]][[3]] # LL
    check_resp <- f3(dr_data$lwr[nrow(dr_data)]) * 100
    expect_true(check_resp == dr_data$lwr[nrow(dr_data)])

    f3 <- mean.resp.list[[m_index]][[3]] # LL
    check_resp <- f3(dr_data$lwr[1]) * 100
    expect_true(check_resp == dr_data$lwr[1])

    f3 <- mean.resp.list[[m_index]][[3]] # LL
    check_resp <- f3(-1e30) * 100
    expect_true(check_resp == dr_data$lwr[which(dr_data$value == min(dr_data$value))])


    # ============================================
    # CHECK LIMITS - UPPER LIMIT
    # ============================================
    f4 <- mean.resp.list[[m_index]][[4]] # LL
    check_resp <- f4(dr_data$upr[nrow(dr_data)]) * 100
    expect_true(check_resp == dr_data$upr[nrow(dr_data)])

    f4 <- mean.resp.list[[m_index]][[4]] # LL
    check_resp <- f4(dr_data$upr[1]) * 100
    expect_true(check_resp == dr_data$upr[1])

    f4 <- mean.resp.list[[m_index]][[4]] # LL
    check_resp <- f4(1e30) * 100
    expect_true(check_resp == dr_data$upr[which(dr_data$value == max(dr_data$value))])
  }







  # Repeat the above but set the SD to 70% for each variable
  # Set lower limit to 5
  # Set  upper limit to 95
  filename_sr <- system.file("extdata", "stressor_response_fixed_ARTR.xlsx", package = "CEMPRA")

  sr_wb_dat <- StressorResponseWorkbook(filename = filename_sr)

  for (i in 1:length(sr_wb_dat$sr_dat)) {
    m_set <- sr_wb_dat$sr_dat[[i]]
    m_set$sd <- 50
    sr_wb_dat$sr_dat[[i]] <- m_set
  }


  # -----------------------------------------------------
  # Run the Joe curves to generate the response functions
  # -----------------------------------------------------

  mean.resp.list <- mean_Response(
    n.stressors = nrow(sr_wb_dat$main_sheet),
    str.list = sr_wb_dat$sr_dat,
    main = sr_wb_dat$main_sheet
  )

  stressors <- sr_wb_dat$stressor_names

  # Loop through stressors to confirm functionality
  for (p in 1:length(stressors)) {

    # Doese response data
    this_stressor <- stressors[p]
    m_index <- which(names(sr_wb_dat$sr_dat) == this_stressor)
    dr_data <- sr_wb_dat$sr_dat[[m_index]]


    # ============================================
    # CHECK MEAN VALUES
    # ============================================
    f1 <- mean.resp.list[[m_index]][[1]] # Mean


    # CHECK LEFT LIMIT
    check_resp <- f1(dr_data$value[1])
    check_resp <- check_resp * 100
    expect_true(check_resp == dr_data$mean_system_capacity[1])


    # CHECK RIGHT LIMIT
    check_resp <- f1(dr_data$value[nrow(dr_data)])
    check_resp <- check_resp * 100
    expect_true(check_resp == dr_data$mean_system_capacity[nrow(dr_data)])


    # CHECK A MIDDLE VALUE
    check_resp <- f1(dr_data$value[2])
    check_resp <- check_resp * 100
    expect_true(check_resp == dr_data$mean_system_capacity[2])


    # EXTRAPOLATE LEFT BY ONE STEP
    mdat <- data.frame(val = dr_data$value, index = seq(1, nrow(dr_data), by = 1))
    z <- lm(val ~ index, data = mdat)
    pdat <- data.frame(index = -100)
    preds <- predict(z, newdata = pdat)[[1]]
    check_resp <- f1(preds)
    check_resp <- check_resp * 100
    expect_true(check_resp == dr_data$mean_system_capacity[1])


    # EXTRAPOLATE RIGHT BY ONE STEP
    mdat <- data.frame(val = dr_data$value, index = seq(1, nrow(dr_data), by = 1))
    z <- lm(val ~ index, data = mdat)
    pdat <- data.frame(index = max(mdat$index) + 100)
    preds <- predict(z, newdata = pdat)[[1]]
    check_resp <- f1(preds)
    check_resp <- check_resp * 100
    expect_true(check_resp == dr_data$mean_system_capacity[nrow(dr_data)])


    # ============================================
    # CHECK STANDAR DEVIATIONS
    # ============================================
    f2 <- mean.resp.list[[m_index]][[2]] # SD
    check_resp <- f2(dr_data$sd[1]) * 100
    expect_true(check_resp == dr_data$sd[1])

    f2 <- mean.resp.list[[m_index]][[2]] # SD
    check_resp <- f2(dr_data$sd[nrow(dr_data)]) * 100
    expect_true(check_resp == dr_data$sd[nrow(dr_data)])
  }
})
