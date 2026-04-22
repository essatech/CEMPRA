test_that("System Capacity", {
  # ----------------------------------------
  # Import of stressor response and magnitude workbook
  filename_rm <-
    system.file("extdata", "stressor_magnitude_unc_ARTR.xlsx", package = "CEMPRA")
  filename_sr <-
    system.file("extdata", "stressor_response_fixed_ARTR.xlsx", package = "CEMPRA")

  dose <-
    StressorMagnitudeWorkbook(filename = filename_rm, scenario_worksheet = "natural_unc")
  sr_wb_dat <- StressorResponseWorkbook(filename = filename_sr)


  # -----------------------------------------------------
  # Run the Joe curves to generate the response functions
  mean.resp.list <- mean_Response(
    n.stressors = nrow(sr_wb_dat$main_sheet),
    str.list = sr_wb_dat$sr_dat,
    main = sr_wb_dat$main_sheet
  )


  stressors <- sr_wb_dat$main_sheet$Stressors

  for (j in 1:length(stressors)) {
    # Choose a sample metric to test
    metric_name <- stressors[j]

    # Set up objects for system capacity function
    f.main.df <-
      sr_wb_dat$main_sheet[which(sr_wb_dat$main_sheet$Stressors == metric_name),]
    f.stressor.df <- sr_wb_dat$sr_dat[[metric_name]]
    f.mean.resp.list <-
      mean.resp.list[[which(sr_wb_dat$main_sheet$Stressors == metric_name)]]

    # Choose values to test
    test_cases <- c("low_exp", "mid", "high_exp")
    test_values <- c(-1e20, NA, 1e20)

    # Find mid values
    mid_v <- f.stressor.df[round(nrow(f.stressor.df) / 2, 0),]
    test_values[2] <- mid_v$value


    #------------------------------------------
    # Simple low, medium, high test with no SD
    #------------------------------------------

    for (v in 1:length(test_values)) {
      this_value <- test_values[v]

      # Set up dummy watershed
      test_shed <- dose[1,]
      test_shed$Stressor <- metric_name
      test_shed$Stressor_cat <- metric_name
      test_shed$Mean <- this_value
      test_shed$SD <- 0
      test_shed$normal <- "normal"
      test_shed$Low_Limit <- NA
      test_shed$Up_Limit <- NA

      test_sc <- SystemCapacity(
        f.dose.df = test_shed,
        f.main.df = f.main.df,
        f.stressor.df = f.stressor.df,
        f.mean.resp.list = f.mean.resp.list,
        n.sims = 10
      )

      # There should be no variation in values
      if (all(f.stressor.df$sd == 0)) {
        expect_true(round(sd(test_sc$sys.cap), 3) == 0)
        expect_true(round(sd(test_sc$dose), 3) == 0)
        expect_true(round(sd(test_sc$dose.mat), 3) == 0)

        # Lower limits should be equal to lower values
        if (v == 1) {
          # Expect dose to equal lowest possible value from dose-response
          expect_true(mean(test_sc$dose) == min(f.stressor.df$value))
          # Expect actual matrix values to be even lower
          expect_true(mean(test_sc$dose.mat) <= min(f.stressor.df$value))
          # Expect System capacity to be linked to the lowest value for the stressor
          min_val_msc <-
            f.stressor.df$mean_system_capacity[which(f.stressor.df$value == min(f.stressor.df$value))]
          expect_true(mean(test_sc$sys.cap) == min_val_msc / 100)
        }

        # Mid values
        if (v == 2) {
          # Expect dose to equal to the assigned value
          expect_true(round(mean(test_sc$dose), 8) == round(mid_v$value, 8))
          # Expect actual matrix values to be equal to the assigned value
          expect_true(mean(test_sc$dose.mat) == mid_v$value)
          # Expect System capacity to be linked to the the assigned value
          val_msc <-
            f.stressor.df$mean_system_capacity[which(f.stressor.df$value == mid_v$value)]
          expect_true(round(mean(test_sc$sys.cap), 8) == round(val_msc /
                                                                 100, 8))
        }

        # Upper Limits
        if (v == 3) {
          # Expect dose to equal lowest possible value from dose-response
          expect_true(mean(test_sc$dose) == max(f.stressor.df$value))
          # Expect actual matrix values to be even lower
          expect_true(mean(test_sc$dose.mat) >= max(f.stressor.df$value))
          # Expect System capacity to be linked to the lowest value for the stressor
          min_val_msc <-
            f.stressor.df$mean_system_capacity[which(f.stressor.df$value == max(f.stressor.df$value))]
          expect_true(mean(test_sc$sys.cap) == min_val_msc / 100)
        }

      }

    } # end of loop through v values with SD equal to zero





    #----------------------------------------------------------
    # Simple low, medium, high test with SD equal to CV of 0.2
    #----------------------------------------------------------

    for (v in 1:length(test_values)) {
      this_value <- test_values[v]

      # Set up dummy watershed
      test_shed <- dose[1,]
      test_shed$Stressor <- metric_name
      test_shed$Stressor_cat <- metric_name
      test_shed$Mean <- this_value
      test_shed$SD <- abs(this_value * 0.2) # SD is CV of 20%
      test_shed$normal <- "normal"
      test_shed$Low_Limit <- min(f.stressor.df$value)
      test_shed$Up_Limit <- max(f.stressor.df$value)

      # Leave lower and upper limits open
      test_sc <- SystemCapacity(
        f.dose.df = test_shed,
        f.main.df = f.main.df,
        f.stressor.df = f.stressor.df,
        f.mean.resp.list = f.mean.resp.list,
        n.sims = 10
      )

      if (v == 2) {
        #expect_true(sd(test_sc$sys.cap) != 0)
        expect_true(sd(test_sc$dose) != 0)
        expect_true(sd(test_sc$dose.mat) != 0)
      }


      # Set strict lower and upper limits
      test_shed$Low_Limit <- this_value
      test_shed$Up_Limit <- this_value

      # Leave lower and upper limits open
      test_sc <- SystemCapacity(
        f.dose.df = test_shed,
        f.main.df = f.main.df,
        f.stressor.df = f.stressor.df,
        f.mean.resp.list = f.mean.resp.list,
        n.sims = 10
      )

      # Do not expect any variation
      if (all(f.stressor.df$sd == 0)) {
        expect_true(round(sd(test_sc$sys.cap), 3) == 0)
      }
      expect_true(round(sd(test_sc$dose), 3) == 0)
      expect_true(round(sd(test_sc$dose.mat), 3) == 0)

      # Expect dose to equal assigned value
      if (v == 2) {
        expect_true(all(round(test_sc$dose, 7) == round(this_value, 7)))
        expect_true(all(round(test_sc$dose.mat, 7) == round(this_value, 7)))
      }




    } # end of loop through v values with SD not equal to zero


  } # end of loop through j stressors




  #=============================================================================
  # Test simple stressor rules for interactions
  #=============================================================================

  # ----------------------------------------
  # Import of stressor response and magnitude workbook
  filename_rm <- system.file("extdata/simple_test",
                             "stressor_magnitude_simple.xlsx",
                             package = "CEMPRA")
  filename_sr <- system.file("extdata/simple_test",
                             "stressor_response_simple.xlsx",
                             package = "CEMPRA")

  dose <- StressorMagnitudeWorkbook(filename = filename_rm)
  sr_wb_dat <- StressorResponseWorkbook(filename = filename_sr)

  # -----------------------------------------------------
  # Run the Joe curves to generate the response functions
  mean.resp.list <- mean_Response(
    n.stressors = nrow(sr_wb_dat$main_sheet),
    str.list = sr_wb_dat$sr_dat,
    main = sr_wb_dat$main_sheet
  )

  tmp_d <- dose[dose$Stressor == "E", ]
  tmp_d$Mean <- 5
  tmp_d <- tmp_d[1, ]

  # Check stressors for accuracy
  test_sc <- SystemCapacity(
    f.dose.df = tmp_d,
    f.main.df = sr_wb_dat$main_sheet[sr_wb_dat$main_sheet$Stressors == "E", ],
    f.stressor.df = sr_wb_dat$sr_dat$E,
    f.mean.resp.list = mean.resp.list[[5]],
    n.sims = 10
  )

  mcheck <- mean(test_sc$sys.cap)
  expect_true(mcheck == 1)

  tmp_d <- dose[dose$Stressor == "A", ]
  tmp_d <- tmp_d[1, ]

  # Leave lower and upper limits open
  test_sc <- SystemCapacity(
    f.dose.df = tmp_d,
    f.main.df = sr_wb_dat$main_sheet[sr_wb_dat$main_sheet$Stressors == "A", ],
    f.stressor.df = sr_wb_dat$sr_dat$A,
    f.mean.resp.list = mean.resp.list[[1]],
    n.sims = 10
  )

  mcheck <- mean(test_sc$sys.cap)
  expect_true(all(mcheck < 1 & mcheck > 0))








})
