test_that("test-pop-model-density", {

  expect_true(1 == 1)

  #---------------------------------------------------
  # Unconstrained DD
  # set all CR values to 1.0
  #---------------------------------------------------

  # Test density independent growth
  filename_lc <-
    system.file("extdata", "./species_profiles/life_cycles_no_stochasticity.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename_lc)

  quick_test <- function(life_cycles) {
    pop_mod_setup <- pop_model_setup(life_cycles = life_cycles)
    pop_mod_mat <-
      pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)
    life_histories <- pop_mod_mat$life_histories
    life_stages_symbolic <- pop_mod_mat$life_stages_symbolic
    density_stage_symbolic <- pop_mod_mat$density_stage_symbolic
    baseline <-
      Projection_DD(
        M.mx = life_stages_symbolic,
        D.mx = density_stage_symbolic,
        H.mx = NULL,
        dat = life_histories,
        K = life_histories$Ka,
        Nyears = 100,
        p.cat = 0,
        CE_df = NULL
      )
    df <- baseline$pop
    return(df)
  }

  # DI growth
  life_cycles$Value[life_cycles$Name == "k"] <- NA
  life_cycles$Value[life_cycles$Name == "eps"] <- 900000
  df1 <- quick_test(life_cycles)
  # plot(df1$year[1:10], df1$N[1:10], type = 'l')


  # Test with DD growth to 100
  life_cycles$Value[life_cycles$Name == "k"] <- 100
  df2 <- quick_test(life_cycles)
  #plot(df2$year[1:100], df2$N[1:100], type = 'l')

  # Test with DD growth to 500
  life_cycles$Value[life_cycles$Name == "k"] <- 500
  df3 <- quick_test(life_cycles)
  #plot(df3$year[1:100], df3$N[1:100], type = 'l')

  expect_true(all(round(tail(df2$N), 0) == 100))
  expect_true(all(round(tail(df3$N), 0) == 500))
  expect_true(all(tail(df1$N) > tail(df3$N)))
  expect_true(all(tail(df1$N) > tail(df2$N)))



  #---------------------------------------------------
  # Constrained DD
  # By adult capacity (last stage) and CR values
  #---------------------------------------------------


  # Look at DD growth with CR values
  life_cycles$Value[life_cycles$Name == "k"] <- 200
  life_cycles$Value[life_cycles$Name == "eps"] <- 10000
  life_cycles$Value[life_cycles$Name == "SE"] <- 0.3
  life_cycles$Value[life_cycles$Name == "S0"] <- 0.3
  life_cycles$Value[life_cycles$Name == "surv_1"] <- 0.3
  life_cycles$Value[life_cycles$Name == "surv_2"] <- 0.3
  life_cycles$Value[life_cycles$Name == "surv_3"] <- 0.3
  life_cycles$Value[life_cycles$Name == "surv_4"] <- 0.3
  life_cycles$Value[life_cycles$Name == "cr_4"]    <- 3
  life_cycles$Value[life_cycles$Name == "cr_3"]    <- 3
  life_cycles$Value[life_cycles$Name == "cr_2"]    <- 3
  life_cycles$Value[life_cycles$Name == "cr_1"]    <- 3
  life_cycles$Value[life_cycles$Name == "cr_0"]    <- 3
  life_cycles$Value[life_cycles$Name == "cr_E"]    <- 3
  life_cycles$Value[life_cycles$Name == "M.cv"]    <- 0
  life_cycles$Value[life_cycles$Name == "eps_sd" ] <- 0
  df4 <- quick_test(life_cycles)
  #plot(df4$year[1:100], df4$N[1:100], type = 'l')

  expect_true(all(round(tail(df4$N)) == 200))







  #-----------------------------------------------------------
  # Constrained DD
  # By stage-specific location habitat values and Hockey Stick
  #-----------------------------------------------------------

  filename_lc <-
    system.file("extdata", "./species_profiles/life_cycles_no_stochasticity.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename_lc)

  quick_test <- function(life_cycles, bh_dd_stages = NULL, stage_k_override = NULL) {
    pop_mod_setup <- pop_model_setup(life_cycles = life_cycles)
    pop_mod_mat <-
      pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)
    life_histories <- pop_mod_mat$life_histories
    life_stages_symbolic <- pop_mod_mat$life_stages_symbolic
    density_stage_symbolic <- pop_mod_mat$density_stage_symbolic
    baseline <-
      Projection_DD(
        M.mx = life_stages_symbolic,
        D.mx = density_stage_symbolic,
        H.mx = NULL,
        dat = life_histories,
        K = NA,
        Nyears = 250,
        p.cat = 0,
        CE_df = NULL,
        bh_dd_stages = bh_dd_stages,
        stage_k_override = stage_k_override
      )
    df <- baseline$pop
    return(df)
  }

  # DI growth
  life_cycles$Value[life_cycles$Name == "k"] <- NA
  life_cycles$Value[life_cycles$Name == "eps"] <- 3000000
  df1 <- quick_test(life_cycles, bh_dd_stages = NULL) # DI
  df2 <- quick_test(life_cycles, bh_dd_stages = c("bh_stage_4")) # DD

  plot(df1$year[1:10], df1$N[1:10], type = 'l')
  plot(df2$year[1:10], df2$N[1:10], type = 'l')

  # Expect df1 to go to infinity..
  expect_true(tail(df1$N)[1] > tail(df2$N)[1])

  # Expect to run without error
  life_cycles$Value[life_cycles$Name == "k"] <- 500
  df1 <- quick_test(life_cycles, bh_dd_stages = c("dd_hs_0", "bh_stage_1", "bh_stage_2", "bh_stage_3"))

  # s0 still being adjusted will fix with system wide k values
  cso <- rep(NA, 5); cso[[2]] <- 1000000; cso[[3]] <- 1000
  df2 <- quick_test(life_cycles, stage_k_override = cso, bh_dd_stages = c("bh_stage_1", "bh_stage_2"))
  plot(df2$year[1:10], df2$N[1:10], type = 'l')






  #-----------------------------------------------------------
  # Test clean 5-stage projection with DD
  #-----------------------------------------------------------

  filename_lc <-
    system.file("extdata", "./species_profiles/life_cycles_test_dd.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename_lc)

  quick_test <- function(life_cycles, bh_dd_stages = NULL, stage_k_override = NULL) {
    pop_mod_setup <- pop_model_setup(life_cycles = life_cycles)
    pop_mod_mat <-
      pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)
    life_histories <- pop_mod_mat$life_histories
    life_stages_symbolic <- pop_mod_mat$life_stages_symbolic
    density_stage_symbolic <- pop_mod_mat$density_stage_symbolic
    baseline <-
      Projection_DD(
        M.mx = life_stages_symbolic,
        D.mx = density_stage_symbolic,
        H.mx = NULL,
        dat = life_histories,
        K = NA,
        Nyears = 250,
        p.cat = 0,
        CE_df = NULL,
        bh_dd_stages = bh_dd_stages,
        stage_k_override = stage_k_override
      )
    df <- baseline$N
    return(df)
  }


  # DD growth
  life_cycles$
  life_cycles$Value[life_cycles$Name == "k"] <- NA
  stage_k_override <- c(10000, NA, NA, NA, NA, NA)
  length(stage_k_override)
  df2 <- quick_test(life_cycles, bh_dd_stages = c("dd_hs_0"), stage_k_override = stage_k_override) # DD
  plot(df2[, 4], type = 'l', ylim = c(0, 12000))
  tail(df2)

  expect_true(median(df2[, 1]) == 10000)


  # DD growth
  life_cycles$
    life_cycles$Value[life_cycles$Name == "k"] <- NA
  stage_k_override <- c(1000000000, 900000, 10000, NA, NA, 90000000)
  length(stage_k_override)
  df2 <- quick_test(life_cycles, bh_dd_stages = c("dd_hs_0", "bh_stage_1"), stage_k_override = stage_k_override) # DD
  plot(df2[, 1], type = 'l', ylim = c(0, 120000))
  plot(df2[, 2], type = 'l', ylim = c(0, 100000))
  expect_true(median(df2[, 1]) < 900000)
  expect_true(median(df2[, 2]) < 900000)









  #-----------------------------------------------------------
  # Plot BH Curve with no errors
  #-----------------------------------------------------------

  # Plot BH Curve
  alpha <- 0.7; k = 100; mseq <- seq(0, 1000, by = 1)
  res <-
    eval(
      bh_stage_f(),
      list(
        alpha = alpha,
        k = k,
        Nt1 = mseq
      )
    )
  plot(mseq, res, type = 'l', xlab = "Nt", ylab = "Nt+1", ylim = c(0, k*1.1))
  abline(0, alpha, lty = 2, col = "red")
  abline(h=k, lty = 2, col = "blue")
  # abline(h=k*alpha, lty = 2, col = "green")





})
