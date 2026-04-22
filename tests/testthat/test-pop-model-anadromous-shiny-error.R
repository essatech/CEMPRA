test_that("test-pop-model-anadromous-shiny-error", {

  # Test errors found in Shiny App

  #----------------------------------------------
  # Setting survivorship to zero crashes app
  #----------------------------------------------
  filename <- system.file("extdata/simple_test/test_shiny_errors/TEST1_zero_surv.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename, stringsAsFactors = FALSE)
  pop_mod_setup <- pop_model_setup(life_cycles = life_cycles)
  pop_mod_mat <- pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)
  expect_true(pop_mod_mat$life_histories$eps[1] == 3000) # This must be equal to 3000
  A <- pop_mod_mat$projection_matrix
  lambda <- popbio::lambda(A)
  expect_true(round(lambda, 2) == 0.58) # Manual check - might be wrong


  # Setup for Projection_DD()
  dat <- pop_mod_mat$life_histories
  M.mx <- pop_mod_mat$life_stages_symbolic
  D.mx <- pop_mod_mat$density_stage_symbolic

  CE_df <- data.frame(HUC = 1, Stressor = 1, dose = 1, sys.cap = 0.5, life_stage = "spawners", parameter = "survival", Stressor_cat = 1)



  M.mx = M.mx
  D.mx = D.mx
  H.mx = NULL
  dat = dat
  K = dat$Ka
  Nyears = 50
  p.cat = NA
  CE_df = CE_df
  K_adj = FALSE
  stage_k_override = NULL
  bh_dd_stages = NULL
  anadromous = pop_mod_setup$anadromous
  # alt_return = NULL



  # Basic Run
  baseline <-
    Projection_DD(
      M.mx = M.mx,
      D.mx = D.mx,
      H.mx = NULL,
      dat = dat,
      K = dat$Ka,
      Nyears = 50,
      p.cat = NA,
      CE_df = CE_df,
      K_adj = FALSE,
      stage_k_override = NULL,
      bh_dd_stages = NULL,
      anadromous = pop_mod_setup$anadromous,
      # alt_return = NULL
    )


  dat$stage_names
  # Should act like even and odd year pink
  # plot(baseline$N[20:100,2], type = 'l')
  # summary(baseline$N[,2])
  diff <- abs(1 - median(baseline$pop$N) / dat$Ka)
  # expect_true(diff < 0.2) # Should roughly match Ka


})

