test_that("Basic population model runs ok", {


  filename_lc <- system.file("extdata", "life_cycles.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename_lc)
  #print(life_cycles)

  expect_true(nrow(life_cycles) > 5)
  expect_true(class(life_cycles) == "data.frame")
  expect_true(class(life_cycles$Value) == "numeric")
  expect_true(!(any(is.na(life_cycles$Value))))


  # Setup objects for population model
  pop_mod_setup <- pop_model_setup(life_cycles = life_cycles)



  # Check out symbolic matrix representations
  # For density
  ds <- pop_mod_setup$density_stage_symbolic
  ds_m <- matrix(as.character(ds), nrow = 4, ncol = 4)
  #t(ds_m)

  # For life stages
  lss <- pop_mod_setup$life_stages_symbolic
  lss_m <- matrix(as.character(lss), nrow = 4, ncol = 4)
  #t(lss_m)



  expect_true(pop_mod_setup$possible_error_state == "All Good")
  expect_true(class(pop_mod_setup$density_stage_symbolic) == "expression")
  expect_true(pop_mod_setup$Nstage == 4)
  expect_true(class(pop_mod_setup$projection_matrix)[1] == "matrix")


  # Build matrix elements for population model
  pop_mod_mat <- pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)
  #names(pop_mod_mat)
  #print(pop_mod_mat$projection_matrix)

  # Preview density-independent transition projection_matrix
  A <- pop_mod_mat$projection_matrix
  # Assign nicknames for each stage
  snames <- c("egg_yoy", "juv", "subadult", "adult")
  rownames(A) <- colnames(A) <- snames
  # Simple density-independent lambda estimate
  lambda <- popbio::lambda(A)


  expect_true(class(lambda) == "numeric")
  expect_true(lambda > 0)



  # Set the K.adj (K adjustment prior to pop model run)
  life_histories <- pop_mod_mat$life_histories
  # Mathematical expression of the transition matrix
  life_stages_symbolic <- pop_mod_mat$life_stages_symbolic
  # Mathematical expression of the density matrix
  density_stage_symbolic <- pop_mod_mat$density_stage_symbolic

  # Run simple population projection - project forward through time
  baseline <-
    Projection_DD(
      M.mx = life_stages_symbolic,
      # projection matrix expression
      D.mx = density_stage_symbolic,
      # density-dependence matrix
      H.mx = NULL,
      dat = life_histories,
      # life history data
      K = life_histories$Ka,
      # initial pop size as stage-structure vector
      Nyears = 100,
      # years to run simulation
      p.cat = 0,      # Probability of catastrophe
      CE_df = NULL
    )

  expect_true(!(any(is.na(baseline$pop$N))))
  expect_true(length(baseline$pop$N) == 101)

  # stable
  expect_true(round(mean(baseline$lambdas), 0) == 1)



  # and two stressors (My Stressor1, My Stressor2)
  CE_df1 <- data.frame(HUC = 123,
                       Stressor = "My Stressor1",
                       dose = 123,      # Stressor magnitude
                       sys.cap = 0.82,  # Effect of vital rate (dose:response)
                       life_stage = "fry_parr",
                       parameter = "capacity",
                       Stressor_cat = "My Stressor1")
  # Second stressor - fake data
  CE_df2 <- data.frame(HUC = 123,
                       Stressor = "My Stressor2",
                       dose = 123,
                       sys.cap = 0.95,
                       life_stage = "all_juv",
                       parameter = "survival",
                       Stressor_cat = "My Stressor2")
  # Stressors dataframe
  CE_df <- rbind(CE_df1, CE_df2)

  ce_pop <- Projection_DD(M.mx = life_stages_symbolic,
                          D.mx = density_stage_symbolic,
                          H.mx = NULL,
                          dat = life_histories,
                          K = life_histories$Ka,
                          Nyears = 100,
                          p.cat = 0,
                          CE_df = CE_df
  )

  expect_true(!(any(is.na(ce_pop$pop$N))))
  expect_true(length(ce_pop$pop$N) == 101)

  # stable
  expect_true(round(mean(ce_pop$lambdas), 0) == 1)






  # Set some values to zero...
  # Second stressor - fake data
  CE_df2 <- data.frame(HUC = 123,
                       Stressor = "My Stressor2",
                       dose = 123,
                       sys.cap = 0,
                       life_stage = "all_juv",
                       parameter = "survival",
                       Stressor_cat = "My Stressor2")
  # Stressors dataframe
  CE_df <- rbind(CE_df1, CE_df2)

  ce_pop <- Projection_DD(M.mx = life_stages_symbolic,
                          D.mx = density_stage_symbolic,
                          H.mx = NULL,
                          dat = life_histories,
                          K = life_histories$Ka,
                          Nyears = 100,
                          p.cat = 0,
                          CE_df = CE_df
  )

  expect_true(!(any(is.na(ce_pop$pop$N))))
  expect_true(length(ce_pop$pop$N) == 101)

  # stable
  expect_true(round(mean(ce_pop$lambdas), 0) == 1)


})
