test_that("test-kw-pop-model-nicola", {

  #----------------------------------------------
  # Test Kyle Wilsons Nicola Code for Comparison
  #----------------------------------------------
  filename_lc <- system.file("extdata/species_profiles/chinook_life_cycles_KW.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename_lc)
  # print(life_cycles)

  expect_true(nrow(life_cycles) > 5)
  expect_true(class(life_cycles$Value) == "numeric")
  expect_true(!(any(is.na(life_cycles$Value))))

  # Setup objects for population model
  pop_mod_setup <- pop_model_setup(life_cycles = life_cycles)

  expect_true(pop_mod_setup$possible_error_state == "All Good")

  # Build matrix elements for population model
  pop_mod_mat <- pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)
  # print(pop_mod_mat$projection_matrix)

  # Preview density-independent transition projection_matrix
  A <- pop_mod_mat$projection_matrix
  # Assign nicknames for each stage
  n_stage <- life_cycles$Value[life_cycles$Name == "Nstage"]
  snames <- paste0("stage_", 1:n_stage)

  rownames(A) <- colnames(A) <- snames
  # Simple density-independent lambda estimate
  (lambda <- popbio::lambda(A))

  # Set the K.adj (K adjustment prior to pop model run)
  life_histories <- pop_mod_mat$life_histories
  # Mathematical expression of the transition matrix
  life_stages_symbolic <- pop_mod_mat$life_stages_symbolic
  # Mathematical expression of the density matrix
  density_stage_symbolic <- pop_mod_mat$density_stage_symbolic

  # --------------------------------------------------------------------
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
      Nyears = 500,
      # years to run simulation
      p.cat = 0,      # Probability of catastrophe
      CE_df = NULL,
      stage_k_override = NULL,
      bh_dd_stages = NULL
    )

  names(baseline)
  df <- baseline$pop
  df <- df[df$year > 50, ]
  plot(df$year, df$N, type = 'l')
  expect_true(median(df$N) > 0)
  df <- baseline$N
  colMeans(df)
  tail(df)



})
