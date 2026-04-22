test_that("Five-stage pop model works", {

  #----------------------------------------------
  # Create a 5-stage population model for coho
  filename_lc <- system.file("extdata/species_profiles", "life_cycles_sockeye_Spromberg_Meador_2006.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename_lc)
  # print(life_cycles)


  life_cycles$Value[life_cycles$Name == "SE"] <- 0.9
  life_cycles$Value[life_cycles$Name == "S0"] <- 0.98


  expect_true(nrow(life_cycles) > 5)
  expect_true(class(life_cycles$Value) == "numeric")
  expect_true(!(any(is.na(life_cycles$Value))))


  # Setup objects for population model
  pop_mod_setup <- pop_model_setup(life_cycles = life_cycles)
  expect_true(pop_mod_setup$possible_error_state == "All Good")

  pop_mod_setup$projection_matrix
  pop_mod_setup$life_histories$S
  pop_mod_setup$life_histories$Surv_annual


  # Build matrix elements for population model
  pop_mod_mat <- pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)
  # print(pop_mod_mat$projection_matrix)

  names(pop_mod_mat)
  pop_mod_mat$projection_matrix
  pop_mod_mat$life_histories$s0.1.det
  pop_mod_mat$life_histories$S
  pop_mod_mat$life_histories$Surv_annual



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



  # Dive into Projection_DD() function
  if(FALSE) {
    M.mx = life_stages_symbolic
    D.mx = density_stage_symbolic
    H.mx = NULL
    dat = life_histories
    K = life_histories$Ka
    K_adj = FALSE
    Nyears = 500
    p.cat = 0
    CE_df = NULL
    stage_k_override = c(NA, 100000, 100000, 10000, 10000, 10000)
    bh_dd_stages = c("bh_stage_1")
  }



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
      stage_k_override = c(NA, 100000, 100000, 10000, 10000, 10000),
      bh_dd_stages = c("bh_stage_1")
    )

  names(baseline)
  df <- baseline$pop
  plot(df$year, df$N, type = 'l')
  expect_true(median(df$N) > 0)
  df <- baseline$N
  colMeans(df)
  tail(df)




})
