test_that("test-pop-model-nanaimo", {

  #------------------------------------------------
  # Run some tests for the Nanaimo River Application
  #------------------------------------------------

  # Beechie et al 2021 tests
  filename <- system.file("extdata/nanaimo/species_profiles/beechie_2021_chehalis.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename, stringsAsFactors = FALSE)
  pop_mod_setup <- pop_model_setup(life_cycles = life_cycles)
  pop_mod_mat <- pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)
  expect_true(pop_mod_mat$life_histories$eps[1] == 5400)
  A <- pop_mod_mat$projection_matrix # See if it looks ok
  lambda <- popbio::lambda(A)
  expect_true(round(lambda, 2) == 1.85) # Should be around this

  # Honea et al 2009 Tests
  filename <- system.file("extdata/nanaimo/species_profiles/honea_2009_wenatchee.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename, stringsAsFactors = FALSE)
  pop_mod_setup <- pop_model_setup(life_cycles = life_cycles)
  pop_mod_mat <- pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)
  expect_true(pop_mod_mat$life_histories$eps[1] == 1000)
  A <- pop_mod_mat$projection_matrix # See if it looks ok
  lambda <- popbio::lambda(A)
  expect_true(round(lambda, 2) == 2.67) # Should be around this

  # Nicola RAMs Test
  filename <- system.file("extdata/nanaimo/species_profiles/nicola_rams_2021.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename, stringsAsFactors = FALSE)
  pop_mod_setup <- pop_model_setup(life_cycles = life_cycles)
  pop_mod_mat <- pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)
  expect_true(pop_mod_mat$life_histories$eps[1] == 3000)
  A <- pop_mod_mat$projection_matrix # See if it looks ok
  lambda <- popbio::lambda(A)
  expect_true(round(lambda, 2) == 1.55) # Should be around this


  # Nanaimo Composite Test
  filename <- system.file("extdata/nanaimo/species_profiles/nanaimo_comp_ocean_summer.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename, stringsAsFactors = FALSE)
  pop_mod_setup <- pop_model_setup(life_cycles = life_cycles)
  pop_mod_mat <- pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)
  expect_true(pop_mod_mat$life_histories$eps[1] == 1953)
  A <- pop_mod_mat$projection_matrix # See if it looks ok
  lambda <- popbio::lambda(A)
  expect_true(round(lambda, 2) == 1.64) # Should be around this

  # Setup for a manual run of the population model using Projection_DD()

  # Setup for Projection_DD()
  dat <- pop_mod_mat$life_histories
  M.mx <- pop_mod_mat$life_stages_symbolic
  D.mx <- pop_mod_mat$density_stage_symbolic


  # --------------------------------------------------------------------
  # Basic Run with spawner K capacity as the only constraint
  # --------------------------------------------------------------------
  n_years <- 500
  spawner_capacity <- 5000

  stage_k_override <- rep(NA, dat$Nstage + 1)

  baseline <-
    Projection_DD(
      M.mx = M.mx,
      D.mx = D.mx,
      dat = dat,
      K = spawner_capacity,
      Nyears = n_years,
      anadromous = pop_mod_setup$anadromous,
      bh_dd_stages = c("bh_spawners"),
      # bh_dd_stages = c("bh_spawners", "bh_stage_0"),
      stage_k_override = stage_k_override,
    )

  dat$stage_names

  spawn_cols <- which(grepl("_B_", dat$stage_names))
  spawners <- baseline$N[1:n_years,c(spawn_cols)]
  spawners <- rowSums(as.data.frame(spawners))
  spawners <- spawners[50:n_years]
  plot(50:n_years, spawners, type = 'l')

  (mean_spawner <- median(spawners))

  # if we set spawner K to 5000
  # We expect the mean spawner to be between 1000 and 6000
  expect_true(mean_spawner > 1000 & mean_spawner < 6000)
  just_spawn_k <- mean_spawner



  # --------------------------------------------------------------------
  # Basic Run with fry BH K capacity as the only constraint
  # --------------------------------------------------------------------
  n_years <- 500
  spawner_capacity <- 5000
  fry_capacity <- 400000

  stage_k_override <- rep(NA, dat$Nstage + 1)
  stage_k_override[1] <- fry_capacity

  baseline <-
    Projection_DD(
      M.mx = M.mx,
      D.mx = D.mx,
      dat = dat,
      K = spawner_capacity,
      Nyears = n_years,
      anadromous = pop_mod_setup$anadromous,
      # bh_dd_stages = c("bh_spawners", "bh_stage_0"),
      bh_dd_stages = c("bh_stage_0"),
      stage_k_override = stage_k_override,
    )

  dat$stage_names

  spawn_cols <- which(grepl("_B_", dat$stage_names))
  spawners <- baseline$N[1:n_years,c(spawn_cols)]
  spawners <- rowSums(as.data.frame(spawners))
  spawners <- spawners[50:n_years]
  plot(50:n_years, spawners, type = 'l')

  (mean_spawner <- median(spawners))
  # BH we get 13700
  # HS we get 14100


  # if we set spawner K to 5000
  # We expect the mean spawner to be between 1000 and 6000
  expect_true(mean_spawner > 1000 & mean_spawner < 50000)
  just_fry_k <- mean_spawner


  # --------------------------------------------------------------------
  # Basic Run with spawner capacity and fry capacity constraints
  # --------------------------------------------------------------------
  n_years <- 500
  spawner_capacity <- 5000
  fry_capacity <- 400000

  stage_k_override <- rep(NA, dat$Nstage + 1)
  stage_k_override[1] <- fry_capacity

  baseline <-
    Projection_DD(
      M.mx = M.mx,
      D.mx = D.mx,
      dat = dat,
      K = spawner_capacity,
      Nyears = n_years,
      anadromous = pop_mod_setup$anadromous,
      bh_dd_stages = c("bh_spawners", "bh_stage_0"),
      stage_k_override = stage_k_override,
    )

  dat$stage_names

  spawn_cols <- which(grepl("_B_", dat$stage_names))
  spawners <- baseline$N[1:n_years,c(spawn_cols)]
  spawners <- rowSums(as.data.frame(spawners))
  spawners <- spawners[50:n_years]
  plot(50:n_years, spawners, type = 'l')

  (mean_both <- median(spawners))

  expect_true(mean_both > 1000 & mean_both < 50000)
  spawn_and_fry_k <- mean_both

  # Expect runs with both DDs will be less than runs with just one
  # or the other...
  expect_true(just_spawn_k > spawn_and_fry_k)
  expect_true(just_fry_k > spawn_and_fry_k)



  # --------------------------------------------------------------------
  # Setup Automated Run with PopulationModel_Run()
  # --------------------------------------------------------------------

  # Stressor Magnitude
  fname <- system.file("extdata/nanaimo/stressor_magnitude_nanaimo.xlsx", package = "CEMPRA")
  stressor_magnitude <- StressorMagnitudeWorkbook(filename = fname)

  # Stressor Response
  fname <- system.file("extdata/nanaimo/stressor_response_nanaimo.xlsx", package = "CEMPRA")
  stressor_response <- StressorResponseWorkbook(filename = fname)
  stressor_response$stressor_names

  # Load location and habitat capacities
  filename <- system.file("extdata/nanaimo/habitat_capacities_nanaimo.csv", package = "CEMPRA")
  habk <- read.csv(filename, stringsAsFactors = FALSE)

  result <- PopulationModel_Run(
    dose = stressor_magnitude,
    sr_wb_dat = stressor_response,
    life_cycle_params = life_cycles,
    HUC_ID = 1, # FALL LOWER
    n_years = 200,
    MC_sims = 3,
    stressors = NA, # c("Spawn_Gravel", "Spawn_Quality", "Fry_Capacity"),
    habitat_dd_k = habk
  )

  spawn_cols <- which(grepl("_B_", dat$stage_names))
  spawners <- result$ce[[1]]$N[1:200,c(spawn_cols)]
  spawners <- rowSums(as.data.frame(spawners))
  spawners <- spawners[50:200]
  print(round(median(spawners), 0)) #3769 ... 1396
  plot(spawners, type = 'l')


  result <- PopulationModel_Run(
    dose = stressor_magnitude,
    sr_wb_dat = stressor_response,
    life_cycle_params = life_cycles,
    HUC_ID = 2,  ### SUMMER LOWER
    n_years = 200,
    MC_sims = 3,
    stressors = NA,
    habitat_dd_k = habk
  )

  spawn_cols <- which(grepl("_B_", dat$stage_names))
  spawners <- result$ce[[1]]$N[1:200,c(spawn_cols)]
  spawners <- rowSums(as.data.frame(spawners))
  spawners <- spawners[50:200]
  print(round(median(spawners), 0)) #3769 ... 1396
  plot(spawners, type = 'l')

  result <- PopulationModel_Run(
    dose = stressor_magnitude,
    sr_wb_dat = stressor_response,
    life_cycle_params = life_cycles,
    HUC_ID = 2,  ### SUMMER - LOWER
    n_years = 200,
    MC_sims = 3,
    stressors = NA,
    habitat_dd_k = habk
  )

  spawn_cols <- which(grepl("_B_", dat$stage_names))
  spawners <- result$ce[[1]]$N[1:200,c(spawn_cols)]
  spawners <- rowSums(as.data.frame(spawners))
  spawners <- spawners[50:200]
  print(round(median(spawners), 0)) #3769 ... 1396
  plot(spawners, type = 'l')


})

