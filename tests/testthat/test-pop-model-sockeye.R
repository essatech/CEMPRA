test_that("test-pop-model-sockeye", {

  #------------------------------------------------
  # Run some tests for anadromous sockeye
  #------------------------------------------------

  # Sockeye tests
  filename <- system.file("extdata/sockeye/life_cycles_sockeye.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename, stringsAsFactors = FALSE)
  life_cycles <- pop_model_dat_clean(life_cycles)

  # BOOT A VITAL RATE TO GET LAMBDA over 1
  life_cycles$Value[life_cycles$Name == "S0"] <- 0.25

  pop_mod_setup <- pop_model_setup(life_cycles = life_cycles)
  pop_mod_mat <- pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)
  expect_true(pop_mod_mat$life_histories$eps[1] == 2876)
  A <- pop_mod_mat$projection_matrix # See if it looks ok
  lambda <- popbio::lambda(A)
  expect_true(round(lambda, 2) == 1.16) # Should be around this

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


  names(baseline)
  head(baseline$N)

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
  expect_true(mean_spawner < 5000)
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





  # --------------------------------------------------------------------
  # What if spawner capacity K is zero
  # but spawner cap is not listed as a DD constraint in
  # the life cycle parameters file
  # --------------------------------------------------------------------
  n_years <- 500
  spawner_capacity <- 0 # set to zero
  fry_capacity <- 9000000

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
      bh_dd_stages = c("bh_stage_0"), # should be no spawner cap limits
      stage_k_override = stage_k_override,
    )

  dat$stage_names

  spawn_cols <- which(grepl("_B_", dat$stage_names))
  spawners <- baseline$N[1:n_years,c(spawn_cols)]
  spawners <- rowSums(as.data.frame(spawners))
  spawners <- spawners[50:n_years]
  plot(50:n_years, spawners, type = 'l')

  expect_true(median(spawners) > 100)

  # --------------------------------------------------------------------
  # Ok and what if we also introduce a parr capacity
  # --------------------------------------------------------------------

  n_years <- 500
  spawner_capacity <- 0 # set to zero
  # Set this to some super high value so that its meaningless
  fry_capacity <- 9999999999999999999999
  parr_capacity <- 100000

  stage_k_override <- rep(NA, dat$Nstage + 1)
  stage_k_override[1] <- fry_capacity
  stage_k_override[1+1] <- parr_capacity

  baseline <-
    Projection_DD(
      M.mx = M.mx,
      D.mx = D.mx,
      dat = dat,
      K = spawner_capacity,
      Nyears = n_years,
      anadromous = pop_mod_setup$anadromous,
      bh_dd_stages = c("bh_stage_0", "bh_stage_1"), # should be no spawner cap limits
      stage_k_override = stage_k_override,
    )

  spawn_cols <- which(grepl("_B_", dat$stage_names))
  spawners <- baseline$N[1:n_years,c(spawn_cols)]
  spawners <- rowSums(as.data.frame(spawners))
  spawners_fry_parr <- spawners[100:n_years]
  plot(100:n_years, spawners_fry_parr, type = 'l')

  expect_true(median(spawners_fry_parr) > 100)
  expect_true(median(spawners_fry_parr) < 500)

  # --------------------------------------------------------------------
  # Ok - now take away the fry cap
  # we should get the same value as above
  # --------------------------------------------------------------------

  n_years <- 500
  spawner_capacity <- 0 # set to zero
  # Set this to some super high value so that its meaningless
  fry_capacity <- 9999999999999999999999
  parr_capacity <- 100000

  stage_k_override <- rep(NA, dat$Nstage + 1)
  # stage_k_override[1] <- fry_capacity
  stage_k_override[1+1] <- parr_capacity

  baseline <-
    Projection_DD(
      M.mx = M.mx,
      D.mx = D.mx,
      dat = dat,
      K = spawner_capacity,
      Nyears = n_years,
      anadromous = pop_mod_setup$anadromous,
      # bh_dd_stages = c("bh_stage_0", "bh_stage_1"), #
      bh_dd_stages = c("bh_stage_1"),
      stage_k_override = stage_k_override,
    )

  spawn_cols <- which(grepl("_B_", dat$stage_names))
  spawners <- baseline$N[1:n_years,c(spawn_cols)]
  spawners <- rowSums(as.data.frame(spawners))
  spawners_parr <- spawners[100:n_years]
  plot(100:n_years, spawners_parr, type = 'l')

  expect_true(median(spawners_parr) > 100)
  expect_true(median(spawners_parr) < 500)



  # --------------------------------------------------------------------
  # Setup Automated Run with PopulationModel_Run()
  # --------------------------------------------------------------------

  # Stressor Magnitude
  fname <- system.file("extdata/sockeye/sm_sockeye.xlsx", package = "CEMPRA")
  stressor_magnitude <- StressorMagnitudeWorkbook(filename = fname)

  # Stressor Response
  fname <- system.file("extdata/sockeye/sr_sockeye.xlsx", package = "CEMPRA")
  stressor_response <- StressorResponseWorkbook(filename = fname)
  stressor_response$stressor_names

  # Load location and habitat capacities
  filename <- system.file("extdata/sockeye/habitat_capacities_sockeye.csv", package = "CEMPRA")
  habk <- read.csv(filename, stringsAsFactors = FALSE)

  result <- PopulationModel_Run(
    dose = stressor_magnitude,
    sr_wb_dat = stressor_response,
    life_cycle_params = life_cycles,
    HUC_ID = 30007, # one with some non-zero spawners.
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


  # What if all values are blank in habitat capacity file
  filename <- system.file("extdata/sockeye/alt/habitat_capacities_sockeye_all_blanks.csv", package = "CEMPRA")
  habk <- read.csv(filename, stringsAsFactors = FALSE)

  result <- PopulationModel_Run(
    dose = stressor_magnitude,
    sr_wb_dat = stressor_response,
    life_cycle_params = life_cycles,
    HUC_ID = 30007,
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

  # should be a very high number
  expect_true(mean(spawners) > 999999)



  # What if all values are Zero in habitat capacity file
  filename <- system.file("extdata/sockeye/alt/habitat_capacities_sockeye_all_zeros.csv", package = "CEMPRA")
  habk <- read.csv(filename, stringsAsFactors = FALSE)

  result <- PopulationModel_Run(
    dose = stressor_magnitude,
    sr_wb_dat = stressor_response,
    life_cycle_params = life_cycles,
    HUC_ID = 30007,
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

  # should be zero
  expect_true(round(mean(spawners), 0) == 0)



  # What if it is a mix of zeros and blanks in habitat capacity file
  filename <- system.file("extdata/sockeye/alt/habitat_capacities_sockeye_blanks_and_zeros.csv", package = "CEMPRA")
  habk <- read.csv(filename, stringsAsFactors = FALSE)

  result <- PopulationModel_Run(
    dose = stressor_magnitude,
    sr_wb_dat = stressor_response,
    life_cycle_params = life_cycles,
    HUC_ID = 30007,
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

  # non-nonsensical... falls back defaults from compensation ratio
  expect_true(round(mean(spawners), 0) < 1000)



  # What if inputs are all simplified
  filename <- system.file("extdata/sockeye/alt/life_cycle_sockeye_simple.csv", package = "CEMPRA")
  life_cycles_simple <- read.csv(filename, stringsAsFactors = FALSE)

  filename <- system.file("extdata/sockeye/alt/habitat_capacities_sockeye_simple.csv", package = "CEMPRA")
  habk <- read.csv(filename, stringsAsFactors = FALSE)

  result <- PopulationModel_Run(
    dose = stressor_magnitude,
    sr_wb_dat = stressor_response,
    life_cycle_params = life_cycles_simple,
    HUC_ID = 30007,
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

  # non-nonsensical... falls back defaults from compensation ratio
  expect_true(round(mean(spawners), 0) < 50000 && round(mean(spawners), 0) > 5000)



  # Test explainer function
  explanation <- explain_dd_settings(life_cycles = life_cycles_simple,
                      habitat_dd_k = habk,
                      HUC_ID = 30007,
                      verbose = FALSE)

  names(explanation)
  # explanation$explanation
  # explanation$s0_details
  # explanation$summary
  # explanation$compensation_ratios
  # explanation$bh_dd_stages
  # explanation$k_values
  # explanation$warnings








})

