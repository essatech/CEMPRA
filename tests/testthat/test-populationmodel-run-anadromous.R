test_that("test-populationmodel-run-anadromous", {

  #----------------------------------------------------------
  # Test the 3X3 anadromous matrix with PopulationModel_Run()
  #----------------------------------------------------------

  # two stage with maturation at age-3
  filename <- system.file("extdata/simple_test/test_anadromous/stage_3_anadromous.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename)

  # Load stressor-magnitude (sm) data file
  filename_sm <- system.file("extdata", "./matrix_test/stressor_magnitude_matrix.xlsx", package = "CEMPRA")
  dose <- StressorMagnitudeWorkbook(filename = filename_sm)
  # Load stressor-response (sr) data file
  filename_sr <- system.file("extdata", "./matrix_test/stressor_response_matrix_AB.xlsx", package = "CEMPRA")
  sr_wb_dat <- StressorResponseWorkbook(filename = filename_sr)

  # head(dose)
  # names(sr_wb_dat)
  # head(life_cycles)

  # Load in the habitat data optional
  data <- PopulationModel_Run(
    dose = dose,
    sr_wb_dat = sr_wb_dat,
    life_cycle_params = life_cycles,
    HUC_ID = 2, # HUC ID 2
    n_years = 50,
    MC_sims = 5,
    stressors = NA  # Run with all stressors
  )
  # Expect this to just run without errors

  names(data)
  length(data$ce)
  length(data$baseline)
  length(data$MC_sims)
  expect_true(1 == 1)


  #----------------------------------------------------------
  # Test Nicola Example
  #----------------------------------------------------------

  # Life Cycles File
  filename_lc <- system.file("extdata", "./simple_test/nicola_test/20240926_lcm/chinook_life_cycle_profile.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename_lc)

  # Load stressor-magnitude (sm) data file
  filename_sm <- system.file("extdata", "./simple_test/nicola_test/20240926_lcm/stressor_magnitude.xlsx", package = "CEMPRA")
  dose <- StressorMagnitudeWorkbook(filename = filename_sm)
  # Filter for only the base-case scenario
  dose <- dose[dose$base == TRUE, ]
  head(dose)
  dose$SD <- 0

  # Load stressor-response (sr) data file
  filename_sr <- system.file("extdata", "./simple_test/nicola_test/20240926_lcm/stressor_response_no_stochast.xlsx", package = "CEMPRA")
  sr_wb_dat <- StressorResponseWorkbook(filename = filename_sr)

  # Location-habitat DD
  filename_hab <- system.file("extdata", "./simple_test/nicola_test/20240926_lcm/habitat_dd_k_nicola.xlsx", package = "CEMPRA")
  habitat_dd_k <- readxl::read_excel(filename_hab, sheet = 1)

  data <- PopulationModel_Run(
    dose = dose,
    sr_wb_dat = sr_wb_dat,
    life_cycle_params = life_cycles,
    HUC_ID = 1, # HUC ID 2
    n_years = 100,
    MC_sims = 3,
    stressors = NA,
    habitat_dd_k = habitat_dd_k
  )

  names(data)
  plot(data$ce[[1]]$pop, type = 'l', ylim = c(0, 300), col = "red")
  points(data$baseline[[2]]$pop, type = 'l', col = "green")

  # Expect CE to do worse than baseline
  expect_true(mean(data$ce[[2]]$pop$N) < mean(data$baseline[[2]]$pop$N))

  # ------------------------------------------------------
  # TEST: Turn off some stressors (fines) and compare
  # ------------------------------------------------------
  unique(dose$Stressor)

  data_b <- PopulationModel_Run(
    dose = dose,
    sr_wb_dat = sr_wb_dat,
    life_cycle_params = life_cycles,
    HUC_ID = 1,
    # HUC ID 2
    n_years = 100,
    MC_sims = 3,
    stressors = c( # excluded fines.
      "Cap Rearing",
      "Cap Spawning",
      "MWAT Prespawn",
      "MWAT Rearing"
    ),
    habitat_dd_k = habitat_dd_k
  )

  names(data)
  plot(data$ce[[1]]$pop, type = 'l', ylim = c(0, 300), col = "red")
  points(data$baseline[[2]]$pop, type = 'l', col = "green")

  mmean_a_1 <- mean(data$ce[[1]]$pop$N)
  mmean_a_2 <- mean(data$ce[[2]]$pop$N)
  mmean_a_3 <- mean(data$ce[[3]]$pop$N)
  mmean_a <- mean(c(mmean_a_1, mmean_a_2, mmean_a_3))

  mmean_b_1 <- mean(data_b$ce[[1]]$pop$N)
  mmean_b_2 <- mean(data_b$ce[[2]]$pop$N)
  mmean_b_3 <- mean(data_b$ce[[3]]$pop$N)
  mmean_b <- mean(c(mmean_b_1, mmean_b_2, mmean_b_3))

  expect_true(mmean_a < mmean_b)


  # ------------------------------------------------------
  # TEST: Relax Spawner DD
  # ------------------------------------------------------
  unique(dose$Stressor)
  life_cycles_c <- life_cycles[!(life_cycles$Name == "bh_spawners"), ]
  habitat_dd_k_c <- habitat_dd_k
  habitat_dd_k_c$k_stage_B_mean <- NULL

  data_c <- PopulationModel_Run(
    dose = dose,
    sr_wb_dat = sr_wb_dat,
    life_cycle_params = life_cycles_c,
    HUC_ID = 1,
    # HUC ID 2
    n_years = 100,
    MC_sims = 3,
    stressors = c(
      "Cap Rearing",
      "Cap Spawning",
      "MWAT Prespawn",
      "MWAT Rearing"
    ),
    habitat_dd_k = habitat_dd_k_c
  )

  mmean_b_1 <- mean(data_b$ce[[1]]$pop$N)
  mmean_b_2 <- mean(data_b$ce[[2]]$pop$N)
  mmean_b_3 <- mean(data_b$ce[[3]]$pop$N)
  mmean_b <- mean(c(mmean_b_1, mmean_b_2, mmean_b_3))

  mmean_c_1 <- mean(data_c$ce[[1]]$pop$N)
  mmean_c_2 <- mean(data_c$ce[[2]]$pop$N)
  mmean_c_3 <- mean(data_c$ce[[3]]$pop$N)
  mmean_c <- mean(c(mmean_c_1, mmean_c_2, mmean_c_3))

  # spawner DD is relaced, so expect C to do better than B
  expect_true(mmean_b < mmean_c)



  # ------------------------------------------------------
  # TEST: Relax both egg to fry and fry to parr DD
  # ------------------------------------------------------
  unique(dose$Stressor)
  life_cycles_c <- life_cycles[!(life_cycles$Name == "bh_stage_1"), ]
  life_cycles_c <- life_cycles_c[!(life_cycles_c$Name == "bh_stage_0"), ]

  habitat_dd_k_c <- habitat_dd_k
  habitat_dd_k_c$k_stage_0_mean <- NULL
  habitat_dd_k_c$k_stage_Pb_1_mean <- NULL

  data_d <- PopulationModel_Run(
    dose = dose,
    sr_wb_dat = sr_wb_dat,
    life_cycle_params = life_cycles_c,
    HUC_ID = 1,
    # HUC ID 2
    n_years = 100,
    MC_sims = 3,
    stressors = c(
      "Cap Rearing",
      "Cap Spawning",
      "MWAT Prespawn",
      "MWAT Rearing"
    ),
    habitat_dd_k = habitat_dd_k_c
  )

  mmean_d_1 <- mean(data_d$ce[[1]]$pop$N)
  mmean_d_2 <- mean(data_d$ce[[2]]$pop$N)
  mmean_d_3 <- mean(data_d$ce[[3]]$pop$N)
  mmean_d <- mean(c(mmean_d_1, mmean_d_2, mmean_d_3))

  # fry DD is relaxed, so expect C to do better than B
  # MIGHT NOT DO ANYTHING....?
  expect_true(mmean_b <= mmean_d)


  # Cap rearing when no fry cap
  data_e <- PopulationModel_Run(
    dose = dose,
    sr_wb_dat = sr_wb_dat,
    life_cycle_params = life_cycles_c,
    HUC_ID = 1,
    # HUC ID 2
    n_years = 100,
    MC_sims = 3,
    stressors = c(
      "Cap Spawning",
      "MWAT Prespawn"
    ),
    habitat_dd_k = habitat_dd_k_c
  )

  mmean_e_1 <- mean(data_e$ce[[1]]$pop$N)
  mmean_e_2 <- mean(data_e$ce[[2]]$pop$N)
  mmean_e_3 <- mean(data_e$ce[[3]]$pop$N)
  mmean_e <- mean(c(mmean_e_1, mmean_e_2, mmean_e_3))

  expect_true(mmean_b <= mmean_e)




})

