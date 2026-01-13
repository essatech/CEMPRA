test_that("test-pop-model-stage-1-dd", {

  # Life cycle and habitat input
  filename_lc <- system.file("extdata/simple_test/test_stage_1_k/lcm_stage_pb_1_check.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename_lc)

  # Habitat capacities
  filename_lc <- system.file("extdata/simple_test/test_stage_1_k/hk_stage_pb_1_check.csv", package = "CEMPRA")
  hab_k <- read.csv(filename_lc)


  fname <- system.file("extdata/simple_test/test_stage_1_k/sm.xlsx", package = "CEMPRA")
  stressor_magnitude <- StressorMagnitudeWorkbook(filename = fname)

  # Stressor Response
  fname <- system.file("extdata/simple_test/test_stage_1_k/sr.xlsx", package = "CEMPRA")
  stressor_response <- StressorResponseWorkbook(filename = fname)


  pop_mod_setup <- pop_model_setup(life_cycles = life_cycles)
  pop_mod_mat <- pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)
  dat <- pop_mod_mat$life_histories

  explain_dd_settings(life_cycles = life_cycles,
                                  habitat_dd_k = hab_k,
                                  HUC_ID = 1,
                                  verbose = FALSE)

  result <- PopulationModel_Run(dose = stressor_magnitude,
                      sr_wb_dat = stressor_response,
                      life_cycle_params = life_cycles,
                      HUC_ID = 1,
                      n_years = 200,
                      MC_sims = 1,
                      habitat_dd_k = hab_k)

  names(result)
  length(result$ce)

  spawn_cols <- which(grepl("_B_", dat$stage_names))
  spawners <- result$ce[[1]]$N[100:200,c(spawn_cols)]
  spawners <- rowSums(as.data.frame(spawners))
  print(round(median(spawners), 0)) #3769 ... 1396
  plot(spawners, type = 'l')

  # Try again but inflate stage 1 abundance 10X
  hab_k$k_stage_Pb_1_mean <- hab_k$k_stage_Pb_1_mean * 10

  result <- PopulationModel_Run(dose = stressor_magnitude,
                                sr_wb_dat = stressor_response,
                                life_cycle_params = life_cycles,
                                HUC_ID = 1,
                                n_years = 200,
                                MC_sims = 1,
                                habitat_dd_k = hab_k)

  names(result)
  length(result$ce)

  spawn_cols <- which(grepl("_B_", dat$stage_names))
  spawners10 <- result$ce[[1]]$N[100:200,c(spawn_cols)]
  spawners10 <- rowSums(as.data.frame(spawners10))
  print(round(median(spawners10), 0)) #3769 ... 1396
  plot(spawners10, type = 'l')

  expect_true({ (median(spawners10)) > (median(spawners) * 5) })


  # ----------------------------------------------------------------------------
  # Try only with stage pd_2
  # ----------------------------------------------------------------------------
  # Habitat capacities
  filename_lc <- system.file("extdata/simple_test/test_stage_1_k/hk_stage_pb_2_check.csv", package = "CEMPRA")
  hab_k <- read.csv(filename_lc)
  # Life cycle and habitat input
  filename_lc <- system.file("extdata/simple_test/test_stage_1_k/lcm_stage_pb_2_check.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename_lc)

  result <- PopulationModel_Run(dose = stressor_magnitude,
                                sr_wb_dat = stressor_response,
                                life_cycle_params = life_cycles,
                                HUC_ID = 1,
                                n_years = 200,
                                MC_sims = 1,
                                habitat_dd_k = hab_k)

  names(result)
  length(result$ce)

  spawn_cols <- which(grepl("_B_", dat$stage_names))
  spawners <- result$ce[[1]]$N[100:200,c(spawn_cols)]
  spawners <- rowSums(as.data.frame(spawners))
  print(round(median(spawners), 0)) #3769 ... 1396
  plot(spawners, type = 'l')

  hab_k$k_stage_Pb_2_mean <- hab_k$k_stage_Pb_2_mean * 10

  result <- PopulationModel_Run(dose = stressor_magnitude,
                                sr_wb_dat = stressor_response,
                                life_cycle_params = life_cycles,
                                HUC_ID = 1,
                                n_years = 200,
                                MC_sims = 1,
                                habitat_dd_k = hab_k)

  names(result)
  length(result$ce)

  spawn_cols <- which(grepl("_B_", dat$stage_names))
  spawners10 <- result$ce[[1]]$N[100:200,c(spawn_cols)]
  spawners10 <- rowSums(as.data.frame(spawners10))
  print(round(median(spawners10), 0)) #3769 ... 1396
  plot(spawners10, type = 'l')



  expect_true({ (median(spawners10)) > (median(spawners) * 5) })






})
