test_that("test-build-k-for-proj-dd", {

  #------------------------------------------------
  # Make sure build_k_for_proj_dd()
  # is working correctly
  #------------------------------------------------

  filename <- system.file("extdata/simple_test/test_build_k_for_dd/test_01_life_cycles.csv",
                          package = "CEMPRA")

  life_cycle_params <- read.csv(filename, stringsAsFactors = FALSE)

  # Clean inputs
  life_cycle_params <- pop_model_dat_clean(life_cycle_params)

  # Run basic model setup
  pop_mod_setup <- pop_model_setup(life_cycles = life_cycle_params)
  pop_mod_mat <- pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)

  # Get the life histories object
  life_histories <- pop_mod_mat$life_histories

  # Habitat capacities
  habitat_dd_k <-

    filename <- system.file("extdata/simple_test/test_build_k_for_dd/test_01_dd_cap_table.csv", package = "CEMPRA")

  habitat_dd_k <- read.csv(filename, stringsAsFactors = FALSE)
  habitat_dd_k <- pop_model_hab_dens_clean(hab_dens = habitat_dd_k)

  HUC_ID <- 1

  # Return object
  ro <- CEMPRA::build_k_for_proj_dd(
    habitat_dd_k = habitat_dd_k,
    HUC_ID = 1,
    life_histories = life_histories,
    life_cycle_params = life_cycle_params
  )

  expect_true({ ro$ret_total_anadromous_spawners == 10 })
  expect_true({ all(ro$ret_bh_dd_stages == c("bh_stage_0","bh_stage_pb_1", "bh_stage_b_4", "bh_spawners")) })
  expect_true({ ro$ret_stage_k_override["K0"] == 100 })
  expect_true({ ro$ret_stage_k_override["stage_Pb_1"] == 1000 })
  expect_true({ ro$ret_stage_k_override["stage_B_4"] == 4 })
  expect_true({ is.na(ro$ret_stage_k_override["stage_Pb_2"]) })
  expect_true({ is.na(ro$ret_stage_k_override["stage_B_5"]) })


  # Return object
  colnames(habitat_dd_k) <- tolower(colnames(habitat_dd_k))
  habitat_dd_k$HUC_ID <- habitat_dd_k$huc_id
  habitat_dd_k$NAME <- habitat_dd_k$name

  ro <- CEMPRA::build_k_for_proj_dd(
    habitat_dd_k = habitat_dd_k,
    HUC_ID = 1,
    life_histories = life_histories,
    life_cycle_params = life_cycle_params
  )

  expect_true({ ro$ret_total_anadromous_spawners == 10 })
  expect_true({ all(ro$ret_bh_dd_stages == c("bh_stage_0","bh_stage_pb_1", "bh_stage_b_4", "bh_spawners")) })
  expect_true({ ro$ret_stage_k_override["K0"] == 100 })
  expect_true({ ro$ret_stage_k_override["stage_Pb_1"] == 1000 })
  expect_true({ ro$ret_stage_k_override["stage_B_4"] == 4 })
  expect_true({ is.na(ro$ret_stage_k_override["stage_Pb_2"]) })
  expect_true({ is.na(ro$ret_stage_k_override["stage_B_5"]) })



  #------------------------------------------------
  # Try running with hs_ instead of bh_
  #------------------------------------------------

  filename <- system.file("extdata/simple_test/test_build_k_for_dd/test_02_life_cycles_hs.csv",
                          package = "CEMPRA")

  life_cycle_params <- read.csv(filename, stringsAsFactors = FALSE)

  # Clean inputs
  life_cycle_params <- pop_model_dat_clean(life_cycle_params)

  # Run basic model setup
  pop_mod_setup <- pop_model_setup(life_cycles = life_cycle_params)
  pop_mod_mat <- pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)

  # Get the life histories object
  life_histories <- pop_mod_mat$life_histories

  # Return object
  ro <- CEMPRA::build_k_for_proj_dd(
    habitat_dd_k = habitat_dd_k,
    HUC_ID = 1,
    life_histories = life_histories,
    life_cycle_params = life_cycle_params
  )

  expect_true({ ro$ret_total_anadromous_spawners == 10 })
  expect_true({ all(ro$ret_bh_dd_stages == c("hs_stage_0", "hs_stage_pb_1", "hs_stage_b_4", "hs_spawners")) })
  expect_true({ ro$ret_stage_k_override["K0"] == 100 })
  expect_true({ ro$ret_stage_k_override["stage_Pb_1"] == 1000 })
  expect_true({ ro$ret_stage_k_override["stage_B_4"] == 4 })
  expect_true({ is.na(ro$ret_stage_k_override["stage_Pb_2"]) })
  expect_true({ is.na(ro$ret_stage_k_override["stage_B_5"]) })



  # ============================================================================
  # Check for possible errors with zero in the life stage chain
  # ============================================================================

  filename <- system.file("extdata/simple_test/test_build_k_for_dd/test_03_lc.csv",
                          package = "CEMPRA")

  lc <- read.csv(filename, stringsAsFactors = FALSE)
  pms3a <- pop_model_setup(life_cycles = lc)

  filename <- system.file("extdata/simple_test/test_build_k_for_dd/test_03_clean.csv",
                          package = "CEMPRA")

  lc <- read.csv(filename, stringsAsFactors = FALSE)
  pms3b <- pop_model_setup(life_cycles = lc)


  filename <- system.file("extdata/simple_test/test_build_k_for_dd/test_04_error_fix_lc.csv",
                          package = "CEMPRA")

  lc <- read.csv(filename, stringsAsFactors = FALSE)
  pms4a <- pop_model_setup(life_cycles = lc)


  filename <- system.file("extdata/simple_test/test_build_k_for_dd/test_04_error_fix_clean.csv",
                          package = "CEMPRA")

  lc <- read.csv(filename, stringsAsFactors = FALSE)
  pms4b <- pop_model_setup(life_cycles = lc)

  # Check that all values are equal
  expect_true({ pms3a$life_histories$Nstage == 7 })
  expect_true({ pms3b$life_histories$Nstage == 7 })
  expect_true({ pms4a$life_histories$Nstage == 7 })
  expect_true({ pms4b$life_histories$Nstage == 7 })
  expect_true({ pms3a$life_histories$Nstage_Pb == 4 })
  expect_true({ pms3b$life_histories$Nstage_Pb == 4 })
  expect_true({ pms4a$life_histories$Nstage_Pb == 4 })
  expect_true({ pms4b$life_histories$Nstage_Pb == 4 })
  expect_true({ pms3a$life_histories$Nstage_B == 7 })
  expect_true({ pms3b$life_histories$Nstage_B == 7 })
  expect_true({ pms4a$life_histories$Nstage_B == 7 })
  expect_true({ pms4b$life_histories$Nstage_B == 7 })
  expect_true({ all(names(pms3a$life_histories$S) == c("sE", "s0", "s1", "s2", "s3", "s4")) })
  expect_true({ all(names(pms3b$life_histories$S) == c("sE", "s0", "s1", "s2", "s3", "s4")) })
  expect_true({ all(names(pms4a$life_histories$S) == c("sE", "s0", "s1", "s2", "s3", "s4")) })
  expect_true({ all(names(pms4b$life_histories$S) == c("sE", "s0", "s1", "s2", "s3", "s4")) })

  pm <- pop_model_matrix_elements(pop_mod_setup = pms3a)
  life_histories <- pm$life_histories

  habitat_dd_k_bu <- habitat_dd_k
  habitat_dd_k_bu$k_stage_b_4_mean <- NULL

  hab_obj <- build_k_for_proj_dd(
    habitat_dd_k = habitat_dd_k_bu,
    HUC_ID = HUC_ID,
    life_histories = life_histories,
    life_cycle_params = lc
  )

  expect_true({ names(hab_obj$ret_stage_k_override[1]) == "K0" })
  expect_true({ names(hab_obj$ret_stage_k_override[8]) == "stage_B_5" })
  expect_true({ length(hab_obj$ret_stage_k_override) == 8 })


  pm <- pop_model_matrix_elements(pop_mod_setup = pms3b)
  life_histories <- pm$life_histories

  habitat_dd_k_bu <- habitat_dd_k
  habitat_dd_k_bu$k_stage_b_4_mean <- NULL

  hab_obj <- build_k_for_proj_dd(
    habitat_dd_k = habitat_dd_k_bu,
    HUC_ID = HUC_ID,
    life_histories = life_histories,
    life_cycle_params = lc
  )

  expect_true({ names(hab_obj$ret_stage_k_override[1]) == "K0" })
  expect_true({ names(hab_obj$ret_stage_k_override[8]) == "stage_B_5" })
  expect_true({ length(hab_obj$ret_stage_k_override) == 8 })





  # ===========================================================================
  # Test weird issue with character data
  # ===========================================================================
  filename <- system.file("extdata/simple_test/test_build_k_for_dd/test_05_life_cycles.csv",
                          package = "CEMPRA")
  lc <- read.csv(filename, stringsAsFactors = FALSE)
  life_cycle_params <- pop_model_dat_clean(lc)
  pop_mod_setup <- pop_model_setup(life_cycles = life_cycle_params)
  pop_mod_mat <- pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)

  # Habitat capacities
  filename <- system.file("extdata/simple_test/test_build_k_for_dd/test_05_char_hab_cap.csv", package = "CEMPRA")
  hc <- read.csv(filename, stringsAsFactors = FALSE)
  habitat_dd_k <- pop_model_hab_dens_clean(hab_dens = hc)



  # ===========================================================================
  # Test issue where user supplies non-anadromous life history... by
  # anadromous habitat capacities
  # ===========================================================================
  filename <- system.file("extdata/simple_test/test_build_k_for_dd/test_06_lc_trout.csv",
                          package = "CEMPRA")
  lc <- read.csv(filename, stringsAsFactors = FALSE)
  life_cycle_params <- pop_model_dat_clean(lc)
  pop_mod_setup <- pop_model_setup(life_cycles = life_cycle_params)
  pm <- pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)
  life_histories <- pm$life_histories

  # Habitat capacities
  filename <- system.file("extdata/simple_test/test_build_k_for_dd/test_06_hab_anadromous.csv", package = "CEMPRA")
  hc <- read.csv(filename, stringsAsFactors = FALSE)
  habitat_dd_k <- pop_model_hab_dens_clean(hab_dens = hc)

  hab_obj <- build_k_for_proj_dd(
    habitat_dd_k = habitat_dd_k,
    HUC_ID = HUC_ID,
    life_histories = life_histories,
    life_cycle_params = lc
  )

  expect_true({ names(hab_obj$ret_stage_k_override[1]) == "k_stage_0" })
  expect_true({ names(hab_obj$ret_stage_k_override[6]) == "k_stage_5" })
  expect_true({ length(hab_obj$ret_stage_k_override) == 6 })
  expect_true({ length(hab_obj$ret_bh_dd_stages) == 1 })
  expect_true({ hab_obj$ret_bh_dd_stages == "bh_stage_0" })




})

