test_that("CE capacity stressor modifies K_list for juvenile stages via life_stage tag variants", {

  # Build a minimal rates object mimicking what Projection_DD passes
  # Non-anadromous 3-stage model: K1, K2, K3
  n_years <- 3
  base_K <- c(K1 = 1000, K2 = 500, K3 = 200)
  base_Ke <- 50000
  base_K0 <- 10000

  rates <- list(
    ft = replicate(n_years, c(eps = 100), simplify = FALSE),
    st = replicate(n_years, c(sE = 0.9, s0 = 0.5, s1 = 0.6, s2 = 0.7, s3 = 0.8), simplify = FALSE),
    ut = NULL,
    smigt = NULL,
    K_list = replicate(n_years, base_K, simplify = FALSE),
    Ke_list = replicate(n_years, base_Ke, simplify = FALSE),
    K0_list = replicate(n_years, base_K0, simplify = FALSE),
    Kspawners_list = replicate(n_years, 0, simplify = FALSE)
  )

  # All these life_stage tags should target K1 capacity
  # Note: underscores get stripped, so "stage_Pb_1" -> "stagepb1", "stage_1" -> "stage1"
  k1_variants <- c("stage1", "s1", "k1", "stage_Pb_1", "Stage_Pb_1", "p1")

  for (tag in k1_variants) {
    CE_df <- data.frame(
      life_stage = tag,
      parameter = "capacity",
      sys.cap = 0.5,
      Stressor = "test_stressor",
      simulation = 1,
      HUC = 1,
      Year = 1,
      stringsAsFactors = FALSE
    )

    result <- pop_model_ce_apply_year(
      rates = rates,
      CE_df = CE_df,
      anadromous = FALSE,
      mat_stage = c(s3 = 1)
    )

    expect_equal(
      result$K_list[[1]]["K1"], c(K1 = 500),
      info = paste("life_stage tag", shQuote(tag), "should halve K1")
    )
    # K2 should be unchanged
    expect_equal(
      result$K_list[[1]]["K2"], c(K2 = 500),
      info = paste("life_stage tag", shQuote(tag), "should NOT affect K2")
    )
  }

})


test_that("CE capacity stressor modifies egg and fry capacity", {

  n_years <- 2
  rates <- list(
    ft = replicate(n_years, c(eps = 100), simplify = FALSE),
    st = replicate(n_years, c(sE = 0.9, s0 = 0.5, s1 = 0.6), simplify = FALSE),
    ut = NULL,
    smigt = NULL,
    K_list = replicate(n_years, c(K1 = 1000), simplify = FALSE),
    Ke_list = replicate(n_years, 50000, simplify = FALSE),
    K0_list = replicate(n_years, 10000, simplify = FALSE),
    Kspawners_list = replicate(n_years, 0, simplify = FALSE)
  )

  # Test egg capacity tags
  egg_tags <- c("se", "stagee", "egg", "eggs", "ke")
  for (tag in egg_tags) {
    CE_df <- data.frame(
      life_stage = tag,
      parameter = "capacity",
      sys.cap = 0.5,
      Stressor = "test",
      simulation = 1,
      HUC = 1,
      Year = 1,
      stringsAsFactors = FALSE
    )
    result <- pop_model_ce_apply_year(rates = rates, CE_df = CE_df, anadromous = FALSE, mat_stage = c(s1 = 1))
    expect_equal(result$Ke_list[[1]], 25000, info = paste("egg tag", shQuote(tag)))
  }

  # Test fry capacity tags
  fry_tags <- c("s0", "so", "stage0", "fry", "k0")
  for (tag in fry_tags) {
    CE_df <- data.frame(
      life_stage = tag,
      parameter = "capacity",
      sys.cap = 0.5,
      Stressor = "test",
      simulation = 1,
      HUC = 1,
      Year = 1,
      stringsAsFactors = FALSE
    )
    result <- pop_model_ce_apply_year(rates = rates, CE_df = CE_df, anadromous = FALSE, mat_stage = c(s1 = 1))
    expect_equal(result$K0_list[[1]], 5000, info = paste("fry tag", shQuote(tag)))
  }

})


test_that("CE capacity stressor modifies spawner K via life_stage tag variants (anadromous)", {

  n_years <- 2
  base_K <- c(stage_Pb_1 = 1000, stage_Pb_2 = 500, stage_B_1 = 200, stage_B_2 = 100)

  rates <- list(
    ft = replicate(n_years, c(eps1 = 100, eps2 = 80), simplify = FALSE),
    st = replicate(n_years, c(sE = 0.9, s0 = 0.5, s1 = 0.6, s2 = 0.7), simplify = FALSE),
    ut = replicate(n_years, c(u1 = 0.9, u2 = 0.85), simplify = FALSE),
    smigt = replicate(n_years, c(smig1 = 0.95, smig2 = 0.9), simplify = FALSE),
    K_list = replicate(n_years, base_K, simplify = FALSE),
    Ke_list = replicate(n_years, 50000, simplify = FALSE),
    K0_list = replicate(n_years, 10000, simplify = FALSE),
    Kspawners_list = replicate(n_years, 300, simplify = FALSE)
  )

  # All these tags should target ALL spawner capacity (Kspawners_list + all stage_B_ in K_list)
  all_spawner_tags <- c("spawner", "spawners", "spawn", "stage_B", "Stage_B")

  for (tag in all_spawner_tags) {
    CE_df <- data.frame(
      life_stage = tag,
      parameter = "capacity",
      sys.cap = 0.5,
      Stressor = "test",
      simulation = 1,
      HUC = 1,
      Year = 1,
      stringsAsFactors = FALSE
    )
    result <- pop_model_ce_apply_year(rates = rates, CE_df = CE_df, anadromous = TRUE, mat_stage = NULL)

    expect_equal(result$Kspawners_list[[1]], 150,
                 info = paste("all-spawner tag", shQuote(tag), "should halve Kspawners_list"))
    expect_equal(result$K_list[[1]]["stage_B_1"], c(stage_B_1 = 100),
                 info = paste("all-spawner tag", shQuote(tag), "should halve stage_B_1"))
    expect_equal(result$K_list[[1]]["stage_B_2"], c(stage_B_2 = 50),
                 info = paste("all-spawner tag", shQuote(tag), "should halve stage_B_2"))
    # Pre-breeder K should be untouched
    expect_equal(result$K_list[[1]]["stage_Pb_1"], c(stage_Pb_1 = 1000),
                 info = paste("all-spawner tag", shQuote(tag), "should NOT affect stage_Pb_1"))
  }

  # Test specific spawner stage tags targeting stage_B_1 only
  b1_tags <- c("spawn1", "spawners1", "spawner1", "b1", "stage_B_1", "Stage_B_1", "stageb1")

  for (tag in b1_tags) {
    CE_df <- data.frame(
      life_stage = tag,
      parameter = "capacity",
      sys.cap = 0.5,
      Stressor = "test",
      simulation = 1,
      HUC = 1,
      Year = 1,
      stringsAsFactors = FALSE
    )
    result <- pop_model_ce_apply_year(rates = rates, CE_df = CE_df, anadromous = TRUE, mat_stage = NULL)

    expect_equal(result$K_list[[1]]["stage_B_1"], c(stage_B_1 = 100),
                 info = paste("b1 tag", shQuote(tag), "should halve stage_B_1"))
    # stage_B_2 should be untouched
    expect_equal(result$K_list[[1]]["stage_B_2"], c(stage_B_2 = 100),
                 info = paste("b1 tag", shQuote(tag), "should NOT affect stage_B_2"))
  }

})


test_that("CE capacity stressor with 'adult' tag targets mature stages for non-anadromous", {

  n_years <- 2
  base_K <- c(K1 = 1000, K2 = 500, K3 = 200)

  rates <- list(
    ft = replicate(n_years, c(eps = 100), simplify = FALSE),
    st = replicate(n_years, c(sE = 0.9, s0 = 0.5, s1 = 0.6, s2 = 0.7, s3 = 0.8), simplify = FALSE),
    ut = NULL,
    smigt = NULL,
    K_list = replicate(n_years, base_K, simplify = FALSE),
    Ke_list = replicate(n_years, 50000, simplify = FALSE),
    K0_list = replicate(n_years, 10000, simplify = FALSE),
    Kspawners_list = replicate(n_years, 0, simplify = FALSE)
  )

  CE_df <- data.frame(
    life_stage = "adult",
    parameter = "capacity",
    sys.cap = 0.5,
    Stressor = "test",
    simulation = 1,
    HUC = 1,
    Year = 1,
    stringsAsFactors = FALSE
  )

  # mat_stage says s3 is the mature stage
  result <- pop_model_ce_apply_year(rates = rates, CE_df = CE_df, anadromous = FALSE, mat_stage = c(s3 = 1))

  expect_equal(result$K_list[[1]]["K3"], c(K3 = 100), info = "adult tag should halve K3 (mature stage)")
  expect_equal(result$K_list[[1]]["K1"], c(K1 = 1000), info = "adult tag should NOT affect K1")
  expect_equal(result$K_list[[1]]["K2"], c(K2 = 500), info = "adult tag should NOT affect K2")

})


test_that("CE capacity with 'adult' tag redirects to spawners for anadromous", {

  n_years <- 2
  base_K <- c(stage_Pb_1 = 1000, stage_B_1 = 200)

  rates <- list(
    ft = replicate(n_years, c(eps1 = 100), simplify = FALSE),
    st = replicate(n_years, c(sE = 0.9, s0 = 0.5, s1 = 0.6), simplify = FALSE),
    ut = replicate(n_years, c(u1 = 0.9), simplify = FALSE),
    smigt = replicate(n_years, c(smig1 = 0.95), simplify = FALSE),
    K_list = replicate(n_years, base_K, simplify = FALSE),
    Ke_list = replicate(n_years, 50000, simplify = FALSE),
    K0_list = replicate(n_years, 10000, simplify = FALSE),
    Kspawners_list = replicate(n_years, 200, simplify = FALSE)
  )

  CE_df <- data.frame(
    life_stage = "adult",
    parameter = "capacity",
    sys.cap = 0.5,
    Stressor = "test",
    simulation = 1,
    HUC = 1,
    Year = 1,
    stringsAsFactors = FALSE
  )

  result <- pop_model_ce_apply_year(rates = rates, CE_df = CE_df, anadromous = TRUE, mat_stage = NULL)

  # For anadromous, "adult" redirects to "spawners" which modifies Kspawners_list and stage_B_ in K_list
  expect_equal(result$Kspawners_list[[1]], 100, info = "adult -> spawners should halve Kspawners_list")
  expect_equal(result$K_list[[1]]["stage_B_1"], c(stage_B_1 = 100), info = "adult -> spawners should halve stage_B_1")
  expect_equal(result$K_list[[1]]["stage_Pb_1"], c(stage_Pb_1 = 1000), info = "adult -> spawners should NOT affect pre-breeders")

})


test_that("CE capacity with pre-breeder tag variants targets stage_Pb K (anadromous)", {

  n_years <- 2
  base_K <- c(stage_Pb_1 = 1000, stage_Pb_2 = 500, stage_B_1 = 200)

  rates <- list(
    ft = replicate(n_years, c(eps1 = 100), simplify = FALSE),
    st = replicate(n_years, c(sE = 0.9, s0 = 0.5, s1 = 0.6, s2 = 0.7), simplify = FALSE),
    ut = replicate(n_years, c(u1 = 0.9), simplify = FALSE),
    smigt = replicate(n_years, c(smig1 = 0.95), simplify = FALSE),
    K_list = replicate(n_years, base_K, simplify = FALSE),
    Ke_list = replicate(n_years, 50000, simplify = FALSE),
    K0_list = replicate(n_years, 10000, simplify = FALSE),
    Kspawners_list = replicate(n_years, 200, simplify = FALSE)
  )

  # These tags should all target stage_Pb_1 capacity
  # After underscore stripping: "stage_Pb_1" -> "stagepb1", "Stage_Pb_1" -> "stagepb1"
  pb1_tags <- c("stage1", "s1", "k1", "stage_Pb_1", "Stage_Pb_1", "p1")

  for (tag in pb1_tags) {
    CE_df <- data.frame(
      life_stage = tag,
      parameter = "capacity",
      sys.cap = 0.5,
      Stressor = "test",
      simulation = 1,
      HUC = 1,
      Year = 1,
      stringsAsFactors = FALSE
    )
    result <- pop_model_ce_apply_year(rates = rates, CE_df = CE_df, anadromous = TRUE, mat_stage = NULL)

    expect_equal(result$K_list[[1]]["stage_Pb_1"], c(stage_Pb_1 = 500),
                 info = paste("tag", shQuote(tag), "should halve stage_Pb_1"))
    expect_equal(result$K_list[[1]]["stage_Pb_2"], c(stage_Pb_2 = 500),
                 info = paste("tag", shQuote(tag), "should NOT affect stage_Pb_2"))
    expect_equal(result$K_list[[1]]["stage_B_1"], c(stage_B_1 = 200),
                 info = paste("tag", shQuote(tag), "should NOT affect stage_B_1"))
  }

})
