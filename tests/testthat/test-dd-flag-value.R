# Density-dependence flag rows (bh_stage_..., hs_stage_..., *_spawners)
# are switches: active only when the row exists AND Value is 1/TRUE.
# Historically the anadromous branch of build_k_for_proj_dd() activated
# flags on row PRESENCE alone - a flag set to 0 (or blank) still switched
# density dependence ON, and a bh_ row always shadowed an hs_ row
# regardless of their values. Both modes now share dd_flag_enabled().

# --- helpers --------------------------------------------------------------

# Anadromous setup from the shipped sockeye profile (all flags = TRUE)
ddf_anad_setup <- function() {
  lc <- read.csv(system.file("extdata/sockeye/life_cycles_sockeye.csv",
                             package = "CEMPRA"), stringsAsFactors = FALSE)
  lc <- pop_model_dat_clean(lc)
  setup <- pop_model_setup(life_cycles = lc)
  mat_el <- pop_model_matrix_elements(setup)
  list(lc = lc, lh = mat_el$life_histories, mat_el = mat_el)
}

ddf_set_flag <- function(lc, name, value) {
  if (name %in% lc$Name) {
    lc$Value[lc$Name == name] <- value
  } else {
    lc <- rbind(lc, data.frame(Parameters = "DD", Name = name, Value = value))
  }
  lc
}

ddf_build <- function(lc, lh, hab) {
  build_k_for_proj_dd(habitat_dd_k = hab, HUC_ID = 1,
                      life_histories = lh, life_cycle_params = lc)
}


test_that("anadromous: flag Value is honored (1 on; 0/NA/absent off)", {
  s <- ddf_anad_setup()
  hab <- data.frame(HUC_ID = 1, NAME = "x", k_stage_Pb_1_mean = 5000)

  # Value = 1 (shipped) -> active
  res <- suppressMessages(ddf_build(s$lc, s$lh, hab))
  expect_true("bh_stage_pb_1" %in% res$ret_bh_dd_stages)

  # Value = 0 -> off
  res0 <- suppressWarnings(suppressMessages(ddf_build(ddf_set_flag(s$lc, "bh_stage_pb_1", 0),
                                     s$lh, hab)))
  expect_false("bh_stage_pb_1" %in% res0$ret_bh_dd_stages)

  # Value = NA (blank cell) -> off
  res_na <- suppressWarnings(suppressMessages(ddf_build(ddf_set_flag(s$lc, "bh_stage_pb_1", NA),
                                       s$lh, hab)))
  expect_false("bh_stage_pb_1" %in% res_na$ret_bh_dd_stages)

  # Row absent -> off
  lc_rm <- s$lc[s$lc$Name != "bh_stage_pb_1", ]
  res_rm <- suppressWarnings(suppressMessages(ddf_build(lc_rm, s$lh, hab)))
  expect_false("bh_stage_pb_1" %in% res_rm$ret_bh_dd_stages)

  # The K value itself still loads (constraint number without a switch)
  expect_equal(unname(res0$ret_stage_k_override["stage_Pb_1"]), 5000)
})


test_that("anadromous: bh = 0 + hs = 1 selects the hockey stick", {
  s <- ddf_anad_setup()
  hab <- data.frame(HUC_ID = 1, NAME = "x", k_stage_Pb_1_mean = 5000)
  lc <- ddf_set_flag(s$lc, "bh_stage_pb_1", 0)
  lc <- ddf_set_flag(lc, "hs_stage_pb_1", 1)
  res <- suppressMessages(ddf_build(lc, s$lh, hab))
  expect_false("bh_stage_pb_1" %in% res$ret_bh_dd_stages)
  expect_true("hs_stage_pb_1" %in% res$ret_bh_dd_stages)
})


test_that("anadromous: spawner flag Value is honored", {
  s <- ddf_anad_setup()
  hab <- data.frame(HUC_ID = 1, NAME = "x", k_stage_B_mean = 2000)

  res <- suppressMessages(ddf_build(s$lc, s$lh, hab))
  expect_true("bh_spawners" %in% res$ret_bh_dd_stages)

  res0 <- suppressWarnings(suppressMessages(ddf_build(ddf_set_flag(s$lc, "bh_spawners", 0),
                                     s$lh, hab)))
  expect_false("bh_spawners" %in% res0$ret_bh_dd_stages)
})


test_that("anadromous: dd_hs_0 is accepted as an alias of hs_stage_0", {
  s <- ddf_anad_setup()
  hab <- data.frame(HUC_ID = 1, NAME = "x", k_stage_0_mean = 100000)
  lc <- ddf_set_flag(s$lc, "bh_stage_0", 0)
  lc <- ddf_set_flag(lc, "dd_hs_0", 1)
  res <- suppressMessages(ddf_build(lc, s$lh, hab))
  expect_true("hs_stage_0" %in% res$ret_bh_dd_stages)
  expect_false("bh_stage_0" %in% res$ret_bh_dd_stages)
})


test_that("non-anadromous: flag semantics unchanged and identical to anadromous", {
  lc <- read.csv(system.file("extdata", "life_cycles.csv", package = "CEMPRA"),
                 stringsAsFactors = FALSE)
  lc <- pop_model_dat_clean(lc)
  setup <- pop_model_setup(life_cycles = lc)
  lh <- pop_model_matrix_elements(setup)$life_histories
  hab <- data.frame(HUC_ID = 1, NAME = "x", k_stage_1_mean = 5000)

  on <- ddf_build(ddf_set_flag(lc, "bh_stage_1", 1), lh, hab)
  expect_true("bh_stage_1" %in% on$ret_bh_dd_stages)

  off <- suppressWarnings(ddf_build(ddf_set_flag(lc, "bh_stage_1", 0), lh, hab))
  expect_false("bh_stage_1" %in% off$ret_bh_dd_stages)
})


test_that("non-anadromous: disabled bh_spawners no longer shadows enabled hs_spawners", {
  lc <- read.csv(system.file("extdata", "life_cycles.csv", package = "CEMPRA"),
                 stringsAsFactors = FALSE)
  lc <- pop_model_dat_clean(lc)
  setup <- pop_model_setup(life_cycles = lc)
  lh <- pop_model_matrix_elements(setup)$life_histories
  hab <- data.frame(HUC_ID = 1, NAME = "x", k_stage_B_mean = 300)

  lc2 <- ddf_set_flag(lc, "bh_spawners", 0)
  lc2 <- ddf_set_flag(lc2, "hs_spawners", 1)
  res <- ddf_build(lc2, lh, hab)
  expect_true("hs_spawners" %in% res$ret_bh_dd_stages)
  expect_false("bh_spawners" %in% res$ret_bh_dd_stages)
})


test_that("behavioral: flag on vs off produces different trajectories", {
  set.seed(42)
  s <- ddf_anad_setup()
  # A tightly binding stage-1 capacity so the constraint clearly matters
  hab <- data.frame(HUC_ID = 1, NAME = "x", k_stage_Pb_1_mean = 200)

  run_proj <- function(lc) {
    b <- suppressWarnings(suppressMessages(ddf_build(lc, s$lh, hab)))
    set.seed(7)
    suppressWarnings(Projection_DD(
      M.mx = s$mat_el$life_stages_symbolic,
      D.mx = s$mat_el$density_stage_symbolic,
      dat = s$lh, K = s$lh$Ka, Nyears = 50, p.cat = 0,
      stage_k_override = b$ret_stage_k_override,
      bh_dd_stages = b$ret_bh_dd_stages,
      anadromous = TRUE
    ))
  }

  on <- run_proj(s$lc)
  off <- run_proj(ddf_set_flag(s$lc, "bh_stage_pb_1", 0))

  m_on <- median(on$pop$N)
  m_off <- median(off$pop$N)
  # Constraint active -> population held below the unconstrained run.
  # Before the fix these two runs were identical (0 could not disable).
  expect_false(isTRUE(all.equal(m_on, m_off)))
  expect_true(m_on < m_off)
})
