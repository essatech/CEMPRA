# Habitat capacity uncertainty (k_..._cv columns).
#
# Historically the cv columns were silently dropped by
# pop_model_hab_dens_clean(), so the documented K-uncertainty feature was
# completely inert through PopulationModel_Run(); and had the cv survived,
# build_k_for_proj_dd() made a single unclamped rnorm draw shared by every
# batch replicate (a large cv could produce a NEGATIVE capacity that became
# the starting abundance). New behavior: the cleaner keeps cv columns
# (keep_cv = FALSE drops them - Shiny app mode), build_k passes means
# through untouched and returns the cv values, and PopulationModel_Run
# draws per-replicate, per-year multiplicative noise factors clamped at
# zero via build_stage_k_noise(), sharing each replicate's draws between
# the CE and baseline runs.

lc_canonical <- function() {
  base <- system.file("extdata", package = "CEMPRA")
  lc <- read.csv(file.path(base, "life_cycles.csv"))
  # K-file mode: cr = 1 + a bh flag on stage 1
  lc$Value[grepl("^cr", lc$Name)] <- 1
  rbind(lc, data.frame(Parameters = "flag", Name = "bh_stage_1", Value = 1))
}

run_pm <- function(hab, lc, n_years = 15, MC_sims = 2) {
  PopulationModel_Run(
    dose = NULL, sr_wb_dat = NULL, life_cycle_params = lc,
    HUC_ID = 1, n_years = n_years, MC_sims = MC_sims, habitat_dd_k = hab
  )
}

# ---------------------------------------------------------------------------
# pop_model_hab_dens_clean
# ---------------------------------------------------------------------------

test_that("hab_dens_clean keeps cv columns by default and drops them on request", {
  set.seed(42)
  hab <- data.frame(HUC_ID = 1, NAME = "x",
                    k_stage_1_mean = 5000, k_stage_1_cv = 0.3)

  out <- pop_model_hab_dens_clean(hab)
  expect_true("k_stage_1_cv" %in% colnames(out))
  expect_equal(out$k_stage_1_cv, 0.3)

  out2 <- pop_model_hab_dens_clean(hab, keep_cv = FALSE)
  expect_false("k_stage_1_cv" %in% colnames(out2))
  expect_true("k_stage_1_mean" %in% colnames(out2))
})

test_that("hab_dens_clean normalizes spawner cv variants and drops orphan cv", {
  set.seed(42)
  hab <- data.frame(HUC_ID = 1, NAME = "x",
                    k_spawners_mean = 800, k_spawners_cv = 0.2)
  out <- pop_model_hab_dens_clean(hab)
  expect_true("k_stage_B_mean" %in% colnames(out))
  expect_true("k_stage_B_cv" %in% colnames(out))

  # cv with no matching mean column: warn and drop
  hab2 <- data.frame(HUC_ID = 1, NAME = "x",
                     k_stage_1_mean = 5000, k_stage_2_cv = 0.4)
  expect_warning(out2 <- pop_model_hab_dens_clean(hab2),
                 "no matching _mean")
  expect_false("k_stage_2_cv" %in% colnames(out2))
})

test_that("hab_dens_clean errors clearly when no k_ columns exist", {
  set.seed(42)
  hab <- data.frame(HUC_ID = 1, NAME = "x", capacity = 100)
  expect_error(pop_model_hab_dens_clean(hab), "No capacity")
})

# ---------------------------------------------------------------------------
# build_k_for_proj_dd cv extraction (means must pass through undrawn)
# ---------------------------------------------------------------------------

test_that("build_k passes K means through exactly and returns cv values", {
  set.seed(42)
  lc <- lc_canonical()
  dat <- pop_model_dat_clean(lc)
  setup <- pop_model_setup(life_cycles = dat)
  lh <- pop_model_matrix_elements(pop_mod_setup = setup)$life_histories

  hab <- pop_model_hab_dens_clean(
    data.frame(HUC_ID = 1, NAME = "x",
               k_stage_1_mean = 5000, k_stage_1_cv = 0.5,
               k_stage_2_mean = 300)
  )
  ret <- suppressWarnings(build_k_for_proj_dd(habitat_dd_k = hab, HUC_ID = 1,
                             life_histories = lh, life_cycle_params = dat))

  # Means untouched (previously an rnorm draw happened here)
  expect_identical(as.numeric(ret$ret_stage_k_override[["k_stage_1"]]), 5000)
  expect_identical(as.numeric(ret$ret_stage_k_override[["k_stage_2"]]), 300)
  # cv captured; 0 where absent
  expect_identical(as.numeric(ret$ret_stage_k_cv[["k_stage_1"]]), 0.5)
  expect_identical(as.numeric(ret$ret_stage_k_cv[["k_stage_2"]]), 0)
  expect_identical(ret$ret_spawner_k_cv, 0)
})

test_that("build_k extracts cv in the anadromous branch (fry, Pb, B, spawners)", {
  set.seed(42)
  lh <- list(anadromous = TRUE, Nstage_Pb = 4,
             stage_names = c("stage_Pb_1", "stage_Pb_2", "stage_Pb_3",
                             "stage_Pb_4", "stage_B_3", "stage_B_4"))
  lcp <- data.frame(Name = c("Nstage", "k", "bh_stage_b_3"), Value = c(4, 100, 1))

  hab <- data.frame(HUC_ID = 1,
                    k_stage_0_mean = 200, k_stage_0_cv = 0.1,
                    k_stage_Pb_2_mean = 400, k_stage_Pb_2_cv = 0.2,
                    k_stage_B_3_mean = 500, k_stage_B_3_cv = 0.3,
                    k_stage_B_mean = 900, k_stage_B_cv = 0.4)
  ret <- suppressWarnings(build_k_for_proj_dd(habitat_dd_k = hab, HUC_ID = 1,
                             life_histories = lh, life_cycle_params = lcp))

  expect_identical(as.numeric(ret$ret_stage_k_cv[["K0"]]), 0.1)
  expect_identical(as.numeric(ret$ret_stage_k_cv[["stage_Pb_2"]]), 0.2)
  expect_identical(as.numeric(ret$ret_stage_k_cv[["stage_B_3"]]), 0.3)
  expect_identical(ret$ret_spawner_k_cv, 0.4)
  # cv defaults to 0 for stages without a cv column
  expect_identical(as.numeric(ret$ret_stage_k_cv[["stage_Pb_1"]]), 0)
})

test_that("negative cv values error clearly", {
  set.seed(42)
  lh <- list(anadromous = TRUE, Nstage_Pb = 2,
             stage_names = c("stage_Pb_1", "stage_Pb_2", "stage_B_2"))
  lcp <- data.frame(Name = c("Nstage"), Value = c(2))
  hab <- data.frame(HUC_ID = 1, k_stage_0_mean = 200, k_stage_0_cv = -0.5)
  expect_error(
    build_k_for_proj_dd(habitat_dd_k = hab, HUC_ID = 1,
                        life_histories = lh, life_cycle_params = lcp),
    "cv values must be >= 0"
  )
})

# ---------------------------------------------------------------------------
# build_stage_k_noise
# ---------------------------------------------------------------------------

test_that("build_stage_k_noise draws per year, clamps at zero, and is NULL with no cv", {
  set.seed(42)
  # All cv zero (or NULL input): NULL, and NO random numbers consumed
  rng_before <- .Random.seed
  expect_null(build_stage_k_noise(c(0, 0, NA), 0, Nyears = 10))
  expect_null(build_stage_k_noise(NULL, NULL, Nyears = 10))
  expect_identical(.Random.seed, rng_before)

  # Dimensions: Nyears rows, length(stage_k_cv) + 1 columns
  nz <- build_stage_k_noise(c(0, 0.3, 0), 0, Nyears = 50)
  expect_true(is.matrix(nz))
  expect_identical(dim(nz), c(50L, 4L))

  # cv = 0 columns constant at 1; cv > 0 column varies by year, mean near 1
  expect_true(all(nz[, c(1, 3, 4)] == 1))
  expect_gt(stats::sd(nz[, 2]), 0)
  expect_lt(abs(mean(nz[, 2]) - 1), 0.2)

  # Clamped at zero even for very large cv (draws would go negative)
  big <- build_stage_k_noise(c(2), 0, Nyears = 2000)
  expect_gte(min(big[, 1]), 0)
  expect_gt(sum(big[, 1] == 0), 0) # clamping actually engaged

  # Spawner cv occupies the final column
  nzs <- build_stage_k_noise(c(0, 0), 0.5, Nyears = 30)
  expect_gt(stats::sd(nzs[, 3]), 0)
  expect_true(all(nzs[, 1:2] == 1))

  expect_error(build_stage_k_noise(c(-0.1), 0, 10), "cv values must be >= 0")
})

# ---------------------------------------------------------------------------
# Projection_DD integration
# ---------------------------------------------------------------------------

test_that("all-ones noise is identical to NULL noise; binding noise scales the cap", {
  set.seed(42)
  lc <- lc_canonical()
  dat <- pop_model_dat_clean(lc)
  setup <- pop_model_setup(life_cycles = dat)
  me <- pop_model_matrix_elements(pop_mod_setup = setup)
  lh <- me$life_histories
  n_stage <- lh$Nstage

  ko <- rep(NA_real_, n_stage + 1)
  ko[2] <- 5000

  run_proj <- function(noise, seed = 99) {
    set.seed(seed)
    Projection_DD(
      M.mx = me$life_stages_symbolic,
      D.mx = me$density_stage_symbolic,
      dat = lh, Nyears = 20, p.cat = 0,
      stage_k_override = ko,
      bh_dd_stages = c("hs_stage_1"),
      stage_k_noise = noise
    )
  }

  r_null <- run_proj(NULL)
  r_ones <- run_proj(matrix(1, nrow = 20, ncol = n_stage + 2))
  expect_identical(r_null$N, r_ones$N)

  # Noise of 0.5 on stage 1 halves the hard (hockey-stick) cap
  half <- matrix(1, nrow = 20, ncol = n_stage + 2)
  half[, 2] <- 0.5
  r_half <- run_proj(half)
  s1 <- r_half$N[-1, 1] # stage-1 abundance, years 1..20
  expect_true(all(s1 <= 5000 * 0.5 + 1e-8))

  # Malformed noise matrix errors clearly
  expect_error(run_proj(matrix(1, nrow = 20, ncol = 2)), "stage_k_noise")
})

# ---------------------------------------------------------------------------
# PopulationModel_Run end to end
# ---------------------------------------------------------------------------

test_that("cv absent, cv = 0 and cv = NA are bit-identical (no RNG consumed)", {
  set.seed(42)
  lc <- lc_canonical()
  hab_none <- data.frame(HUC_ID = 1, NAME = "x", k_stage_1_mean = 5000)
  hab_zero <- data.frame(HUC_ID = 1, NAME = "x",
                         k_stage_1_mean = 5000, k_stage_1_cv = 0)
  hab_na <- data.frame(HUC_ID = 1, NAME = "x",
                       k_stage_1_mean = 5000, k_stage_1_cv = NA)

  set.seed(101); r_none <- run_pm(hab_none, lc)
  set.seed(101); r_zero <- run_pm(hab_zero, lc)
  set.seed(101); r_na <- run_pm(hab_na, lc)

  expect_identical(r_none$ce[[1]]$pop$N, r_zero$ce[[1]]$pop$N)
  expect_identical(r_none$ce[[1]]$pop$N, r_na$ce[[1]]$pop$N)
  expect_identical(r_none$baseline[[2]]$pop$N, r_zero$baseline[[2]]$pop$N)
})

test_that("cv > 0 varies capacity between years and between replicates, never negative", {
  set.seed(42)
  lc <- lc_canonical()
  hab <- data.frame(HUC_ID = 1, NAME = "x",
                    k_stage_1_mean = 5000, k_stage_1_cv = 0.4)
  hab_none <- data.frame(HUC_ID = 1, NAME = "x", k_stage_1_mean = 5000)

  set.seed(101); r_cv <- run_pm(hab, lc, n_years = 25, MC_sims = 3)
  set.seed(101); r_none <- run_pm(hab_none, lc, n_years = 25, MC_sims = 3)

  # The K draws changed the trajectories
  expect_false(identical(r_none$ce[[1]]$pop$N, r_cv$ce[[1]]$pop$N))

  # Replicates differ from one another (per-replicate draws, not one
  # shared draw as before)
  expect_false(identical(r_cv$ce[[1]]$pop$N, r_cv$ce[[2]]$pop$N))
  expect_false(identical(r_cv$ce[[2]]$pop$N, r_cv$ce[[3]]$pop$N))

  # No negative abundances anywhere, even with a large cv
  for (i in 1:3) {
    expect_true(all(r_cv$ce[[i]]$pop$N >= 0, na.rm = TRUE))
    expect_true(all(r_cv$baseline[[i]]$pop$N >= 0, na.rm = TRUE))
  }
})

test_that("anadromous run with spawner capacity cv completes without negatives", {
  set.seed(42)
  lc <- read.csv(system.file(
    "extdata/nanaimo/species_profiles/beechie_2021_chehalis.csv",
    package = "CEMPRA"
  ))
  # Avoid the mixed-mode (cr > 1 with bh_dd_stages) warning
  lc$Value[grepl("^cr", lc$Name)] <- 1
  lc <- rbind(lc, data.frame(Parameters = "flag", Name = "bh_spawners",
                             Value = 1, Notes = ""))

  hab <- data.frame(HUC_ID = 1, NAME = "x",
                    k_spawners_mean = 800, k_spawners_cv = 0.3)
  hab <- pop_model_hab_dens_clean(hab)

  set.seed(7)
  res <- run_pm(hab, lc, n_years = 20, MC_sims = 2)
  for (i in 1:2) {
    expect_true(all(res$ce[[i]]$pop$N >= 0, na.rm = TRUE))
  }
  # Replicates got different spawner-K draws
  expect_false(identical(res$ce[[1]]$pop$N, res$ce[[2]]$pop$N))
})
