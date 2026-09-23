# Polish batch (2026-09-22): six small robustness fixes.
# 1. NA mat_x in anadromous profiles -> clear early error (was "object
#    'uNA' not found" far downstream)  [issue 2.1b]
# 2. build_k missing/duplicate habitat row -> informative messages (was a
#    typo'd catch-all)                  [Part 5 ruling #4: keep the error]
# 3. dat_clean out-of-range stage rows removed by regex, not 1:10 lists
# 4. dd_hs_spawners accepted as an alias of hs_spawners end to end
# 5. NA-safe guards: survival==1 repair (name-based) + symbolic-vs-QA
#    matrix comparisons
# 6. lognormal dose with Mean <= 0 warns instead of silently zeroing

test_that("NA maturity rows error early with a clear message (2.1b)", {
  lc <- read.csv(system.file(
    "extdata/nanaimo/species_profiles/beechie_2021_chehalis.csv",
    package = "CEMPRA"))
  lc$Value[lc$Name == "mat_3"] <- NA
  expect_error(pop_model_setup(life_cycles = lc), "Maturity row")
})

test_that("build_k names the missing/duplicated habitat location", {
  lc <- read.csv(system.file("extdata/sockeye/life_cycles_sockeye.csv",
                             package = "CEMPRA"))
  hab <- read.csv(system.file(
    "extdata/sockeye/habitat_capacities_sockeye.csv", package = "CEMPRA"),
    stringsAsFactors = FALSE)
  s <- pop_model_setup(life_cycles = lc)
  m <- pop_model_matrix_elements(s)

  # location absent from the habitat file
  expect_error(
    build_k_for_proj_dd(habitat_dd_k = hab, HUC_ID = 999999,
                        life_histories = m$life_histories,
                        life_cycle_params = pop_model_dat_clean(lc)),
    "has no row in the habitat capacities")

  # location duplicated in the habitat file
  hab2 <- rbind(hab, hab[1, ])
  expect_error(
    build_k_for_proj_dd(habitat_dd_k = hab2, HUC_ID = hab$HUC_ID[1],
                        life_histories = m$life_histories,
                        life_cycle_params = pop_model_dat_clean(lc)),
    "exactly one row")
})

test_that("dat_clean removes out-of-range stage rows for any stage count", {
  # anadromous: stray eps_11 removed (Nstage < 11)
  lc_a <- read.csv(system.file(
    "extdata/nanaimo/species_profiles/beechie_2021_chehalis.csv",
    package = "CEMPRA"))
  lc_a <- rbind(lc_a[, c("Parameters", "Name", "Value")],
                data.frame(Parameters = "eps_11", Name = "eps_11", Value = 9999))
  out_a <- pop_model_dat_clean(lc_a)
  expect_false("eps_11" %in% out_a$Name)

  # non-anadromous: stray u_11 / smig_11 rows removed entirely
  lc_n <- read.csv(system.file("extdata", "life_cycles.csv", package = "CEMPRA"))
  lc_n <- rbind(lc_n,
                data.frame(Parameters = c("u_11", "smig_11"),
                           Name = c("u_11", "smig_11"),
                           Value = c(0.5, 0.5)))
  out_n <- pop_model_dat_clean(lc_n)
  expect_false(any(c("u_11", "smig_11") %in% out_n$Name))
})

test_that("dd_hs_spawners works as an alias of hs_spawners", {
  # validator accepts it
  expect_true(bh_dd_stages_check_ok("dd_hs_spawners"))

  # dd.N.bh treats it exactly like hs_spawners (toy from the spawner-cap
  # tests: weighted spawners land on the cap)
  dat <- list(anadromous = FALSE, Nstage = 3, stage_names = c("K1", "K2", "K3"),
              mat = c(mat2 = 0.1, mat3 = 1.0), K = c(NA, NA, NA), K0 = NA,
              stage_k_spawners = 100,
              Surv_annual = c(sE = 0.1, s0 = 0.3, s1 = 0.3, s2 = 0.5, s3 = 0.9))
  st_t <- c(sE = 0.1, s0 = 0.3, s1 = 0.3, s2 = 0.5, s3 = 0.9)
  N <- c(10, 1000, 50)
  out_alias <- dd.N.bh(dat = dat, t = 1, st = list(st_t, st_t), N = N,
                       N_prev = N, bh_dd_stages = "dd_hs_spawners", A = diag(3))
  out_canon <- dd.N.bh(dat = dat, t = 1, st = list(st_t, st_t), N = N,
                       N_prev = N, bh_dd_stages = "hs_spawners", A = diag(3))
  expect_equal(out_alias, out_canon)

  # a dd_hs_spawners flag row counts as an enabled DD flag (calibration gate)
  flags_df <- data.frame(Name = "dd_hs_spawners", Value = 1)
  expect_true(any_dd_flag_enabled(flags_df))

  # build_k maps the flag row to hs_spawners
  lc <- read.csv(system.file("extdata/sockeye/life_cycles_sockeye.csv",
                             package = "CEMPRA"))
  lc$Name[lc$Name == "bh_spawners"] <- "dd_hs_spawners"
  lc$Parameters[lc$Parameters == "BH DD 5"] <- "DD HS spawners"
  hab <- read.csv(system.file(
    "extdata/sockeye/habitat_capacities_sockeye.csv", package = "CEMPRA"),
    stringsAsFactors = FALSE)
  s <- pop_model_setup(life_cycles = lc)
  m <- pop_model_matrix_elements(s)
  bk <- suppressWarnings(suppressMessages(
    build_k_for_proj_dd(habitat_dd_k = hab, HUC_ID = hab$HUC_ID[1],
                        life_histories = m$life_histories,
                        life_cycle_params = pop_model_dat_clean(lc))))
  expect_true("hs_spawners" %in% bk$ret_bh_dd_stages)
})

test_that("NA survival values no longer crash the matrix builder guards", {
  lc <- read.csv(system.file("extdata", "life_cycles.csv", package = "CEMPRA"))
  lc$Value[lc$Name == "surv_2"] <- NA
  # previously: "missing value where TRUE/FALSE needed" from the
  # survival==1 repair or the symbolic-vs-QA comparison. Now the pipeline
  # completes (the NA propagates into the matrix, visibly).
  s <- pop_model_setup(life_cycles = lc)
  expect_true(is.list(suppressMessages(pop_model_matrix_elements(s))))
})

test_that("lognormal dose with Mean <= 0 warns instead of silent zeros", {
  dose <- StressorMagnitudeWorkbook(
    filename = system.file("extdata", "stressor_magnitude_unc_ARTR.xlsx",
                           package = "CEMPRA"),
    scenario_worksheet = "natural_unc")
  sr_wb_dat <- StressorResponseWorkbook(
    filename = system.file("extdata", "stressor_response_fixed_ARTR.xlsx",
                           package = "CEMPRA"))
  dose$Distribution[1] <- "lognormal"
  dose$Mean[1] <- 0
  dose$Low_Limit[1] <- 0
  expect_warning(
    JoeModel_Run(dose = dose, sr_wb_dat = sr_wb_dat, MC_sims = 3),
    "lognormal")
})
