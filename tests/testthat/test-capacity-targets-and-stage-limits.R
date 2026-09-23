# 2026-09-22 batch: issues 1.1, 1.2 and 1.4.
#
# 1.1 Age-specific spawner capacity stressors (anadromous): the
#     stage_k_override path renamed the K vector to generic K1..KN,
#     orphaning stage_B_x capacity targets (a write to the missing name
#     APPENDED an element -> dd.N.bh length-check crash, or silent
#     no-op). Fixed: anadromous K vectors keep their stage-class names
#     through the override, and the CE capacity writes guard against
#     missing targets (warn + skip instead of append).
# 1.2 Habitat K values without enabled flags are silently ignored while
#     the location still runs uncalibrated. Fixed: build_k warns, naming
#     the orphaned stages (both model modes; spawner cap included).
# 1.4 Stage counts are limited to 2-10 everywhere (dat_clean + both
#     setups), and the anadromous numeric substitution runs in
#     DESCENDING order so age-10 symbols (eps10 vs eps1 ...) are intact.

# ---------------------------------------------------------------- 1.4 --
test_that("stage counts above 10 are rejected everywhere", {
  lc <- read.csv(system.file("extdata", "life_cycles.csv", package = "CEMPRA"))
  lc$Value[lc$Name == "Nstage"] <- 11
  expect_error(pop_model_dat_clean(lc), "between")
  expect_error(pop_model_setup(life_cycles = lc), "between 2 and 10")
})

mk10_anadromous <- function() {
  rows <- list(
    c("Nstage", 10), c("anadromous", 1), c("k", 1000),
    c("events", 1), c("int", 1), c("SE", 0.1), c("S0", 0.3), c("SR", 0.5),
    c("eps_sd", 0), c("egg_rho", 0), c("M.cv", 0), c("M.rho", 0)
  )
  for (i in 1:9) rows[[length(rows) + 1]] <- c(paste0("surv_", i), 0.5)
  rows[[length(rows) + 1]] <- c("surv_10", 0)
  for (i in 1:9) rows[[length(rows) + 1]] <- c(paste0("mat_", i), 0)
  rows[[length(rows) + 1]] <- c("mat_10", 1)
  rows[[length(rows) + 1]] <- c("eps_10", 4000)
  rows[[length(rows) + 1]] <- c("u_10", 0.9)
  rows[[length(rows) + 1]] <- c("smig_10", 1)
  for (i in 1:10) rows[[length(rows) + 1]] <- c(paste0("year_", i), 1)
  do.call(rbind, lapply(rows, function(x) data.frame(
    Parameters = x[1], Name = x[1], Value = as.numeric(x[2]))))
}

test_that("age-10 anadromous parameters survive the numeric substitution", {
  s10 <- pop_model_setup(life_cycles = mk10_anadromous())
  A <- s10$projection_matrix
  sn <- s10$life_histories$stage_names
  b10 <- which(sn == "stage_B_10")

  # fecundity for age-10 spawners: (u10 * events * eps10 * sE * s0 * sR)/int
  # pre-fix the ascending gsub turned eps10 into "<eps_1-value>0" etc.
  expect_equal(A[1, b10], (0.9 * 1 * 4000 * 0.1 * 0.3 * 0.5) / 1)
  # maturation into B_10: s9 * mat10 * smig10 = 0.5 * 1 * 1
  expect_equal(A[b10, 9], 0.5)

  # an 11-age anadromous file is rejected
  lc11 <- mk10_anadromous()
  lc11$Value[lc11$Name == "Nstage"] <- 11
  lc11 <- rbind(lc11, data.frame(Parameters = "surv_11", Name = "surv_11", Value = 0))
  expect_error(pop_model_setup(life_cycles = lc11), "between 2 and 10")
})

# ---------------------------------------------------------------- 1.2 --
test_that("habitat K values without enabled flags warn (non-anadromous)", {
  lc <- read.csv(system.file("extdata", "life_cycles.csv", package = "CEMPRA"))
  s <- pop_model_setup(life_cycles = lc)
  m <- pop_model_matrix_elements(s)
  hab <- data.frame(HUC_ID = 1, NAME = "x", k_stage_1_mean = 5000)

  # no flags at all -> K value orphaned -> warning
  expect_warning(
    build_k_for_proj_dd(habitat_dd_k = hab, HUC_ID = 1,
                        life_histories = m$life_histories,
                        life_cycle_params = pop_model_dat_clean(lc)),
    "IGNORED")

  # flag enabled -> no warning
  lcf <- rbind(lc, data.frame(Parameters = "bh_stage_1",
                              Name = "bh_stage_1", Value = 1))
  expect_no_warning(
    build_k_for_proj_dd(habitat_dd_k = hab, HUC_ID = 1,
                        life_histories = m$life_histories,
                        life_cycle_params = pop_model_dat_clean(lcf)))

  # spawner capacity without spawner flags -> warning
  hab_sp <- data.frame(HUC_ID = 1, NAME = "x", k_stage_B_mean = 800)
  expect_warning(
    build_k_for_proj_dd(habitat_dd_k = hab_sp, HUC_ID = 1,
                        life_histories = m$life_histories,
                        life_cycle_params = pop_model_dat_clean(lc)),
    "spawner capacity will be IGNORED")
})

# ---------------------------------------------------------------- 1.1 --
sockeye_kmode <- function() {
  lc <- read.csv(system.file("extdata/sockeye/life_cycles_sockeye.csv",
                             package = "CEMPRA"))
  s <- pop_model_setup(life_cycles = lc)
  m <- pop_model_matrix_elements(s)
  dat <- m$life_histories
  iB5 <- which(dat$stage_names == "stage_B_5")
  kover <- rep(NA, dat$Nstage + 1)
  kover[iB5 + 1] <- 300
  list(m = m, dat = dat, iB5 = iB5, kover = kover)
}
run_sockeye <- function(f, ce) {
  set.seed(31)
  Projection_DD(M.mx = f$m$life_stages_symbolic,
                D.mx = f$m$density_stage_symbolic,
                H.mx = NULL, dat = f$dat, K = 5000, Nyears = 60, p.cat = 0,
                CE_df = ce, anadromous = TRUE,
                bh_dd_stages = c("bh_stage_b_5"),
                stage_k_override = f$kover)
}

test_that("age-specific spawner capacity stressors work (anadromous)", {
  f <- sockeye_kmode()
  ce_b5 <- data.frame(HUC = 1, Stressor = "cap_b5", dose = 1, sys.cap = 0.3,
                      life_stage = "b5", parameter = "capacity",
                      Stressor_cat = "cap_b5")

  rA <- run_sockeye(f, NULL)
  rB <- run_sockeye(f, ce_b5)     # crashed pre-fix ("Nstage does not match K vector")

  # K vector kept its length (no stray appended element)
  expect_equal(ncol(rB$N), unname(f$dat$Nstage))
  # the stressor genuinely acts on the age-5 spawner capacity
  expect_false(identical(rA$pop$N, rB$pop$N))
  expect_lt(median(rB$N[30:61, f$iB5]), median(rA$N[30:61, f$iB5]))
})

test_that("capacity targets with no matching class warn and skip", {
  f <- sockeye_kmode()
  # sockeye has no stage_B_3 class
  ce_b3 <- data.frame(HUC = 1, Stressor = "cap_b3", dose = 1, sys.cap = 0.3,
                      life_stage = "b3", parameter = "capacity",
                      Stressor_cat = "cap_b3")
  expect_warning(r <- run_sockeye(f, ce_b3), "target skipped")
  expect_equal(ncol(r$N), unname(f$dat$Nstage))

  # non-anadromous: a k9 target on a 4-stage model warns and skips
  # (cr set to 1 so the cr-and-K co-active warning does not interfere)
  lc <- read.csv(system.file("extdata", "life_cycles.csv", package = "CEMPRA"))
  lc$Value[grepl("^cr_", lc$Name)] <- 1
  s <- pop_model_setup(life_cycles = lc)
  m <- pop_model_matrix_elements(s)
  ce_k9 <- data.frame(HUC = 1, Stressor = "cap_k9", dose = 1, sys.cap = 0.3,
                      life_stage = "k9", parameter = "capacity",
                      Stressor_cat = "cap_k9")
  set.seed(4)
  expect_warning(
    r2 <- Projection_DD(M.mx = m$life_stages_symbolic,
                        D.mx = m$density_stage_symbolic,
                        H.mx = NULL, dat = m$life_histories,
                        K = m$life_histories$Ka, Nyears = 30, p.cat = 0,
                        CE_df = ce_k9,
                        bh_dd_stages = c("bh_stage_1"),
                        stage_k_override = c(NA, 5000, NA, NA, NA)),
    "target skipped")
  expect_equal(ncol(r2$N), unname(m$life_histories$Nstage))
})

test_that("non-anadromous K naming is unchanged (K1..KN)", {
  lc <- read.csv(system.file("extdata", "life_cycles.csv", package = "CEMPRA"))
  lc$Value[grepl("^cr_", lc$Name)] <- 1
  s <- pop_model_setup(life_cycles = lc)
  m <- pop_model_matrix_elements(s)
  chk <- Projection_DD(M.mx = m$life_stages_symbolic,
                       D.mx = m$density_stage_symbolic,
                       H.mx = NULL, dat = m$life_histories,
                       K = m$life_histories$Ka, Nyears = 5, p.cat = 0,
                       CE_df = NULL,
                       bh_dd_stages = c("bh_stage_1"),
                       stage_k_override = c(NA, 5000, NA, NA, NA),
                       alt_return = "check_k_stage")
  expect_equal(names(chk[[2]]), paste0("K", 1:4))

  # anadromous K vectors keep their stage-class names through the override
  f <- sockeye_kmode()
  chk2 <- Projection_DD(M.mx = f$m$life_stages_symbolic,
                        D.mx = f$m$density_stage_symbolic,
                        H.mx = NULL, dat = f$dat, K = 5000, Nyears = 5,
                        p.cat = 0, CE_df = NULL, anadromous = TRUE,
                        bh_dd_stages = c("bh_stage_b_5"),
                        stage_k_override = f$kover,
                        alt_return = "check_k_stage")
  expect_equal(names(chk2[[2]]), f$dat$stage_names)
})
