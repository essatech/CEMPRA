# Tests for the optional From_Year / To_Year stressor activation window.
# See stressor_year_window_plan.md for the design + semantics.

# --------------------------------------------------------------------------
# Helpers
# --------------------------------------------------------------------------

# Build a minimal non-anadromous rates object of a given length (= n_years + 1)
make_rates <- function(n_len) {
  list(
    ft = replicate(n_len, c(eps = 100), simplify = FALSE),
    st = replicate(n_len, c(sE = 0.9, s0 = 0.5, s1 = 0.6, s2 = 0.7, s3 = 0.8), simplify = FALSE),
    ut = NULL,
    smigt = NULL,
    K_list = replicate(n_len, c(K1 = 1000, K2 = 500, K3 = 200), simplify = FALSE),
    Ke_list = replicate(n_len, 50000, simplify = FALSE),
    K0_list = replicate(n_len, 10000, simplify = FALSE),
    Kspawners_list = replicate(n_len, 0, simplify = FALSE)
  )
}

# Build a CE_df with one row per Year (1..n_years)
make_ce <- function(life_stage, parameter, sys.cap = 0.5, n_years = 5,
                    from = NULL, to = NULL, add_year = TRUE) {
  df <- data.frame(
    life_stage = life_stage,
    parameter = parameter,
    sys.cap = sys.cap,
    Stressor = "test_stressor",
    simulation = 1,
    HUC = 1,
    stringsAsFactors = FALSE
  )
  df <- df[rep(1, n_years), ]
  if (add_year) df$Year <- 1:n_years
  if (!is.null(from)) df$From_Year <- from
  if (!is.null(to))   df$To_Year <- to
  rownames(df) <- NULL
  df
}

# Pull a named survival value across the rates$st list
sE_vec <- function(res) vapply(res$st, function(x) unname(x["sE"]), numeric(1))
s0_vec <- function(res) vapply(res$st, function(x) unname(x["s0"]), numeric(1))


# --------------------------------------------------------------------------
# 1. No window columns -> all years reduced (current behaviour preserved)
# --------------------------------------------------------------------------
test_that("no From/To columns -> stressor applied to all years", {
  rates <- make_rates(6)
  CE_df <- make_ce("stage_e", "survival", sys.cap = 0.5, n_years = 5)
  res <- pop_model_ce_apply_year(rates = rates, CE_df = CE_df,
                                 anadromous = FALSE, mat_stage = c(s3 = 1))
  # base sE 0.9 * 0.5 = 0.45 for every rates entry
  expect_equal(sE_vec(res), rep(0.45, 6))
})


# --------------------------------------------------------------------------
# 2. From + To window -> only in-window years reduced
# --------------------------------------------------------------------------
test_that("From=2,To=4 -> only years 2-4 reduced", {
  rates <- make_rates(6)
  CE_df <- make_ce("stage_e", "survival", sys.cap = 0.5, n_years = 5, from = 2, to = 4)
  res <- pop_model_ce_apply_year(rates = rates, CE_df = CE_df,
                                 anadromous = FALSE, mat_stage = c(s3 = 1))
  # rates slot 1 = setup (carries Y1's value, off); slot t+1 = year t:
  # slots 2 (Y1, off), 3-5 (Y2-4, on), 6 (Y5, off)
  expect_equal(sE_vec(res), c(0.9, 0.9, 0.45, 0.45, 0.45, 0.9))
})


# --------------------------------------------------------------------------
# 3. From only -> active for Year >= From
# --------------------------------------------------------------------------
test_that("From only (=3) -> years >= 3 reduced", {
  rates <- make_rates(6)
  CE_df <- make_ce("stage_e", "survival", sys.cap = 0.5, n_years = 5, from = 3)
  res <- pop_model_ce_apply_year(rates = rates, CE_df = CE_df,
                                 anadromous = FALSE, mat_stage = c(s3 = 1))
  expect_equal(sE_vec(res), c(0.9, 0.9, 0.9, 0.45, 0.45, 0.45))
})


# --------------------------------------------------------------------------
# 4. To only -> active for Year <= To
# --------------------------------------------------------------------------
test_that("To only (=4) -> years <= 4 reduced", {
  rates <- make_rates(6)
  CE_df <- make_ce("stage_e", "survival", sys.cap = 0.5, n_years = 5, to = 4)
  res <- pop_model_ce_apply_year(rates = rates, CE_df = CE_df,
                                 anadromous = FALSE, mat_stage = c(s3 = 1))
  # setup slot + years 1-4 on; year 5 (final slot) off - no wrap-around
  expect_equal(sE_vec(res), c(0.45, 0.45, 0.45, 0.45, 0.45, 0.9))
})


# --------------------------------------------------------------------------
# 5. Columns present but values NA -> applied to all years
# --------------------------------------------------------------------------
test_that("From/To columns present but NA -> applied to all years", {
  rates <- make_rates(6)
  CE_df <- make_ce("stage_e", "survival", sys.cap = 0.5, n_years = 5,
                   from = NA, to = NA)
  res <- pop_model_ce_apply_year(rates = rates, CE_df = CE_df,
                                 anadromous = FALSE, mat_stage = c(s3 = 1))
  expect_equal(sE_vec(res), rep(0.45, 6))
})


# --------------------------------------------------------------------------
# 6. From > To -> empty window, inactive all years + warning
# --------------------------------------------------------------------------
test_that("From > To -> stressor inactive all years + warning", {
  rates <- make_rates(6)
  CE_df <- make_ce("stage_e", "survival", sys.cap = 0.5, n_years = 5, from = 4, to = 2)
  expect_warning(
    res <- pop_model_ce_apply_year(rates = rates, CE_df = CE_df,
                                   anadromous = FALSE, mat_stage = c(s3 = 1)),
    regexp = "From_Year > To_Year"
  )
  expect_equal(sE_vec(res), rep(0.9, 6))
})


# --------------------------------------------------------------------------
# 7. Window columns present but no Year column -> warn, no windowing
# --------------------------------------------------------------------------
test_that("From/To present but no Year column -> warning, applied all years", {
  rates <- make_rates(6)
  # Single constant row (multi-row per-year tables without a Year column
  # are rejected by the duplicate-target check - see
  # test-ce-duplicate-targets.R)
  CE_df <- make_ce("stage_e", "survival", sys.cap = 0.5, n_years = 1,
                   from = 2, to = 4, add_year = FALSE)
  expect_warning(
    res <- pop_model_ce_apply_year(rates = rates, CE_df = CE_df,
                                   anadromous = FALSE, mat_stage = c(s3 = 1)),
    regexp = "no 'Year' column"
  )
  # No windowing applied -> all years reduced
  expect_equal(sE_vec(res), rep(0.45, 6))
})


# --------------------------------------------------------------------------
# 8. Stressor linked to multiple life stages (comma-separated) + window
# --------------------------------------------------------------------------
test_that("multi life-stage stressor (stage_e, stage_0) respects window", {
  rates <- make_rates(6)
  CE_df <- make_ce("stage_e,stage_0", "survival", sys.cap = 0.5, n_years = 5,
                   from = 2, to = 4)
  res <- pop_model_ce_apply_year(rates = rates, CE_df = CE_df,
                                 anadromous = FALSE, mat_stage = c(s3 = 1))
  # sE: 0.9 base -> 0.45 inside window; s0: 0.5 base -> 0.25 inside window
  expect_equal(sE_vec(res), c(0.9, 0.9, 0.45, 0.45, 0.45, 0.9))
  expect_equal(s0_vec(res), c(0.5, 0.5, 0.25, 0.25, 0.25, 0.5))
})


# --------------------------------------------------------------------------
# 9. Capacity stressor respects window
# --------------------------------------------------------------------------
test_that("capacity stressor (stage_1) respects window", {
  rates <- make_rates(6)
  CE_df <- make_ce("stage_1", "capacity", sys.cap = 0.5, n_years = 5, from = 2, to = 4)
  res <- pop_model_ce_apply_year(rates = rates, CE_df = CE_df,
                                 anadromous = FALSE, mat_stage = c(s3 = 1))
  k1 <- vapply(res$K_list, function(x) unname(x["K1"]), numeric(1))
  expect_equal(k1, c(1000, 1000, 500, 500, 500, 1000))
})


# --------------------------------------------------------------------------
# 10. Fecundity stressor respects window
# --------------------------------------------------------------------------
test_that("fecundity stressor (eps) respects window", {
  rates <- make_rates(6)
  CE_df <- make_ce("eps", "fecundity", sys.cap = 0.5, n_years = 5, from = 2, to = 4)
  res <- pop_model_ce_apply_year(rates = rates, CE_df = CE_df,
                                 anadromous = FALSE, mat_stage = c(s3 = 1))
  eps <- vapply(res$ft, function(x) unname(x["eps"]), numeric(1))
  expect_equal(eps, c(100, 100, 50, 50, 50, 100))
})


# --------------------------------------------------------------------------
# 11. Anadromous run: prespawn (u) survival window
# --------------------------------------------------------------------------
test_that("anadromous prespawn (u) stressor respects window", {
  n_len <- 6
  rates <- list(
    ft = replicate(n_len, c(eps4 = 4000, eps5 = 4000), simplify = FALSE),
    st = replicate(n_len, c(sE = 0.9, s0 = 0.5, s1 = 0.6, s2 = 0.7, s3 = 0.8), simplify = FALSE),
    ut = replicate(n_len, c(u4 = 0.9, u5 = 0.9), simplify = FALSE),
    smigt = replicate(n_len, c(smig4 = 0.95, smig5 = 0.95), simplify = FALSE),
    K_list = replicate(n_len, c(stage_Pb_1 = 1000, stage_Pb_2 = 800,
                                stage_B_4 = 200, stage_B_5 = 100), simplify = FALSE),
    Ke_list = replicate(n_len, 50000, simplify = FALSE),
    K0_list = replicate(n_len, 10000, simplify = FALSE),
    Kspawners_list = replicate(n_len, 5000, simplify = FALSE)
  )
  CE_df <- make_ce("u", "survival", sys.cap = 0.5, n_years = 5, from = 2, to = 4)

  res <- pop_model_ce_apply_year(rates = rates, CE_df = CE_df,
                                 anadromous = TRUE, mat_stage = NULL)

  u4 <- vapply(res$ut, function(x) unname(x["u4"]), numeric(1))
  expect_equal(u4, c(0.9, 0.9, 0.45, 0.45, 0.45, 0.9))
})


# --------------------------------------------------------------------------
# 12-13. Import: StressorMagnitudeWorkbook
# --------------------------------------------------------------------------
test_that("import without From/To columns is unchanged (backwards compatible)", {
  fname <- system.file("extdata", "./matrix_test/stressor_magnitude_matrix.xlsx", package = "CEMPRA")
  dose <- StressorMagnitudeWorkbook(filename = fname)
  expect_false("From_Year" %in% colnames(dose))
  expect_false("To_Year" %in% colnames(dose))
  expect_true(all(c("HUC_ID", "Stressor", "Mean", "SD") %in% colnames(dose)))
})

test_that("import with From_Year/To_Year columns coerces to numeric and keeps them", {
  skip_if_not_installed("openxlsx")
  df <- data.frame(
    HUC_ID = c(1, 2, 3),
    NAME = c("a", "b", "c"),
    Stressor = c("Drawdown", "Drawdown", "Drawdown"),
    Stressor_cat = c("x", "x", "x"),
    Mean = c(5, 6, 7),
    SD = c(1, 1, 1),
    Distribution = c("normal", "normal", "normal"),
    Low_Limit = c(0, 0, 0),
    Up_Limit = c(10, 10, 10),
    From_Year = c(50, NA, ""),     # numeric, missing, blank
    To_Year   = c(70, 80, NA),
    stringsAsFactors = FALSE
  )
  tmp <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(df, tmp)

  dose <- StressorMagnitudeWorkbook(filename = tmp)
  expect_true("From_Year" %in% colnames(dose))
  expect_true("To_Year" %in% colnames(dose))
  expect_true(is.numeric(dose$From_Year))
  expect_true(is.numeric(dose$To_Year))
  expect_equal(dose$From_Year[dose$HUC_ID == 1], 50)
  expect_equal(dose$To_Year[dose$HUC_ID == 1], 70)
  expect_true(is.na(dose$From_Year[dose$HUC_ID == 2]))
  expect_true(is.na(dose$From_Year[dose$HUC_ID == 3]))  # blank -> NA
})


# --------------------------------------------------------------------------
# 14-16. Integration via Projection_DD (non-anadromous, deterministic)
# --------------------------------------------------------------------------

# Build a non-anadromous projection setup once
make_proj_setup <- function() {
  filename_lc <- system.file("extdata", "life_cycles.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename_lc)
  pop_mod_setup <- pop_model_setup(life_cycles = life_cycles)
  pop_mod_mat <- pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)
  list(
    dat = pop_mod_mat$life_histories,
    M = pop_mod_mat$life_stages_symbolic,
    D = pop_mod_mat$density_stage_symbolic
  )
}

# A harmful survival stressor across all years (one row per Year)
make_proj_ce <- function(n_years, from = NULL, to = NULL) {
  make_ce("all", "survival", sys.cap = 0.5, n_years = n_years, from = from, to = to)
}

test_that("Projection_DD: fully windowed-out stressor == no stressor (non-anadromous)", {
  s <- make_proj_setup()
  Nyears <- 10

  set.seed(123)
  baseline <- Projection_DD(M.mx = s$M, D.mx = s$D, dat = s$dat,
                            K = s$dat$Ka, Nyears = Nyears, p.cat = 0, CE_df = NULL)

  set.seed(123)
  # From_Year beyond the run -> never active -> equivalent to no CE
  windowed_out <- Projection_DD(M.mx = s$M, D.mx = s$D, dat = s$dat,
                                K = s$dat$Ka, Nyears = Nyears, p.cat = 0,
                                CE_df = make_proj_ce(Nyears, from = 9999))

  expect_equal(windowed_out$pop$N, baseline$pop$N)
})

test_that("Projection_DD: full window (From=1,To=Nyears) == no-window stressor", {
  s <- make_proj_setup()
  Nyears <- 10

  set.seed(7)
  full_window <- Projection_DD(M.mx = s$M, D.mx = s$D, dat = s$dat,
                               K = s$dat$Ka, Nyears = Nyears, p.cat = 0,
                               CE_df = make_proj_ce(Nyears, from = 1, to = Nyears))

  set.seed(7)
  no_window <- Projection_DD(M.mx = s$M, D.mx = s$D, dat = s$dat,
                             K = s$dat$Ka, Nyears = Nyears, p.cat = 0,
                             CE_df = make_proj_ce(Nyears))

  expect_equal(full_window$pop$N, no_window$pop$N)
})

test_that("Projection_DD: partial window changes the trajectory vs no stressor", {
  s <- make_proj_setup()
  Nyears <- 10

  set.seed(99)
  baseline <- Projection_DD(M.mx = s$M, D.mx = s$D, dat = s$dat,
                            K = s$dat$Ka, Nyears = Nyears, p.cat = 0, CE_df = NULL)

  set.seed(99)
  windowed <- Projection_DD(M.mx = s$M, D.mx = s$D, dat = s$dat,
                            K = s$dat$Ka, Nyears = Nyears, p.cat = 0,
                            CE_df = make_proj_ce(Nyears, from = 1, to = 5))

  # An active stressor in years 1-5 must perturb the trajectory
  expect_false(isTRUE(all.equal(windowed$pop$N, baseline$pop$N)))
})


# --------------------------------------------------------------------------
# 17. Integration via PopulationModel_Run: plumbing + backwards compat
#     (anadromous matrix fixture, known-good setup)
# --------------------------------------------------------------------------
test_that("PopulationModel_Run: all-NA From/To columns do not change results", {
  filename <- system.file("extdata/simple_test/test_anadromous/stage_3_anadromous.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename)

  filename_sm <- system.file("extdata", "./matrix_test/stressor_magnitude_matrix.xlsx", package = "CEMPRA")
  dose <- StressorMagnitudeWorkbook(filename = filename_sm)
  filename_sr <- system.file("extdata", "./matrix_test/stressor_response_matrix_AB.xlsx", package = "CEMPRA")
  sr_wb_dat <- StressorResponseWorkbook(filename = filename_sr)

  run_args <- list(
    sr_wb_dat = sr_wb_dat,
    life_cycle_params = life_cycles,
    HUC_ID = 2,
    n_years = 30,
    MC_sims = 3,
    stressors = NA
  )

  set.seed(2024)
  out_plain <- do.call(PopulationModel_Run, c(list(dose = dose), run_args))

  dose_na <- dose
  dose_na$From_Year <- NA
  dose_na$To_Year <- NA

  set.seed(2024)
  out_na <- do.call(PopulationModel_Run, c(list(dose = dose_na), run_args))

  n_plain <- do.call(rbind, lapply(out_plain$ce, function(x) x$pop$N))
  n_na    <- do.call(rbind, lapply(out_na$ce,    function(x) x$pop$N))
  expect_equal(n_na, n_plain)
})

test_that("PopulationModel_Run: fully windowed-out stressors reduce impact", {
  filename <- system.file("extdata/simple_test/test_anadromous/stage_3_anadromous.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename)

  filename_sm <- system.file("extdata", "./matrix_test/stressor_magnitude_matrix.xlsx", package = "CEMPRA")
  dose <- StressorMagnitudeWorkbook(filename = filename_sm)
  filename_sr <- system.file("extdata", "./matrix_test/stressor_response_matrix_AB.xlsx", package = "CEMPRA")
  sr_wb_dat <- StressorResponseWorkbook(filename = filename_sr)

  run_args <- list(
    sr_wb_dat = sr_wb_dat,
    life_cycle_params = life_cycles,
    HUC_ID = 2,
    n_years = 50,
    MC_sims = 3,
    stressors = NA
  )

  set.seed(55)
  out_ce <- do.call(PopulationModel_Run, c(list(dose = dose), run_args))

  dose_off <- dose
  dose_off$From_Year <- 9999   # never active anywhere
  dose_off$To_Year <- NA

  set.seed(55)
  out_off <- do.call(PopulationModel_Run, c(list(dose = dose_off), run_args))

  mean_ce  <- mean(vapply(out_ce$ce,  function(x) mean(x$pop$N), numeric(1)))
  mean_off <- mean(vapply(out_off$ce, function(x) mean(x$pop$N), numeric(1)))

  # Harmful stressors switched off -> population should do at least as well
  expect_true(mean_off >= mean_ce)
})
