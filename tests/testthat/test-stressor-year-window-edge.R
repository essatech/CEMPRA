# Additional checks + edge cases for the From_Year / To_Year activation window.

# --- local helpers (self-contained) ---------------------------------------
ewin_rates <- function(n_len) {
  list(
    ft = replicate(n_len, c(eps = 100), simplify = FALSE),
    st = replicate(n_len, c(sE = 0.9, s0 = 0.5, s1 = 0.6, s2 = 0.7, s3 = 0.8), simplify = FALSE),
    ut = NULL, smigt = NULL,
    K_list = replicate(n_len, c(K1 = 1000, K2 = 500, K3 = 200), simplify = FALSE),
    Ke_list = replicate(n_len, 50000, simplify = FALSE),
    K0_list = replicate(n_len, 10000, simplify = FALSE),
    Kspawners_list = replicate(n_len, 0, simplify = FALSE)
  )
}
ewin_ce <- function(life_stage, parameter, sys.cap = 0.5, n_years = 5,
                    from = NULL, to = NULL, stressor = "test_stressor", add_year = TRUE) {
  df <- data.frame(life_stage = life_stage, parameter = parameter, sys.cap = sys.cap,
                   Stressor = stressor, simulation = 1, HUC = 1, stringsAsFactors = FALSE)
  df <- df[rep(1, n_years), ]
  if (add_year) df$Year <- 1:n_years
  if (!is.null(from)) df$From_Year <- from
  if (!is.null(to))   df$To_Year <- to
  rownames(df) <- NULL
  df
}
ewin_sE <- function(res) vapply(res$st, function(x) unname(x["sE"]), numeric(1))
ewin_s0 <- function(res) vapply(res$st, function(x) unname(x["s0"]), numeric(1))


# ==========================================================================
# CHECK 1: Standard Joe Model is not affected by From_Year/To_Year columns
# ==========================================================================
test_that("standalone JoeModel_Run is unaffected by From_Year/To_Year columns", {
  filename_rm <- system.file("extdata", "stressor_magnitude_unc_ARTR.xlsx", package = "CEMPRA")
  filename_sr <- system.file("extdata", "stressor_response_fixed_ARTR.xlsx", package = "CEMPRA")
  dose <- StressorMagnitudeWorkbook(filename = filename_rm, scenario_worksheet = "natural_unc")
  sr_wb_dat <- StressorResponseWorkbook(filename = filename_sr)

  set.seed(1)
  jm_plain <- suppressWarnings(JoeModel_Run(dose = dose, sr_wb_dat = sr_wb_dat, MC_sims = 5))

  dose2 <- dose
  dose2$From_Year <- 5
  dose2$To_Year <- 10
  set.seed(1)
  jm_win <- suppressWarnings(JoeModel_Run(dose = dose2, sr_wb_dat = sr_wb_dat, MC_sims = 5))

  expect_equal(jm_win$sc.dose.df, jm_plain$sc.dose.df)
  expect_equal(jm_win$ce.df, jm_plain$ce.df)
})


# ==========================================================================
# CHECK 2: Total_Mortality consolidation tolerates From/To columns
# ==========================================================================
test_that("Total_Mortality import handles From_Year/To_Year without breaking de-dup", {
  skip_if_not_installed("openxlsx")
  df <- data.frame(
    HUC_ID = c(1, 1, 2),
    NAME = c("x", "x", "y"),
    Stressor = c("road", "fines", "road"),
    Stressor_cat = c("Total_Mortality", "Total_Mortality", "Total_Mortality"),
    Mean = c(0.1, 0.2, 0.15), SD = c(0, 0, 0),
    Distribution = c("normal", "normal", "normal"),
    Low_Limit = c(0, 0, 0), Up_Limit = c(1, 1, 1),
    From_Year = c(5, 99, NA),   # differing values on same HUC would break naive de-dup
    To_Year = c(10, NA, NA),
    stringsAsFactors = FALSE
  )
  tmp <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(df, tmp)

  out <- StressorMagnitudeWorkbook(filename = tmp)
  expect_s3_class(out, "data.frame")             # NOT the error string
  expect_false(any(duplicated(out$HUC_ID)))      # one consolidated row per HUC
  expect_true(all(is.na(out$From_Year)))         # window dropped on Total_Mortality
  expect_true(all(is.na(out$To_Year)))
})


# ==========================================================================
# CHECK 3: Non-anadromous CAPACITY link respects the window (BH mode)
#   Capacity stressors only bite in Beverton-Holt / habitat-K mode, so test
#   there. Fully windowed-out capacity stressor == no capacity stressor.
# ==========================================================================
test_that("non-anadromous capacity window (BH mode): windowed-out == no stressor", {
  filename_lc <- system.file("extdata", "life_cycles.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename_lc)
  setup <- pop_model_setup(life_cycles = life_cycles)
  mat <- pop_model_matrix_elements(pop_mod_setup = setup)
  dat <- mat$life_histories
  Nstage <- dat$Nstage
  Nyears <- 10

  sko <- rep(NA_real_, Nstage + 1)
  names(sko) <- c("K0", paste0("K", 1:Nstage))
  sko["K1"] <- 3000
  bh <- c("bh_stage_1")

  set.seed(5)
  base <- Projection_DD(M.mx = mat$life_stages_symbolic, D.mx = mat$density_stage_symbolic,
                        dat = dat, K = dat$Ka, Nyears = Nyears, p.cat = 0,
                        CE_df = NULL, stage_k_override = sko, bh_dd_stages = bh)

  ce <- ewin_ce("stage_1", "capacity", sys.cap = 0.3, n_years = Nyears, from = 9999)
  set.seed(5)
  win_out <- Projection_DD(M.mx = mat$life_stages_symbolic, D.mx = mat$density_stage_symbolic,
                           dat = dat, K = dat$Ka, Nyears = Nyears, p.cat = 0,
                           CE_df = ce, stage_k_override = sko, bh_dd_stages = bh)

  expect_equal(win_out$N, base$N)

  # And an ACTIVE capacity window should change the trajectory
  ce2 <- ewin_ce("stage_1", "capacity", sys.cap = 0.3, n_years = Nyears, from = 1, to = Nyears)
  set.seed(5)
  win_in <- Projection_DD(M.mx = mat$life_stages_symbolic, D.mx = mat$density_stage_symbolic,
                          dat = dat, K = dat$Ka, Nyears = Nyears, p.cat = 0,
                          CE_df = ce2, stage_k_override = sko, bh_dd_stages = bh)
  expect_false(isTRUE(all.equal(win_in$N, base$N)))
})


# ==========================================================================
# CHECK 4: Non-anadromous SURVIVORSHIP link respects the window (comp-ratio)
# ==========================================================================
test_that("non-anadromous survival window (comp-ratio): windowed-out == no stressor", {
  filename_lc <- system.file("extdata", "life_cycles.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename_lc)
  setup <- pop_model_setup(life_cycles = life_cycles)
  mat <- pop_model_matrix_elements(pop_mod_setup = setup)
  dat <- mat$life_histories
  Nyears <- 10

  set.seed(8)
  base <- Projection_DD(M.mx = mat$life_stages_symbolic, D.mx = mat$density_stage_symbolic,
                        dat = dat, K = dat$Ka, Nyears = Nyears, p.cat = 0, CE_df = NULL)

  ce <- ewin_ce("all", "survival", sys.cap = 0.5, n_years = Nyears, from = 9999)
  set.seed(8)
  win_out <- Projection_DD(M.mx = mat$life_stages_symbolic, D.mx = mat$density_stage_symbolic,
                           dat = dat, K = dat$Ka, Nyears = Nyears, p.cat = 0, CE_df = ce)
  expect_equal(win_out$pop$N, base$pop$N)
})


# ==========================================================================
# CHECK 5: Anadromous CAPACITY link (spawners) respects the window (unit)
# ==========================================================================
test_that("anadromous spawner capacity window neutralizes correctly", {
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
  CE_df <- ewin_ce("spawners", "capacity", sys.cap = 0.5, n_years = 5, from = 2, to = 4)
  res <- pop_model_ce_apply_year(rates = rates, CE_df = CE_df, anadromous = TRUE, mat_stage = NULL)

  ksp <- vapply(res$Kspawners_list, function(x) unname(x), numeric(1))
  expect_equal(ksp, c(5000, 2500, 2500, 2500, 5000, 5000))
  # stage_B_ entries in K_list also reduced inside window only
  kB4 <- vapply(res$K_list, function(x) unname(x["stage_B_4"]), numeric(1))
  expect_equal(kB4, c(200, 100, 100, 100, 200, 200))
})


# ==========================================================================
# CHECK 6: Anadromous SURVIVORSHIP link (smig) respects the window (unit)
# ==========================================================================
test_that("anadromous spawner-migration (smig) survival window neutralizes correctly", {
  n_len <- 6
  rates <- list(
    ft = replicate(n_len, c(eps4 = 4000), simplify = FALSE),
    st = replicate(n_len, c(sE = 0.9, s0 = 0.5, s1 = 0.6), simplify = FALSE),
    ut = replicate(n_len, c(u4 = 0.9), simplify = FALSE),
    smigt = replicate(n_len, c(smig4 = 0.8), simplify = FALSE),
    K_list = replicate(n_len, c(stage_Pb_1 = 1000, stage_B_4 = 200), simplify = FALSE),
    Ke_list = replicate(n_len, 50000, simplify = FALSE),
    K0_list = replicate(n_len, 10000, simplify = FALSE),
    Kspawners_list = replicate(n_len, 5000, simplify = FALSE)
  )
  CE_df <- ewin_ce("smig", "survival", sys.cap = 0.5, n_years = 5, from = 3, to = 5)
  res <- pop_model_ce_apply_year(rates = rates, CE_df = CE_df, anadromous = TRUE, mat_stage = NULL)
  smig <- vapply(res$smigt, function(x) unname(x["smig4"]), numeric(1))
  # years 3,4,5 reduced (0.4); year 1,2 and filler(=Y1) untouched (0.8)
  expect_equal(smig, c(0.8, 0.8, 0.4, 0.4, 0.4, 0.8))
})


# ==========================================================================
# EDGE CASES
# ==========================================================================

test_that("edge: multiple stressors with different windows each respect their own", {
  rates <- ewin_rates(6)
  a <- ewin_ce("stage_e", "survival", sys.cap = 0.5, n_years = 5, from = 2, to = 3, stressor = "A")
  b <- ewin_ce("stage_0", "survival", sys.cap = 0.5, n_years = 5, from = 4, to = 5, stressor = "B")
  res <- pop_model_ce_apply_year(rates = rates, CE_df = rbind(a, b),
                                 anadromous = FALSE, mat_stage = c(s3 = 1))
  expect_equal(ewin_sE(res), c(0.9, 0.45, 0.45, 0.9, 0.9, 0.9))   # A: years 2-3
  expect_equal(ewin_s0(res), c(0.5, 0.5, 0.5, 0.25, 0.25, 0.5))   # B: years 4-5
})

test_that("edge: From == To activates a single year", {
  rates <- ewin_rates(6)
  ce <- ewin_ce("stage_e", "survival", sys.cap = 0.5, n_years = 5, from = 3, to = 3)
  res <- pop_model_ce_apply_year(rates = rates, CE_df = ce, anadromous = FALSE, mat_stage = c(s3 = 1))
  expect_equal(ewin_sE(res), c(0.9, 0.9, 0.45, 0.9, 0.9, 0.9))
})

test_that("edge: To_Year beyond n_years extends to end of run", {
  rates <- ewin_rates(6)
  ce <- ewin_ce("stage_e", "survival", sys.cap = 0.5, n_years = 5, from = 3, to = 99)
  res <- pop_model_ce_apply_year(rates = rates, CE_df = ce, anadromous = FALSE, mat_stage = c(s3 = 1))
  expect_equal(ewin_sE(res), c(0.9, 0.9, 0.45, 0.45, 0.45, 0.9))
})

test_that("edge: From_Year <= 0 behaves as active from year 1", {
  rates <- ewin_rates(6)
  ce <- ewin_ce("stage_e", "survival", sys.cap = 0.5, n_years = 5, from = -5)
  res <- pop_model_ce_apply_year(rates = rates, CE_df = ce, anadromous = FALSE, mat_stage = c(s3 = 1))
  expect_equal(ewin_sE(res), rep(0.45, 6))
})

test_that("edge: fractional From_Year (2.5) excludes years below it", {
  rates <- ewin_rates(6)
  ce <- ewin_ce("stage_e", "survival", sys.cap = 0.5, n_years = 5, from = 2.5)
  res <- pop_model_ce_apply_year(rates = rates, CE_df = ce, anadromous = FALSE, mat_stage = c(s3 = 1))
  # years 1,2 below 2.5 -> off; years 3-5 -> on
  expect_equal(ewin_sE(res), c(0.9, 0.9, 0.45, 0.45, 0.45, 0.9))
})

test_that("edge: character From_Year/To_Year values are coerced", {
  rates <- ewin_rates(6)
  ce <- ewin_ce("stage_e", "survival", sys.cap = 0.5, n_years = 5, from = "2", to = "4")
  res <- pop_model_ce_apply_year(rates = rates, CE_df = ce, anadromous = FALSE, mat_stage = c(s3 = 1))
  expect_equal(ewin_sE(res), c(0.9, 0.45, 0.45, 0.45, 0.9, 0.9))
})

test_that("edge: only some stressors carry window values (mixed NA)", {
  rates <- ewin_rates(6)
  # Stressor A windowed; Stressor B has NA window (applies all years)
  a <- ewin_ce("stage_e", "survival", sys.cap = 0.5, n_years = 5, from = 2, to = 3, stressor = "A")
  b <- ewin_ce("stage_0", "survival", sys.cap = 0.5, n_years = 5, from = NA, to = NA, stressor = "B")
  res <- pop_model_ce_apply_year(rates = rates, CE_df = rbind(a, b),
                                 anadromous = FALSE, mat_stage = c(s3 = 1))
  expect_equal(ewin_sE(res), c(0.9, 0.45, 0.45, 0.9, 0.9, 0.9))   # A windowed
  expect_equal(ewin_s0(res), rep(0.25, 6))                         # B all years
})
