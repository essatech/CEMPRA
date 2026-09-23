# The s0 lambda-calibration gate (issues 1.5 + 1.13, fixed 2026-09-22).
#
# Design ruling (Matt, 2026-09-22): the model is almost always run in
# location-specific K (habitat-capacity) mode; s0 is calibrated to give
# lambda = 1 ONLY when the run is truly in compensation-ratio mode. The
# gate recognizes every DD flag spelling (bh_/hs_, any life stage,
# spawners, dd_hs_0) and checks flag VALUES: a row set to 0/blank is
# disabled (2026-09-16 ruling), so a profile whose flag rows are all
# disabled runs in CR mode and IS calibrated. Previously the gate was a
# presence-only grep("bh_stage_"), which (a) skipped the calibration for
# disabled flags - CR runs silently equilibrated ~2.7x above the stated
# k - and (b) missed the hs_/spawners/dd_hs_0 spellings entirely.
# dd_flag_enabled() is now also case-insensitive ("true" == "TRUE",
# issue 1.13), matching explain_dd_settings().

lc_base <- read.csv(system.file("extdata", "life_cycles.csv", package = "CEMPRA"))

s0_of <- function(lcx) {
  s <- pop_model_setup(life_cycles = lcx)
  m <- pop_model_matrix_elements(s)
  unname(m$life_histories$S[["s0"]])
}
addrow <- function(lcx, nm, v) {
  rbind(lcx, data.frame(Parameters = nm, Name = nm, Value = v))
}
S0_RAW <- 0.3
S0_CAL <- 0.07443813

test_that("truly-CR profiles are calibrated (incl. disabled flag rows)", {
  # clean CR profile
  expect_equal(s0_of(lc_base), S0_CAL, tolerance = 1e-6)

  # flag rows present but DISABLED (Value = 0) -> still CR mode -> calibrated.
  # (Presence-only gate left s0 raw here: equilibrium ran ~2.7x above k.)
  lc2 <- addrow(addrow(lc_base, "bh_stage_1", 0), "bh_stage_2", 0)
  expect_equal(s0_of(lc2), S0_CAL, tolerance = 1e-6)

  # and the CR run genuinely equilibrates at the stated k again
  s <- pop_model_setup(life_cycles = lc2)
  m <- pop_model_matrix_elements(s)
  set.seed(42)
  r <- Projection_DD(M.mx = m$life_stages_symbolic,
                     D.mx = m$density_stage_symbolic,
                     H.mx = NULL, dat = m$life_histories,
                     K = m$life_histories$Ka,
                     Nyears = 300, p.cat = 0, CE_df = NULL)
  eq <- median(r$pop$N[100:300])
  expect_true(eq > 80 & eq < 120)   # stated k = 100 (was ~273 pre-fix)
})

test_that("every enabled DD flag spelling disables the calibration", {
  for (flag in c("bh_stage_1", "hs_stage_1", "bh_stage_0", "hs_stage_0",
                 "dd_hs_0", "bh_spawners", "hs_spawners")) {
    expect_equal(s0_of(addrow(lc_base, flag, 1)), S0_RAW,
                 label = paste0("s0 with ", flag, " = 1"))
  }
})

test_that("flag values are case-insensitive (issue 1.13)", {
  lct <- lc_base
  lct[] <- lapply(lct, as.character)
  for (v in c("TRUE", "true", "True", " true ")) {
    expect_equal(s0_of(addrow(lct, "hs_stage_2", v)), S0_RAW,
                 label = paste0("s0 with hs_stage_2 = '", v, "'"))
  }
  # and disabled spellings stay disabled
  for (v in c("0", "FALSE", "false", "")) {
    lcx <- addrow(lct, "hs_stage_2", v)
    expect_equal(s0_of(lcx), S0_CAL, tolerance = 1e-6,
                 label = paste0("s0 with hs_stage_2 = '", v, "'"))
  }
})

test_that("dd_flag_enabled accepts lowercase true and rejects junk", {
  mk <- function(v) data.frame(Name = "bh_stage_1", Value = v)
  expect_true(dd_flag_enabled(mk("TRUE"), "bh_stage_1"))
  expect_true(dd_flag_enabled(mk("true"), "bh_stage_1"))
  expect_true(dd_flag_enabled(mk(" True "), "bh_stage_1"))
  expect_true(dd_flag_enabled(mk(1), "bh_stage_1"))
  expect_false(dd_flag_enabled(mk(0), "bh_stage_1"))
  expect_false(dd_flag_enabled(mk("FALSE"), "bh_stage_1"))
  expect_false(dd_flag_enabled(mk(NA), "bh_stage_1"))
  expect_false(dd_flag_enabled(mk("yes"), "bh_stage_1"))
})

test_that("anadromous K-mode profiles keep raw s0 (unchanged behavior)", {
  lc_so <- read.csv(system.file("extdata/sockeye/life_cycles_sockeye.csv",
                                package = "CEMPRA"))
  s <- pop_model_setup(life_cycles = lc_so)
  m <- pop_model_matrix_elements(s)
  # sockeye profile: bh flags all TRUE -> raw S0 (0.104) preserved
  expect_equal(unname(m$life_histories$S[["s0"]]), 0.104)
})
