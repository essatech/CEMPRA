# 2026-09-23 ruling batch 2:
# - habitat capacity columns validated (unrecognized k_ names error;
#   case-insensitive input accepted)
# - CE life-stage tags are model-mode dependent (pop_model_ce_validate)
# - s_rand() returns 0 (not NA) for a mean survival of exactly 0
# - blank survival rows error up front naming the row

test_that("unrecognized k_ habitat columns error; case-insensitive names pass", {
  hd <- data.frame(HUC_ID = 1, NAME = "x",
                   K_Stage_1_Mean = 100, k_stage_2_mean = 50)
  out <- pop_model_hab_dens_clean(hd)
  expect_true(all(c("k_stage_1_mean", "k_stage_2_mean") %in% colnames(out)))

  hd_bad <- data.frame(HUC_ID = 1, NAME = "x",
                       k_stage_1_mean = 100, k_notes = "scratch")
  expect_error(pop_model_hab_dens_clean(hd_bad), "k_notes")

  # spawner variants still accepted
  hd_sp <- data.frame(HUC_ID = 1, NAME = "x", k_spawners = 500)
  out_sp <- pop_model_hab_dens_clean(hd_sp)
  expect_true("k_stage_B_mean" %in% colnames(out_sp))
})

test_that("CE tag validation is model-mode dependent", {
  # salmon-only survival tags valid only in anadromous mode
  expect_equal(pop_model_ce_validate("spawners", "survival", TRUE), "")
  expect_match(pop_model_ce_validate("spawners", "survival", FALSE),
               "not recognized")
  expect_match(pop_model_ce_validate("smig_3", "survival", FALSE),
               "non-anadromous")
  # age-specific spawner capacity tags are salmon-only
  expect_equal(pop_model_ce_validate("b4", "capacity", TRUE), "")
  expect_match(pop_model_ce_validate("b4", "capacity", FALSE),
               "not recognized")
  # common tags valid in both modes
  expect_equal(pop_model_ce_validate("stage_1, stage_2", "survival", FALSE), "")
  expect_equal(pop_model_ce_validate("eps_2", "fecundity", FALSE), "")
  expect_equal(pop_model_ce_validate("adult", "capacity", FALSE), "")
  # garbage flagged either mode
  expect_match(pop_model_ce_validate("jlkjkj", "capacity", FALSE),
               "jlkjkj")
  expect_match(pop_model_ce_validate("stage_1", "growth", TRUE),
               "unrecognized parameter")
  # Joe-Model-only rows (no parameter) pass
  expect_equal(pop_model_ce_validate("adult", NA, FALSE), "")
})

test_that("the non-anadromous apply path warns on salmon-only tags", {
  # ce_known_tags is the shared source of truth for the run-time warning
  known_na <- CEMPRA:::ce_known_tags(FALSE)
  known_an <- CEMPRA:::ce_known_tags(TRUE)
  expect_false("u" %in% known_na$surv)
  expect_true("u" %in% known_an$surv)
  expect_false("b4" %in% known_na$cap)
  expect_true("b4" %in% known_an$cap)
  expect_true("adult" %in% known_na$surv)
})

test_that("s_rand returns 0 for a mean survival of exactly 0", {
  set.seed(3)
  # (the zero-mean branch produces a benign NaN warning internally
  # before the repair gate returns 0)
  out <- suppressWarnings(CEMPRA:::s_rand(mn = c(0, 0.5, 1), cv = 0.1, rho = 0))
  expect_equal(out[1], 0)
  expect_true(out[2] > 0 && out[2] < 1)
  expect_equal(out[3], 1)
})

test_that("a blank survival row errors up front naming the row", {
  flc <- system.file("extdata", "life_cycles.csv", package = "CEMPRA")
  lc <- read.csv(flc)
  lc$Value[lc$Name == "surv_3"] <- NA
  expect_error(pop_model_setup(life_cycles = lc), "surv_3")

  fan <- system.file("extdata/simple_test/test_anadromous/chinook_life_cycle.csv",
                     package = "CEMPRA")
  lca <- read.csv(fan)
  lca$Value[lca$Name == "surv_2"] <- NA
  expect_error(suppressWarnings(pop_model_setup(life_cycles = lca)), "surv_2")
})
