# Regression tests for the CE sys.cap year-alignment fix.
#
# The stochastic rate lists in Projection_DD() have Nyears + 1 slots:
# slot 1 is the initialization ("year 0") entry and slot t + 1 holds
# simulation year t. Historically the per-year sys.cap vector was padded at
# the END (c(sys.cap, sys.cap[1])), which (a) applied Year t+1's stressor
# magnitude in simulation year t and (b) wrapped Year 1's magnitude onto
# the final simulated year; length mismatches were then "repaired" with
# sample(replace = TRUE), randomly shuffling the temporal sequence.
# align_sys_caps() replaces all of that with explicit Year matching.

# --- local helpers (self-contained) ---------------------------------------
yal_rates <- function(n_len) {
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

yal_ce <- function(life_stage, parameter, sys.cap, years = seq_along(sys.cap),
                   add_year = TRUE) {
  df <- data.frame(
    life_stage = life_stage, parameter = parameter, sys.cap = sys.cap,
    Stressor = "test_stressor", simulation = 1, HUC = 1,
    stringsAsFactors = FALSE
  )
  if (add_year) df$Year <- years
  rownames(df) <- NULL
  df
}

yal_sE <- function(res) vapply(res$st, function(x) unname(x["sE"]), numeric(1))


test_that("single-year pulse lands in exactly that year (no one-year shift)", {
  rates <- yal_rates(6)
  # sys.cap 0.5 in Year 3 only; 5 simulation years
  ce <- yal_ce("stage_e", "survival", sys.cap = c(1, 1, 0.5, 1, 1))
  res <- pop_model_ce_apply_year(rates = rates, CE_df = ce,
                                 anadromous = FALSE, mat_stage = c(s3 = 1))
  # slot 4 (= simulation year 3) is the only reduced projection year
  expect_equal(yal_sE(res), c(0.9, 0.9, 0.9, 0.45, 0.9, 0.9))
})


test_that("Year 1 pulse does not wrap onto the final simulated year", {
  rates <- yal_rates(6)
  ce <- yal_ce("stage_e", "survival", sys.cap = c(0.5, 1, 1, 1, 1))
  res <- pop_model_ce_apply_year(rates = rates, CE_df = ce,
                                 anadromous = FALSE, mat_stage = c(s3 = 1))
  # setup slot + year 1 reduced; final year (slot 6) untouched
  expect_equal(yal_sE(res), c(0.45, 0.45, 0.9, 0.9, 0.9, 0.9))
})


test_that("alignment matches on the Year column, not on row order", {
  rates <- yal_rates(6)
  # Same pulse as above, rows supplied in reverse year order
  ce <- yal_ce("stage_e", "survival",
               sys.cap = c(1, 1, 0.5, 1, 1)[5:1], years = 5:1)
  res <- pop_model_ce_apply_year(rates = rates, CE_df = ce,
                                 anadromous = FALSE, mat_stage = c(s3 = 1))
  expect_equal(yal_sE(res), c(0.9, 0.9, 0.9, 0.45, 0.9, 0.9))
})


test_that("missing years are treated as unstressed with a warning (no shuffling)", {
  rates <- yal_rates(6)
  # Year 3 row absent; remaining years stressed at 0.5
  ce <- yal_ce("stage_e", "survival",
               sys.cap = c(0.5, 0.5, 0.5, 0.5), years = c(1, 2, 4, 5))
  expect_warning(
    res <- pop_model_ce_apply_year(rates = rates, CE_df = ce,
                                   anadromous = FALSE, mat_stage = c(s3 = 1)),
    regexp = "missing sys.cap values"
  )
  # Year 3 (slot 4) neutral; all other years reduced
  expect_equal(yal_sE(res), c(0.45, 0.45, 0.45, 0.9, 0.45, 0.45))
})


test_that("constant single-row stressor (no Year column) applies to all years", {
  rates <- yal_rates(6)
  ce <- yal_ce("stage_e", "survival", sys.cap = 0.5, add_year = FALSE)
  res <- pop_model_ce_apply_year(rates = rates, CE_df = ce,
                                 anadromous = FALSE, mat_stage = c(s3 = 1))
  expect_equal(yal_sE(res), rep(0.45, 6))
})


test_that("capacity pathway uses the same corrected alignment", {
  rates <- yal_rates(6)
  ce <- yal_ce("stage_1", "capacity", sys.cap = c(1, 1, 0.5, 1, 1))
  res <- pop_model_ce_apply_year(rates = rates, CE_df = ce,
                                 anadromous = FALSE, mat_stage = c(s3 = 1))
  k1 <- vapply(res$K_list, function(x) unname(x["K1"]), numeric(1))
  expect_equal(k1, c(1000, 1000, 1000, 500, 1000, 1000))
})


test_that("conflicting duplicate rows without Year info are a hard error", {
  rates <- yal_rates(6)
  # Two rows, same stressor / life stage / parameter, different scores,
  # no Year column: duplicate targeting - refuse rather than guess
  ce <- yal_ce("stage_e", "survival", sys.cap = c(0.3, 0.4), add_year = FALSE)
  expect_error(
    pop_model_ce_apply_year(rates = rates, CE_df = ce,
                            anadromous = FALSE, mat_stage = c(s3 = 1)),
    regexp = "same vital rate more than once"
  )
})


test_that("duplicate rows for the same year are a hard error", {
  rates <- yal_rates(6)
  ce <- yal_ce("stage_e", "survival",
               sys.cap = c(0.5, 0.5, 0.3, 0.4, 0.5, 0.5),
               years = c(1, 2, 3, 3, 4, 5))
  expect_error(
    pop_model_ce_apply_year(rates = rates, CE_df = ce,
                            anadromous = FALSE, mat_stage = c(s3 = 1)),
    regexp = "same vital rate more than once"
  )
})


test_that("two distinctly named stressors on the same endpoint multiply", {
  rates <- yal_rates(6)
  ce <- rbind(
    yal_ce("s1", "survival", sys.cap = 0.3, add_year = FALSE),
    yal_ce("s1", "survival", sys.cap = 0.4, add_year = FALSE)
  )
  ce$Stressor <- c("sediment", "temperature")
  res <- pop_model_ce_apply_year(rates = rates, CE_df = ce,
                                 anadromous = FALSE, mat_stage = c(s3 = 1))
  s1 <- vapply(res$st, function(x) unname(x["s1"]), numeric(1))
  expect_equal(s1, rep(0.6 * 0.3 * 0.4, 6))   # effects compound
})


test_that("end-to-end: constant stressor produces uniform yearly reduction", {
  # A constant stressor must reduce every projection year identically -
  # the alignment change is invisible for the (most common) constant case.
  filename_lc <- system.file("extdata", "life_cycles.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename_lc)
  setup <- pop_model_setup(life_cycles = life_cycles)
  mat <- pop_model_matrix_elements(pop_mod_setup = setup)
  Nyears <- 10

  ce_const <- yal_ce("all", "survival", sys.cap = rep(0.5, Nyears))
  ce_single <- yal_ce("all", "survival", sys.cap = 0.5, add_year = FALSE)

  set.seed(11)
  run_const <- Projection_DD(M.mx = mat$life_stages_symbolic,
                             D.mx = mat$density_stage_symbolic,
                             dat = mat$life_histories, K = mat$life_histories$Ka,
                             Nyears = Nyears, p.cat = 0, CE_df = ce_const)
  set.seed(11)
  run_single <- Projection_DD(M.mx = mat$life_stages_symbolic,
                              D.mx = mat$density_stage_symbolic,
                              dat = mat$life_histories, K = mat$life_histories$Ka,
                              Nyears = Nyears, p.cat = 0, CE_df = ce_single)

  # One value per year (all 0.5) and a single constant row are equivalent
  expect_equal(run_const$pop$N, run_single$pop$N)
})
