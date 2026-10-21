# Interaction matrices in the population model (added 2026-09-23):
# - matrices from the workbook apply by default
# - the `stressors` argument can select matrices (MInt_ names); any
#   combination of mains / matrix / both / neither works
# - Main_Effect is IGNORED (no absorption): mains and matrix combine
#   multiplicatively when both selected
# - matrix CE rows are tagged from the MInt_ sheet's Life_stages /
#   Parameters header cells

fr <- system.file("extdata/matrix_test/stressor_magnitude_matrix.xlsx", package = "CEMPRA")
fs <- system.file("extdata/matrix_test/stressor_response_matrix.xlsx", package = "CEMPRA")
flc <- system.file("extdata", "life_cycles.csv", package = "CEMPRA")

dose_fix <- StressorMagnitudeWorkbook(filename = fr)
sr_fix <- suppressWarnings(StressorResponseWorkbook(filename = fs))
life_cycles <- read.csv(flc)

# Tag MInt_AB for the population model: stage-1 survival
sr_pop <- sr_fix
sr_pop$MInt$MInt_AB$Life_stages <- "stage_1"
sr_pop$MInt$MInt_AB$Parameters <- "survival"

run_pop <- function(sr = sr_pop, seed = 11, huc = 1, ...) {
  set.seed(seed)
  suppressWarnings(suppressMessages(
    PopulationModel_Run(
      dose = dose_fix,
      sr_wb_dat = sr,
      life_cycle_params = life_cycles,
      HUC_ID = huc,
      n_years = 10,
      MC_sims = 2,
      ...
    )
  ))
}

final_N <- function(res, which = "ce") {
  mean(vapply(res[[which]], function(x) tail(x$pop$N, 1), 1))
}

test_that("matrix-only pop run applies the matrix (CE < baseline)", {
  res <- run_pop(stressors = "MInt_AB")
  expect_equal(length(res$ce), 2) # MC_sims replicates
  # The stage-1 survival matrix (surface 20-100%) must depress the
  # population relative to the paired no-CE baseline
  expect_lt(final_N(res, "ce"), final_N(res, "baseline"))
})

test_that("default pop run (all mains + all matrices) completes", {
  res <- run_pop()
  expect_equal(length(res$ce), 2)
  expect_true(all(is.finite(vapply(res$ce, function(x) tail(x$pop$N, 1), 1))))
})

test_that("mains-only, both, and neither all run", {
  expect_error(run_pop(stressors = c("A", "B")), NA)
  expect_error(run_pop(stressors = c("A", "B", "MInt_AB")), NA)
  # neither: no dose data path (dose not a data frame -> no CE layers)
  set.seed(11)
  expect_error(
    suppressWarnings(suppressMessages(
      PopulationModel_Run(
        dose = NA, sr_wb_dat = sr_pop, life_cycle_params = life_cycles,
        HUC_ID = 1, n_years = 5, MC_sims = 1
      )
    )),
    NA
  )
})

test_that("Main_Effect is ignored: mains contribute alongside an Included matrix", {
  # MInt_AB is Main_Effect = Included. In the Joe Model that absorbs A
  # and B - in the population model it must NOT. Same seed, same draw
  # sequence: adding mains A+B to the matrix selection must change
  # (lower) the outcome vs the matrix alone. Run at HUC 2, where B's
  # univariate curve gives sys.cap = 0.1 on adult survival (at HUC 1
  # it is 1.0 - no effect - and A is capacity-linked, inert in this
  # compensation-ratio profile).
  n_matrix_only <- final_N(run_pop(stressors = "MInt_AB", huc = 2), "ce")
  n_both <- final_N(run_pop(stressors = c("A", "B", "MInt_AB"), huc = 2), "ce")
  expect_lt(n_both, n_matrix_only)
})

test_that("matrix life-stage tag is honored (bogus tag = no effect)", {
  # Identical draw sequences; only the CE target tag differs
  sr_bogus <- sr_pop
  sr_bogus$MInt$MInt_AB$Life_stages <- "zzz_not_a_stage"
  n_real <- final_N(run_pop(stressors = "MInt_AB"), "ce")
  n_bogus <- final_N(run_pop(sr = sr_bogus, stressors = "MInt_AB"), "ce")
  # The bogus-tagged matrix cannot depress the population; the real
  # stage-1 survival tag must
  expect_lt(n_real, n_bogus)
})

test_that("unknown matrix names in `stressors` warn", {
  set.seed(11)
  expect_warning(
    suppressMessages(
      PopulationModel_Run(
        dose = dose_fix, sr_wb_dat = sr_pop,
        life_cycle_params = life_cycles,
        HUC_ID = 1, n_years = 5, MC_sims = 1,
        stressors = c("A", "MInt_ZZZ")
      )
    ),
    "Unknown interaction matrix"
  )
})
