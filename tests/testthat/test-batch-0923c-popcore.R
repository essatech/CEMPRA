# 2026-09-23 ruling batch 3 (population core):
# - egg/fry crowding terms use the CURRENT year's spawners and rate draws
#   (census-timing fix; previously last year's egg count with this year's
#   egg survival, and anadromous eggs back-calculated post-trim)
# - NA sys.cap rows in CE_df warn instead of silently disabling
# - the anadromous "0 survival in intermediate life stage" check only
#   fires for genuine mid-chain zeros

test_that("CR-mode runs complete with the egg-timing fix (both modes)", {
  # Non-anadromous CR mode
  lc <- read.csv(system.file("extdata", "life_cycles.csv", package = "CEMPRA"))
  s <- pop_model_setup(life_cycles = lc)
  m <- pop_model_matrix_elements(s)
  set.seed(21)
  run <- Projection_DD(
    M.mx = m$life_stages_symbolic, D.mx = m$density_stage_symbolic,
    H.mx = NULL, dat = m$life_histories, K = m$life_histories$Ka,
    Nyears = 60, p.cat = 0, anadromous = FALSE, CE_df = NULL
  )
  finalN <- tail(run$pop$N, 1)
  expect_true(is.finite(finalN) && finalN > 0)

  # Anadromous CR mode
  lca <- read.csv(system.file(
    "extdata/simple_test/test_anadromous/chinook_life_cycle.csv",
    package = "CEMPRA"))
  sa <- suppressWarnings(suppressMessages(pop_model_setup(life_cycles = lca)))
  ma <- suppressMessages(pop_model_matrix_elements(sa))
  set.seed(21)
  runa <- suppressWarnings(Projection_DD(
    M.mx = ma$life_stages_symbolic, D.mx = ma$density_stage_symbolic,
    H.mx = NULL, dat = ma$life_histories, K = ma$life_histories$Ka,
    Nyears = 60, p.cat = 0, anadromous = TRUE, CE_df = NULL
  ))
  finalNa <- tail(runa$pop$N, 1)
  expect_true(is.finite(finalNa) && finalNa > 0)
})

test_that("NA sys.cap CE rows warn instead of silently disabling", {
  lc <- read.csv(system.file("extdata", "life_cycles.csv", package = "CEMPRA"))
  s <- pop_model_setup(life_cycles = lc)
  m <- pop_model_matrix_elements(s)
  ce <- data.frame(
    HUC = 1, Stressor = "TestStress", Stressor_cat = "TestStress",
    simulation = 1, dose = 1, sys.cap = NA,
    life_stage = "stage_1", parameter = "survival",
    stringsAsFactors = FALSE
  )
  set.seed(4)
  expect_warning(
    Projection_DD(
      M.mx = m$life_stages_symbolic, D.mx = m$density_stage_symbolic,
      H.mx = NULL, dat = m$life_histories, K = m$life_histories$Ka,
      Nyears = 5, p.cat = 0, anadromous = FALSE, CE_df = ce
    ),
    "NA sys.cap"
  )
})

test_that("intermediate-zero check: end-of-chain zeros do not misfire", {
  lca <- read.csv(system.file(
    "extdata/simple_test/test_anadromous/chinook_life_cycle.csv",
    package = "CEMPRA"))
  s <- suppressWarnings(suppressMessages(
    pop_model_setup_anadromous(life_cycles = lca)))
  # The shipped fixture has only legitimate trailing zeros - no misfire
  expect_false(identical(
    s$possible_error_state,
    "Cannot have 0 survival in intermediate life stage"
  ))
})

test_that("intermediate-zero check: a genuine mid-chain zero is repaired", {
  lca <- read.csv(system.file(
    "extdata/simple_test/test_anadromous/chinook_life_cycle.csv",
    package = "CEMPRA"))
  lca$Value[lca$Name == "surv_2"] <- 0 # between nonzero surv_1 and surv_3+
  s <- suppressWarnings(suppressMessages(
    pop_model_setup_anadromous(life_cycles = lca)))
  expect_identical(
    s$possible_error_state,
    "Cannot have 0 survival in intermediate life stage"
  )
  # The zero was repaired so the matrix stays valid
  expect_true(s$life_pars["surv_2", "Value"] > 0)
})
