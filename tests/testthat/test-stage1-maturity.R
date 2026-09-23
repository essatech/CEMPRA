# Stage 1 must be immature (mat_1 = 0): reproduction begins at stage 2.
# mat_1 > 0 previously produced a silent divergence - the preview matrix from
# pop_model_setup() included stage-1 fecundity at [1,1] while the simulated
# (symbolic) matrix ignored it entirely (life_histories$mat drops mat_1).
# It is now a clean error in both setup paths.

test_that("mat_1 > 0 raises a clean error (non-anadromous)", {

  filename_lc <- system.file("extdata", "life_cycles.csv", package = "CEMPRA")
  lc <- read.csv(filename_lc, stringsAsFactors = FALSE)
  lc$Value[lc$Name == "mat_1"] <- "0.5"

  expect_error(
    pop_model_setup(life_cycles = lc),
    regexp = "mat_1 > 0 is not supported"
  )
})

test_that("mat_1 = 0 and blank mat_1 still load fine", {

  filename_lc <- system.file("extdata", "life_cycles.csv", package = "CEMPRA")
  lc <- read.csv(filename_lc, stringsAsFactors = FALSE)

  # canonical file (mat_1 = 0)
  expect_silent(setup <- pop_model_setup(life_cycles = lc))
  expect_equal(setup$possible_error_state, "All Good")

  # blank/NA mat_1 treated as immature, no error from the new check
  lc2 <- lc
  lc2$Value[lc2$Name == "mat_1"] <- NA
  setup2 <- pop_model_setup(life_cycles = lc2)
  expect_true(is.list(setup2))
})

test_that("age-1 spawning raises a clean error (anadromous)", {

  filename_lc <- system.file(
    "extdata/nanaimo/species_profiles/beechie_2021_chehalis.csv",
    package = "CEMPRA")
  lc <- read.csv(filename_lc, stringsAsFactors = FALSE)

  # sanity: anadromous profile loads fine as shipped (mat_3..mat_6)
  setup_ok <- pop_model_setup(life_cycles = lc)
  expect_true(setup_ok$anadromous)

  # add age-1 maturity -> stage_B_1 would be unreachable
  lc[nrow(lc) + 1, c("Parameters", "Name", "Value")] <-
    c("probability of maturity at age-1", "mat_1", "0.5")

  expect_error(
    pop_model_setup(life_cycles = lc),
    regexp = "not supported for anadromous"
  )
})
