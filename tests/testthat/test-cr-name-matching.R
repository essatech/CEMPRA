# Compensation ratios must be matched by NAME, not file position.
#
# The previous positional grep in pop_model_matrix_elements() assumed cr
# rows appeared in canonical order (cr_E, cr_0, cr_1..cr_N). The data
# cleaner appends missing cr_E/cr_0 rows at the END of the file, which
# silently shifted every compensation ratio two stages (egg-stage DD got
# stage-1's ratio, etc.). Fixed 2026-09-22: name-based lookup with a
# default of 1 (no compensation) for missing/blank rows.

test_that("canonical file: cr vector matches the file values in order", {
  lc <- read.csv(system.file("extdata", "life_cycles.csv", package = "CEMPRA"))
  setup <- pop_model_setup(life_cycles = lc)
  mat_el <- pop_model_matrix_elements(setup)

  expect_equal(mat_el$life_histories$cr,
               c(crE = 1, cr0 = 3, cr1 = 2.5, cr2 = 2, cr3 = 1.1, cr4 = 1))
})


test_that("missing cr_E/cr_0 rows no longer shift the ratios (cleaner path)", {
  lc <- read.csv(system.file("extdata", "life_cycles.csv", package = "CEMPRA"))
  lc2 <- lc[!(lc$Name %in% c("cr_E", "cr_0")), ]

  # The cleaner appends the missing rows at the END of the file
  cleaned <- pop_model_dat_clean(lc2)
  cr_rows <- grep("^cr_", cleaned$Name, value = TRUE)
  expect_equal(cr_rows, c("cr_1", "cr_2", "cr_3", "cr_4", "cr_E", "cr_0"))

  # Name-based lookup must still assign each ratio to its own stage.
  # (The old positional code produced crE=2.5, cr0=2, cr1=1.1, cr2=1,
  # cr3=1, cr4=1 here - every ratio shifted two slots.)
  setup <- pop_model_setup(life_cycles = cleaned)
  mat_el <- pop_model_matrix_elements(setup)
  expect_equal(mat_el$life_histories$cr,
               c(crE = 1, cr0 = 1, cr1 = 2.5, cr2 = 2, cr3 = 1.1, cr4 = 1))
})


test_that("row order in the life cycles file is irrelevant", {
  lc <- read.csv(system.file("extdata", "life_cycles.csv", package = "CEMPRA"))

  setup_a <- pop_model_setup(life_cycles = lc)
  mat_a <- pop_model_matrix_elements(setup_a)

  set.seed(77)
  lc_shuffled <- lc[sample(nrow(lc)), ]
  setup_b <- pop_model_setup(life_cycles = lc_shuffled)
  mat_b <- pop_model_matrix_elements(setup_b)

  expect_equal(mat_b$life_histories$cr, mat_a$life_histories$cr)
  expect_equal(mat_b$projection_matrix, mat_a$projection_matrix)

  # Full seeded projections must be identical
  run_one <- function(m) {
    set.seed(42)
    Projection_DD(M.mx = m$life_stages_symbolic,
                  D.mx = m$density_stage_symbolic,
                  H.mx = NULL, dat = m$life_histories,
                  K = m$life_histories$Ka,
                  Nyears = 50, p.cat = 0, CE_df = NULL)$pop$N
  }
  expect_equal(run_one(mat_b), run_one(mat_a))
})


test_that("blank cr cells default to 1 and unrelated 'cr' substrings are ignored", {
  lc <- read.csv(system.file("extdata", "life_cycles.csv", package = "CEMPRA"))

  # blank cr_2 -> 1, all others untouched
  lc2 <- lc
  lc2$Value[lc2$Name == "cr_2"] <- NA
  setup <- pop_model_setup(life_cycles = lc2)
  mat_el <- pop_model_matrix_elements(setup)
  expect_equal(mat_el$life_histories$cr,
               c(crE = 1, cr0 = 3, cr1 = 2.5, cr2 = 1, cr3 = 1.1, cr4 = 1))

  # a row whose Name merely contains the letters "cr" must not be swept in
  # (the old unanchored grep("cr", ...) would have shifted the whole vector)
  lc3 <- rbind(lc, data.frame(Parameters = "recruitment note",
                              Name = "recr_1", Value = 99))
  setup3 <- pop_model_setup(life_cycles = lc3)
  mat3 <- pop_model_matrix_elements(setup3)
  expect_equal(mat3$life_histories$cr,
               c(crE = 1, cr0 = 3, cr1 = 2.5, cr2 = 2, cr3 = 1.1, cr4 = 1))
})


test_that("no cr rows at all -> all ratios 1 with a console message", {
  lc <- read.csv(system.file("extdata", "life_cycles.csv", package = "CEMPRA"))
  lc2 <- lc[!grepl("^cr_", lc$Name), ]
  setup <- pop_model_setup(life_cycles = lc2)
  expect_message(mat_el <- pop_model_matrix_elements(setup),
                 "No compensation ratio")
  expect_true(all(mat_el$life_histories$cr == 1))
})
