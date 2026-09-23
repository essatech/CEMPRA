test_that("dat_clean propagates legacy single eps to anadromous eps_x rows", {

  # Legacy-format anadromous file: one "eps" row, no eps_1..n rows.
  # This is the Shiny app path (pop_model_dat_clean runs on every upload).
  # Previously the cleaner filled eps_1..n with 0 and the setup fallback
  # (which only fires on NA, not 0) never rescued the legacy value ->
  # fecundity silently zeroed and the population collapsed.

  filename <- system.file(
    "extdata/simple_test/test_anadromous/stage_25_anadromous.csv",
    package = "CEMPRA")
  lc <- read.csv(filename, stringsAsFactors = FALSE)

  # The shipped file has a blank Nstage cell (rejected by dat_clean with a
  # clear error); fill it to represent a user file with a valid Nstage.
  lc$Value[lc$Name == "Nstage"] <- 4
  legacy_eps_val <- as.numeric(lc$Value[lc$Name == "eps"])

  lc_clean <- pop_model_dat_clean(lc, nstage_fill = 10)

  # All generated eps_x rows carry the legacy value, not zero
  eps_rows <- lc_clean[grepl("^eps_[0-9]+$", lc_clean$Name), ]
  expect_true(nrow(eps_rows) > 0)
  expect_true(all(as.numeric(eps_rows$Value) == legacy_eps_val))

  # End-to-end: cleaned file must yield the same fecundity as the direct
  # (non-cleaned) path and a growing population, not lambda = 0
  setup_clean <- pop_model_setup(life_cycles = lc_clean)
  mat_clean <- suppressMessages(
    pop_model_matrix_elements(pop_mod_setup = setup_clean))
  expect_true(all(mat_clean$life_histories$eps == legacy_eps_val))
  expect_gt(popbio::lambda(mat_clean$projection_matrix), 1)

  # Explicit per-stage eps rows are preserved, only missing ones filled
  new_row <- lc[1, ]
  new_row$Parameters <- "eps_3"
  new_row$Name <- "eps_3"
  new_row$Value <- "1234"
  lc_mixed <- rbind(lc, new_row)
  lc_mixed_clean <- pop_model_dat_clean(lc_mixed, nstage_fill = 10)
  expect_equal(
    as.numeric(lc_mixed_clean$Value[lc_mixed_clean$Name == "eps_3"]), 1234)
  expect_equal(
    as.numeric(lc_mixed_clean$Value[lc_mixed_clean$Name == "eps_2"]),
    legacy_eps_val)

  # Without a legacy eps row, missing eps_x still default to 0
  lc_none <- lc[lc$Name != "eps", ]
  lc_none_clean <- pop_model_dat_clean(lc_none, nstage_fill = 10)
  eps_rows0 <- lc_none_clean[grepl("^eps_[0-9]+$", lc_none_clean$Name), ]
  expect_true(all(as.numeric(eps_rows0$Value) == 0))
})
