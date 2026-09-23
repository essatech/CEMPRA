# Blank/NA K cells in habitat_dd_k mean "no constraint for that stage" (by
# design). Supplying a row for a HUC switches that location into
# habitat-capacity mode even if every stage is unconstrained - behaviour is
# unchanged, but an all-blank row now triggers a warning since leaving every
# stage unconstrained is usually a data-entry oversight.

test_that("all-blank habitat K row warns but behaviour is unchanged", {

  filename_lc <- system.file("extdata", "life_cycles.csv", package = "CEMPRA")
  lc <- read.csv(filename_lc, stringsAsFactors = FALSE)
  me <- pop_model_matrix_elements(pop_model_setup(lc))
  lh <- me$life_histories

  hab_blank <- data.frame(HUC_ID = 1, k_stage_1_mean = NA)

  expect_warning(
    ret <- build_k_for_proj_dd(
      habitat_dd_k = hab_blank,
      HUC_ID = 1,
      life_histories = lh,
      life_cycle_params = lc
    ),
    regexp = "every K value is blank/NA"
  )

  # Behaviour preserved: the all-NA vector is still returned (K-file mode),
  # NA = unconstrained at every stage
  expect_false(is.null(ret$ret_stage_k_override))
  expect_true(all(is.na(ret$ret_stage_k_override)))
  expect_length(ret$ret_stage_k_override, as.numeric(lh$Nstage) + 1)
  expect_null(ret$ret_bh_dd_stages)
})


test_that("no blank-row warning when at least one K value is supplied", {

  filename_lc <- system.file("extdata", "life_cycles.csv", package = "CEMPRA")
  lc <- read.csv(filename_lc, stringsAsFactors = FALSE)
  me <- pop_model_matrix_elements(pop_model_setup(lc))
  lh <- me$life_histories

  hab_ok <- data.frame(HUC_ID = 1, k_stage_1_mean = 500)

  # Re-anchored 2026-09-22 (issue 1.2): a K value with NO enabled
  # bh_/hs_ flag now raises the orphaned-capacity warning (the K would be
  # silently ignored while the location runs uncalibrated). With the flag
  # enabled, no warning fires at all.
  expect_warning(
    ret <- build_k_for_proj_dd(
      habitat_dd_k = hab_ok,
      HUC_ID = 1,
      life_histories = lh,
      life_cycle_params = lc
    ),
    "IGNORED"
  )
  expect_equal(as.numeric(ret$ret_stage_k_override[["k_stage_1"]]), 500)

  lcf <- rbind(lc, data.frame(Parameters = "bh_stage_1",
                              Name = "bh_stage_1", Value = 1))
  expect_no_warning(
    build_k_for_proj_dd(
      habitat_dd_k = hab_ok,
      HUC_ID = 1,
      life_histories = lh,
      life_cycle_params = lcf
    )
  )
})


test_that("no warning for spawner-only anadromous configuration", {

  # Legitimate configuration: only the total spawner capacity is set, so the
  # per-stage vector is all NA on purpose - must not warn.
  lh <- list(
    anadromous = TRUE,
    Nstage_Pb = 3,
    stage_names = c("stage_Pb_1", "stage_Pb_2", "stage_Pb_3",
                    "stage_B_3", "stage_B_4")
  )
  # (bh_spawners enabled so the spawner capacity is genuinely consumed -
  # since 2026-09-22 a spawner K without a spawner flag warns)
  lcp <- data.frame(Name = c("Nstage", "k", "bh_spawners"),
                    Value = c(3, 100, 1))

  hab_spawners <- data.frame(HUC_ID = 1, k_stage_B_mean = 400)

  expect_no_warning(
    ret <- build_k_for_proj_dd(
      habitat_dd_k = hab_spawners,
      HUC_ID = 1,
      life_histories = lh,
      life_cycle_params = lcp
    )
  )
  expect_equal(ret$ret_total_anadromous_spawners, 400)
  expect_true(all(is.na(ret$ret_stage_k_override)))
})
