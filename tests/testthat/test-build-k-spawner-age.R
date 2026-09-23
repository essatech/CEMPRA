# Age-specific spawner K columns (k_stage_B_x_mean) in the habitat capacity
# file must load regardless of whether other K columns are present.
# Regression for the class-check bug in build_k_for_proj_dd: the spawner-age
# block inspected class(stage_k_override) and class(mean_k) (the FRY block's
# variable) instead of class(mean_k_B) - a workbook with a spawner-age column
# but no k_stage_0_mean column crashed with "object 'mean_k' not found".

mk_lh <- function() {
  list(anadromous = TRUE, Nstage_Pb = 4,
       stage_names = c("stage_Pb_1", "stage_Pb_2", "stage_Pb_3",
                       "stage_Pb_4", "stage_B_3", "stage_B_4"))
}

mk_lcp <- function() {
  data.frame(Name = c("Nstage", "k", "bh_stage_b_3"), Value = c(4, 100, 1))
}

test_that("spawner-age K loads without a fry (k_stage_0_mean) column", {

  # base data.frame input (the reproduced crash case)
  hab <- data.frame(HUC_ID = 1, k_stage_B_3_mean = 500)
  ret <- build_k_for_proj_dd(habitat_dd_k = hab, HUC_ID = 1,
                             life_histories = mk_lh(),
                             life_cycle_params = mk_lcp())
  expect_equal(as.numeric(ret$ret_stage_k_override[["stage_B_3"]]), 500)
  expect_true("bh_stage_b_3" %in% ret$ret_bh_dd_stages)

  # tibble input (readxl-style), still without a fry column
  hab_tbl <- tibble::tibble(HUC_ID = 1, k_stage_B_3_mean = 500)
  ret2 <- suppressWarnings(build_k_for_proj_dd(habitat_dd_k = hab_tbl, HUC_ID = 1,
                              life_histories = mk_lh(),
                              life_cycle_params = mk_lcp()))
  expect_equal(as.numeric(ret2$ret_stage_k_override[["stage_B_3"]]), 500)
})

test_that("spawner-age K still loads alongside a fry column (both input types)", {

  hab <- data.frame(HUC_ID = 1, k_stage_0_mean = 200, k_stage_B_3_mean = 500)
  ret <- suppressWarnings(build_k_for_proj_dd(habitat_dd_k = hab, HUC_ID = 1,
                             life_histories = mk_lh(),
                             life_cycle_params = mk_lcp()))
  expect_equal(as.numeric(ret$ret_stage_k_override[["K0"]]), 200)
  expect_equal(as.numeric(ret$ret_stage_k_override[["stage_B_3"]]), 500)

  hab_tbl <- tibble::tibble(HUC_ID = 1, k_stage_0_mean = 200,
                            k_stage_B_3_mean = 500)
  ret2 <- suppressWarnings(build_k_for_proj_dd(habitat_dd_k = hab_tbl, HUC_ID = 1,
                              life_histories = mk_lh(),
                              life_cycle_params = mk_lcp()))
  expect_equal(as.numeric(ret2$ret_stage_k_override[["K0"]]), 200)
  expect_equal(as.numeric(ret2$ret_stage_k_override[["stage_B_3"]]), 500)
})
