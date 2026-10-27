# Thousands separators in the life cycles Value column (e.g. "2,315" from a
# formatted Excel export) must parse as numbers rather than silently
# becoming NA (which surfaced in Shiny as "Missing parameter for stage 1").

test_that("pop_model_dat_clean strips thousands separators from Value", {
  lc <- data.frame(
    Parameters = c("Nstage", "Anadromous", "k", "events", "eps_3", "eps_4",
                   "int", "SE", "S0", "SR", "surv_1", "surv_2", "surv_3",
                   "surv_4", "mat_3", "mat_4", "eps_sd", "M.cv",
                   "bh_stage_pb_1"),
    Name = c("Nstage", "anadromous", "k", "events", "eps_3", "eps_4", "int",
             "SE", "S0", "SR", "surv_1", "surv_2", "surv_3", "surv_4",
             "mat_3", "mat_4", "eps_sd", "M.cv", "bh_stage_pb_1"),
    Value = c("4", "TRUE", "1,000,000", "1", "2,315", " 2,315 ", "1", "0.2",
              "0.65", "0.5", "0.78", "0.0392", "0.7", "0", "0.9", "1",
              "500", "0.2", "TRUE"),
    stringsAsFactors = FALSE
  )

  cl <- expect_silent(pop_model_dat_clean(dat = lc, nstage_fill = 10))

  expect_equal(cl$Value[cl$Name == "eps_3"], 2315)
  expect_equal(cl$Value[cl$Name == "eps_4"], 2315)
  expect_equal(cl$Value[cl$Name == "k"], 1e6)
  # Untouched values are unaffected
  expect_equal(cl$Value[cl$Name == "surv_2"], 0.0392)
  expect_equal(cl$Value[cl$Name == "bh_stage_pb_1"], 1)

  # The anadromous setup now resolves every matrix cell
  r <- pop_model_setup_anadromous(cl)
  expect_equal(r$possible_error_state, "All Good")
})

test_that("non-numeric comma strings are left for normal NA coercion", {
  lc <- data.frame(
    Parameters = c("Nstage", "k", "eps", "surv_1", "surv_2", "mat_1", "mat_2", "M.cv"),
    Name = c("Nstage", "k", "eps", "surv_1", "surv_2", "mat_1", "mat_2", "M.cv"),
    Value = c("2", "1000", "100", "0.5", "0.5", "0", "1", "a,b"),
    stringsAsFactors = FALSE
  )
  cl <- suppressWarnings(pop_model_dat_clean(dat = lc, nstage_fill = 10))
  expect_true(is.na(cl$Value[cl$Name == "M.cv"]))
})
