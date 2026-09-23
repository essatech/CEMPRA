# One stressor = one stressor-response relationship:
# - duplicate stressor names in the Main sheet raise a clean error
# - comma-separated Life_stages lists reach multiple life stages
# - unrecognized life_stage / parameter tokens warn (silent no-ops otherwise)

test_that("duplicate stressor names in the Main sheet raise a clean error", {

  skip_if_not_installed("writexl")

  src <- system.file(
    "extdata/simple_test/nicola_test/20240926_lcm/stressor_response_no_stochast.xlsx",
    package = "CEMPRA")
  main <- readxl::read_excel(src, sheet = "Main")
  curve_names <- setdiff(readxl::excel_sheets(src), "Main")
  curves <- lapply(curve_names, function(s) readxl::read_excel(src, sheet = s))
  names(curves) <- curve_names

  # Repeat the first stressor on a second row (e.g., trying to target a
  # second parameter) - an authoring error that must be caught on load
  dup_row <- main[1, ]
  dup_row$Parameters <- "capacity"
  main_dup <- rbind(main, dup_row)

  tmp_bad <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(c(list(Main = main_dup), curves), tmp_bad)
  expect_error(
    StressorResponseWorkbook(filename = tmp_bad),
    regexp = "Duplicate stressor name"
  )

  # Control: the same workbook without the duplicate row loads fine
  tmp_ok <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(c(list(Main = main), curves), tmp_ok)
  sr <- StressorResponseWorkbook(filename = tmp_ok)
  expect_true(is.list(sr))
  expect_true(all(!duplicated(sr$stressor_names)))

  unlink(c(tmp_bad, tmp_ok))
})


make_test_rates <- function(n_yr = 3) {
  list(
    ft = replicate(n_yr + 1, c(eps = 1000), simplify = FALSE),
    st = replicate(n_yr + 1,
                   c(sE = 0.10, s0 = 0.30, s1 = 0.40, s2 = 0.50,
                     s3 = 0.60, s4 = 0.70), simplify = FALSE),
    ut = NULL, smigt = NULL,
    K_list = replicate(n_yr + 1, c(K1 = 1, K2 = 1, K3 = 1, K4 = 1), simplify = FALSE),
    Ke_list = replicate(n_yr + 1, c(Ke = 1), simplify = FALSE),
    K0_list = replicate(n_yr + 1, c(K0 = 1), simplify = FALSE),
    Kspawners_list = replicate(n_yr + 1, NULL, simplify = FALSE)
  )
}

make_test_ce <- function(life_stage, parameter, n_yr = 3) {
  data.frame(Stressor = "TestStressor", life_stage = life_stage,
             parameter = parameter, sys.cap = 0.5,
             Year = 1:n_yr, simulation = 1, HUC = 1)
}

test_that("comma-separated Life_stages applies effect to every listed stage", {

  rates <- make_test_rates()
  out <- expect_no_warning(
    pop_model_ce_apply_year(rates = rates,
                            CE_df = make_test_ce("egg, S0, stage_1", "survival"),
                            anadromous = FALSE, mat_stage = c(mat4 = 1))
  )
  ratio <- out$st[[2]] / rates$st[[2]]
  expect_equal(unname(ratio[c("sE", "s0", "s1")]), c(0.5, 0.5, 0.5))
  expect_equal(unname(ratio[c("s2", "s3", "s4")]), c(1, 1, 1))
})


test_that("unrecognized life_stage tokens warn and have no effect", {

  rates <- make_test_rates()

  # Unsupported alias (hyphen not stripped)
  expect_warning(
    out <- pop_model_ce_apply_year(rates = rates,
                                   CE_df = make_test_ce("age-1", "survival"),
                                   anadromous = FALSE, mat_stage = c(mat4 = 1)),
    regexp = "unrecognized"
  )
  expect_identical(out$st, rates$st)

  # Typo inside a comma list still warns (valid tokens still applied).
  # Note the warning reports the normalized token (underscores stripped).
  expect_warning(
    out2 <- pop_model_ce_apply_year(rates = rates,
                                    CE_df = make_test_ce("egg, stge_1", "survival"),
                                    anadromous = FALSE, mat_stage = c(mat4 = 1)),
    regexp = "stge1"
  )
  expect_equal(unname(out2$st[[2]][["sE"]] / rates$st[[2]][["sE"]]), 0.5)
  expect_equal(unname(out2$st[[2]][["s1"]]), unname(rates$st[[2]][["s1"]]))

  # Stage number above the supported 1:12 range
  expect_warning(
    pop_model_ce_apply_year(rates = rates,
                            CE_df = make_test_ce("stage_13", "survival"),
                            anadromous = FALSE, mat_stage = c(mat4 = 1)),
    regexp = "stage13"
  )
})


test_that("unrecognized parameter values warn; Survivorship alias accepted", {

  rates <- make_test_rates()

  expect_warning(
    pop_model_ce_apply_year(rates = rates,
                            CE_df = make_test_ce("stage_1", "surival"),
                            anadromous = FALSE, mat_stage = c(mat4 = 1)),
    regexp = "surival"
  )

  # "Survivorship" (any case) maps to survival with no warning
  out <- expect_no_warning(
    pop_model_ce_apply_year(rates = rates,
                            CE_df = make_test_ce("stage_1", "Survivorship"),
                            anadromous = FALSE, mat_stage = c(mat4 = 1))
  )
  expect_equal(unname(out$st[[2]][["s1"]] / rates$st[[2]][["s1"]]), 0.5)
})
