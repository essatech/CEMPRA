# Stressor_cat semantics:
# - optional column in both workbooks (synthesized from the stressor name)
# - duplicate stressor names with distinct categories auto-rename to
#   "<Stressor>_<Stressor_cat>" (with a warning) in BOTH loaders
# - reserved value "Total_Mortality" aggregates magnitude rows; SD pooled in
#   quadrature with na.rm; near-miss spellings warn

skip_if_not_installed("writexl")

mk_curve <- function() {
  data.frame(value = c(0, 10),
             `Mean System Capacity (%)` = c(100, 50),
             SD = c(0, 0), low.limit = c(0, 0), up.limit = c(100, 100),
             check.names = FALSE)
}

mk_main <- function(stressors, cats = NULL) {
  df <- data.frame(Stressors = stressors,
                   Interaction = NA, Linked = NA,
                   Stress_Scale = "linear", Function = "continuous",
                   Life_stages = "adult", Parameters = NA,
                   stringsAsFactors = FALSE)
  if (!is.null(cats)) df$Stressor_cat <- cats
  df
}

write_sr <- function(main, sheet_names) {
  sheets <- c(list(Main = main),
              stats::setNames(replicate(length(sheet_names), mk_curve(),
                                        simplify = FALSE), sheet_names))
  tmp <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(sheets, tmp)
  tmp
}

test_that("SR workbook loads without a Stressor_cat column", {
  tmp <- write_sr(mk_main(c("FlowA", "TempB")), c("FlowA", "TempB"))
  sr <- StressorResponseWorkbook(filename = tmp)
  expect_equal(sort(sr$stressor_names), c("FlowA", "TempB"))
  expect_equal(sr$main_sheet$Stressor_cat, sr$main_sheet$Stressors)
  expect_true("Stressor_cat_orig" %in% colnames(sr$main_sheet))
  unlink(tmp)
})

test_that("SR duplicate stressor names resolve via distinct Stressor_cat", {
  main <- mk_main(c("Pools", "Pools", "TempB"),
                  c("Winter Habitat", "Summer Rear", "TempB"))
  tmp <- write_sr(main, c("Pools_Winter Habitat", "Pools_Summer Rear", "TempB"))
  expect_warning(
    sr <- StressorResponseWorkbook(filename = tmp),
    regexp = "resolved using Stressor_cat"
  )
  expect_setequal(sr$stressor_names,
                  c("Pools_Winter Habitat", "Pools_Summer Rear", "TempB"))
  # final names in Stressor_cat; original labels preserved
  expect_equal(sr$main_sheet$Stressor_cat, sr$main_sheet$Stressors)
  expect_true("Winter Habitat" %in% sr$main_sheet$Stressor_cat_orig)
  unlink(tmp)
})

test_that("SR renamed stressors need matching (full-name) worksheets", {
  main <- mk_main(c("Pools", "Pools", "TempB"),
                  c("Winter Habitat", "Summer Rear", "TempB"))
  # sheets still use the short name -> informative error
  tmp <- write_sr(main, c("Pools", "TempB"))
  expect_error(
    suppressWarnings(StressorResponseWorkbook(filename = tmp)),
    regexp = "no matching worksheet"
  )
  unlink(tmp)
})

test_that("SR duplicates with identical categories still error cleanly", {
  main <- mk_main(c("Pools", "Pools"), c("Same Cat", "Same Cat"))
  tmp <- write_sr(main, c("Pools"))
  expect_error(
    StressorResponseWorkbook(filename = tmp),
    regexp = "Duplicate stressor name"
  )
  unlink(tmp)
})


mk_sm <- function(stressor, cat, mean, sd, huc = 1) {
  data.frame(HUC_ID = huc, NAME = "unit", Stressor = stressor,
             Stressor_cat = cat, Mean = mean, SD = sd,
             Distribution = "normal", Low_Limit = 0, Up_Limit = 1,
             Comments = NA, stringsAsFactors = FALSE)
}

write_sm <- function(df) {
  tmp <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list(natural_unc = df), tmp)
  tmp
}

test_that("SM workbook loads without a Stressor_cat column", {
  df <- mk_sm("FlowA", "x", 5, 0.1)
  df$Stressor_cat <- NULL
  tmp <- write_sm(df)
  sm <- StressorMagnitudeWorkbook(filename = tmp, scenario_worksheet = 1)
  expect_equal(sm$Stressor_cat, sm$Stressor)
  expect_true("Stressor_cat_orig" %in% colnames(sm))
  unlink(tmp)
})

test_that("dose names align to SR-disambiguated names at run time only", {

  # SR workbook disambiguated 'Pools' into two full names
  sr_names <- c("Pools_Winter Habitat", "Pools_Summer Rear", "TempB")

  dose <- rbind(mk_sm("Pools", "Winter Habitat", 5, 0.1, huc = 1),
                mk_sm("Pools", "Summer Rear", 8, 0.1, huc = 1),
                mk_sm("TempB", "TempB", 12, 0, huc = 1))

  expect_warning(
    out <- align_dose_stressor_names(dose, sr_names),
    regexp = "Stressor_cat was used to resolve"
  )
  expect_setequal(unique(out$Stressor),
                  c("Pools_Winter Habitat", "Pools_Summer Rear", "TempB"))
})

test_that("legacy component rows (short name matches SR) are untouched", {

  # Sierra-style pattern: multiple component rows per (HUC, stressor) with
  # Stressor_cat as a component label, feeding ONE stressor-response curve
  sr_names <- c("Competing_Spp", "TempB")
  dose <- rbind(mk_sm("Competing_Spp", "BKTR", 0.1, 0, huc = 1),
                mk_sm("Competing_Spp", "NN_RNTR", 0.2, 0, huc = 1),
                mk_sm("TempB", "TempB", 12, 0, huc = 1))

  expect_no_warning(out <- align_dose_stressor_names(dose, sr_names))
  expect_identical(out$Stressor, dose$Stressor)

  # and a dose with no Stressor_cat column passes through unchanged
  dose2 <- dose
  dose2$Stressor_cat <- NULL
  expect_identical(align_dose_stressor_names(dose2, sr_names), dose2)
})

test_that("Total_Mortality pools SD in quadrature with na.rm", {
  df <- rbind(
    mk_sm("Angling",    "Total_Mortality", 0.05, 0.1, huc = 1),
    mk_sm("Dewatering", "Total_Mortality", 0.10, 0.2, huc = 1),
    mk_sm("Predation",  "Total_Mortality", NA,   NA,  huc = 1),
    mk_sm("Angling",    "Total_Mortality", 0.02, NA,  huc = 2),
    mk_sm("Dewatering", "Total_Mortality", 0.03, NA,  huc = 2),
    mk_sm("FlowA",      "FlowA",           5,    0,   huc = 1))
  tmp <- write_sm(df)
  sm <- StressorMagnitudeWorkbook(filename = tmp, scenario_worksheet = 1)
  tm1 <- sm[sm$Stressor == "Total_Mortality" & sm$HUC_ID == 1, ]
  tm2 <- sm[sm$Stressor == "Total_Mortality" & sm$HUC_ID == 2, ]
  expect_equal(tm1$Mean, 0.15)                       # NA mean dropped
  expect_equal(tm1$SD, sqrt(0.1^2 + 0.2^2))          # quadrature, NA dropped
  expect_equal(tm2$Mean, 0.05)
  expect_equal(tm2$SD, 0)                            # all-NA SD -> 0
  # non-TM stressor untouched
  expect_true("FlowA" %in% sm$Stressor)
  unlink(tmp)
})

test_that("near-miss Total_Mortality spellings warn and do not aggregate", {
  df <- rbind(mk_sm("Angling", "total_mortality", 0.05, 0.1),
              mk_sm("FlowA", "FlowA", 5, 0))
  tmp <- write_sm(df)
  expect_warning(
    sm <- StressorMagnitudeWorkbook(filename = tmp, scenario_worksheet = 1),
    regexp = "do not match exactly"
  )
  expect_false("Total_Mortality" %in% sm$Stressor)
  expect_true("Angling" %in% sm$Stressor)
  unlink(tmp)
})
