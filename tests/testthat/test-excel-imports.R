test_that("Excel imports work", {

  # Next ----------------------------------------
  # Import of stressor response and magnitude workbook
  filename_rm <- system.file("extdata", "stressor_magnitude_unc_ARTR.xlsx", package = "CEMPRA")
  filename_sr <- system.file("extdata", "stressor_response_fixed_ARTR.xlsx", package = "CEMPRA")

  smw <- StressorMagnitudeWorkbook(filename = filename_rm, scenario_worksheet = "natural_unc")


  # Check basics
  expect_true(ncol(smw) == 11)
  expect_true(nrow(smw) > 500)
  expect_true(setequal(colnames(smw), c("HUC_ID", "NAME", "Stressor", "Stressor_cat", "Mean", "SD", "Distribution", "Low_Limit", "Up_Limit", "Comments", "Stressor_cat_orig")))


  # Test ----------------------------------------
  # Test import of stressor response workbook
  srw <- StressorResponseWorkbook(filename = filename_sr)

  # Check basics
    # Further ---------------------------------------
  expect_true(class(srw) == "list")

  # That all names are equal
  s1 <- srw$main_sheet$Stressors
  s2 <- srw$stressor_names
  s3 <- names(srw$sr_dat)
  expect_true(setequal(s1, s2))
  expect_true(setequal(s1, s3))
  expect_true(setequal(s2, s3))


  # Test ---------------------------------------
  # Test that external workbook with interaction
  # term loads correctly.



})


# ---------------------------------------------------------------------
# 2026-09-23 Part-4 ruling batch coverage

test_that("invalid mean capacities return NA instead of crashing (4.1)", {
  # tbeta_rnd guard: NA / out-of-range means with SD > 0
  out <- CEMPRA:::tbeta_rnd(mn_est = c(1.1, NA, -0.2, 0.5),
                            sd_est = c(0.1, 0.1, 0.1, 0),
                            low.limit = 0, up.limit = 1)
  expect_true(all(is.na(out[1:3])))
  expect_equal(out[4], 0.5)
})

test_that("Minimum/Maximum with blank Linked warns naming the stressor (4.3)", {
  fr <- system.file("extdata/matrix_test/stressor_magnitude_matrix.xlsx", package = "CEMPRA")
  fs <- system.file("extdata/matrix_test/stressor_response_matrix.xlsx", package = "CEMPRA")
  dose <- StressorMagnitudeWorkbook(filename = fr)
  sr <- suppressWarnings(StressorResponseWorkbook(filename = fs))
  sr$MInt <- NULL
  sr$main_sheet$Interaction[sr$main_sheet$Stressors == "C"] <- "Minimum"
  sr$main_sheet$Linked[sr$main_sheet$Stressors == "C"] <- NA
  set.seed(1)
  expect_warning(
    suppressMessages(JoeModel_Run(dose = dose, sr_wb_dat = sr, MC_sims = 2)),
    "contribute NOTHING"
  )
})

test_that("curve sheets with wrong column order or Main not first error (4.5)", {
  testthat::skip_if_not_installed("writexl")
  fs <- system.file("extdata/matrix_test/stressor_response_matrix.xlsx", package = "CEMPRA")
  main <- as.data.frame(readxl::read_excel(fs, sheet = "Main"))
  main <- main[main$Stressors == "A", , drop = FALSE]

  good_curve <- data.frame(A = c(0, 10, 20),
                           `Mean System Capacity (%)` = c(100, 50, 10),
                           SD = c(0, 0, 0), low.limit = c(0, 0, 0),
                           up.limit = c(100, 100, 100), check.names = FALSE)

  # Wrong column order (SD and low.limit swapped)
  bad_curve <- good_curve[, c(1, 2, 4, 3, 5)]
  tmp1 <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list(Main = main, A = bad_curve), path = tmp1)
  expect_error(suppressWarnings(StressorResponseWorkbook(filename = tmp1)),
               "unexpected column")
  unlink(tmp1)

  # Main sheet not first
  tmp2 <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list(A = good_curve, Main = main), path = tmp2)
  expect_error(suppressWarnings(StressorResponseWorkbook(filename = tmp2)),
               "FIRST worksheet")
  unlink(tmp2)

  # Stray text cell becomes NA instead of poisoning the column
  txt_curve <- good_curve
  txt_curve$SD <- as.character(txt_curve$SD)
  txt_curve$SD[2] <- "TBD"
  tmp3 <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list(Main = main, A = txt_curve), path = tmp3)
  srx <- suppressWarnings(StressorResponseWorkbook(filename = tmp3))
  expect_true(is.numeric(srx$sr_dat$A$sd))
  expect_true(is.na(srx$sr_dat$A$sd[2]))
  unlink(tmp3)
})

test_that("blank limit columns default to 0/100; blank means error (4.5b)", {
  fs <- system.file("extdata/matrix_test/stressor_response_matrix.xlsx", package = "CEMPRA")
  sr <- suppressWarnings(StressorResponseWorkbook(filename = fs))
  sub <- sr$main_sheet[sr$main_sheet$Stressors == "A", , drop = FALSE]
  dat <- sr$sr_dat[["A"]]
  dat$lwr <- NA
  dat$upr <- NA
  expect_message(
    mrl <- mean_Response(n.stressors = 1, str.list = list(A = dat), main = sub),
    "assuming 0"
  )
  expect_equal(mrl[[1]][[3]](dat$value[1]), 0)   # lower limit fn -> 0
  expect_equal(mrl[[1]][[4]](dat$value[1]), 1)   # upper limit fn -> 100% = 1

  dat2 <- sr$sr_dat[["A"]]
  dat2$mean_system_capacity <- NA
  expect_error(
    mean_Response(n.stressors = 1, str.list = list(A = dat2), main = sub),
    "at least two rows"
  )
})

test_that("magnitude workbooks without a Comments column work (4.5c)", {
  testthat::skip_if_not_installed("writexl")
  sm <- data.frame(
    HUC_ID = c(1, 1, 1), NAME = "Loc",
    Stressor = c("m1", "m2", "A"),
    Stressor_cat = c("Total_Mortality", "Total_Mortality", "A"),
    Mean = c(0.1, 0.2, 5), SD = c(0, 0, 0),
    Distribution = "normal", Low_Limit = c(0, 0, 0), Up_Limit = c(1, 1, 100)
  )
  tmp <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list(sheet1 = sm), path = tmp)
  out <- suppressWarnings(StressorMagnitudeWorkbook(filename = tmp,
                                                    scenario_worksheet = 1))
  expect_true("Comments" %in% colnames(out))
  expect_true("Total_Mortality" %in% out$Stressor)
  unlink(tmp)
})
