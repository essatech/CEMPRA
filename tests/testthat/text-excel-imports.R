test_that("Excel imports work", {

  # Next ----------------------------------------
  # Import of stressor response and magnitude workbook
  filename_rm <- system.file("extdata", "stressor_magnitude_unc_ARTR.xlsx", package = "CEMPRA")
  filename_sr <- system.file("extdata", "stressor_response_fixed_ARTR.xlsx", package = "CEMPRA")

  smw <- StressorMagnitudeWorkbook(filename = filename_rm, scenario_worksheet = "natural_unc")


  # Check basics
  expect_true(ncol(smw) == 10)
  expect_true(nrow(smw) > 500)
  expect_true(setequal(colnames(smw), c("HUC_ID", "NAME", "Stressor", "Stressor_cat", "Mean", "SD", "Distribution", "Low_Limit", "Up_Limit", "Comments")))


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
  if(FALSE) {
    filename_sr <- "C:/Users/mbayly/Desktop/Projects/EN2691 CEPopMod/2022 Extension/stressor_response_Interaction.xlsx"
    srw <- StressorResponseWorkbook(filename = filename_sr)


  } else {
    print("TODO... FIX THIS...")
    print("TODO... FIX THIS...")
    print("TODO... FIX THIS...")
    print("TODO... FIX THIS...")
    print("TODO... FIX THIS...")
  }



})
