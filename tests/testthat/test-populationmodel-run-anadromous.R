test_that("test-populationmodel-run-anadromous", {

  #----------------------------------------------------------
  # Test the 3X3 anadromous matrix with PopulationModel_Run()
  #----------------------------------------------------------

  # two stage with maturation at age-3
  filename <- system.file("extdata/simple_test/test_anadromous/stage_3_anadromous.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename)

  # Load stressor-magnitude (sm) data file
  filename_sm <- system.file("extdata", "./matrix_test/stressor_magnitude_matrix.xlsx", package = "CEMPRA")
  dose <- StressorMagnitudeWorkbook(filename = filename_sm)
  # Load stressor-response (sr) data file
  filename_sr <- system.file("extdata", "./matrix_test/stressor_response_matrix_AB.xlsx", package = "CEMPRA")
  sr_wb_dat <- StressorResponseWorkbook(filename = filename_sr)


  # Load in the habitat data optional
  data <- PopulationModel_Run(
    dose = dose,
    sr_wb_dat = sr_wb_dat,
    life_cycle_params = life_cycles,
    HUC_ID = 2, # HUC ID 2
    n_years = 50,
    MC_sims = 5,
    stressors = NA  # Run with all stressors
  )

  names(data)
  length(data$ce)
  length(data$baseline)
  length(data$MC_sims)


  expect_true(1 == 1)


})

