test_that("test-pop-model-coho-anadromous", {

  #------------------------------------------------
  # Run some tests for Anadromous Coho Applications
  #------------------------------------------------
  # Load custom LCM
  filename <- system.file("extdata/simple_test/test_coho/Life_Cycles_Profile.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename, stringsAsFactors = FALSE)
  pop_mod_setup <- pop_model_setup(life_cycles = life_cycles)
  pop_mod_mat <- pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)
  expect_true(pop_mod_mat$life_histories$eps[1] == 2500)
  A <- pop_mod_mat$projection_matrix # See if it looks ok
  lambda <- popbio::lambda(A)
  expect_true(round(lambda, 2) == 1.64) # Should be around this

})

