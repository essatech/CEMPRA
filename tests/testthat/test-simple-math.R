test_that("Simple Math Works", {

  # ---------------------------------------------------
  # rtnorm_TruncatedDistributions()
  c1 <- rtnorm_TruncatedDistributions(n = 5, mean = 0, sd = 1)
  c2 <- rtnorm_TruncatedDistributions(n = 5, mean = -5, sd = 1)
  c3 <- rtnorm_TruncatedDistributions(n = 5, mean = 5, sd = 1)
  expect_true(!(any(is.na(c1))))
  expect_true(!(any(is.na(c2))))
  expect_true(!(any(is.na(c3))))
  expect_true(length(c1) == 5)
  expect_true(length(c2) == 5)
  expect_true(length(c3) == 5)
  
})
