# A maturity row for an age no fish can survive to (mat_k > 0 while
# surv_(k-1) is 0/missing) used to crash deep inside the symbolic matrix
# build with "object 's4' not found". Found via the Shiny app, where a
# 4-age upload inherited the default 5-age profile's stale mat_5 = 1.

test_that("mat_k beyond the last surviving pre-breeder age errors clearly", {
  lc <- read.csv(system.file("extdata/ltre/life_cycles_input_ltre.csv", package = "CEMPRA"),
                 stringsAsFactors = FALSE)[, c("Parameters", "Name", "Value")]
  lc <- pop_model_dat_clean(lc, nstage_fill = 10)
  # baseline builds
  expect_silent(suppressMessages(pop_model_setup(life_cycles = lc)))

  # terminal age is 5 here (surv_5 = 0); make mat_6 > 0
  bad <- lc
  bad$Value[bad$Name == "mat_6"] <- 1
  expect_error(suppressMessages(pop_model_setup(life_cycles = bad)),
               "mat_6.*no fish can reach that age.*surv_5")
  # message is about the input, not the old symbol-lookup failure
  expect_error(suppressMessages(pop_model_setup(life_cycles = bad)),
               "Set mat_6 to 0")

  # and pop_model_ltre_range surfaces that reason instead of a bare count
  bad$Test_Parameter <- FALSE; bad$Distribution <- ""; bad$SD <- NA
  bad$Low_Limit <- NA; bad$Up_Limit <- NA
  i <- which(bad$Name %in% c("surv_2", "eps_3"))
  bad$Test_Parameter[i] <- TRUE; bad$Distribution[i] <- "uniform"
  bad$Low_Limit[i] <- c(0.05, 1000); bad$Up_Limit[i] <- c(0.5, 3000)
  err <- tryCatch(suppressMessages(pop_model_ltre_range(bad, n_samples = 12, seed = 1)),
                  error = function(e) conditionMessage(e))
  expect_match(err, "Too few valid samples")
  expect_match(err, "mat_6")
})
