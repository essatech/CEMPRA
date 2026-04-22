test_that("test-pop-model-anadromous-basic", {

  #----------------------------------------------
  # Test Strange (but possible input formats)
  #----------------------------------------------

  # eps (legacy) vs eps_3, eps_4, eps_5 etc. for non-anadromous fish
  filename <- system.file("extdata/simple_test/test_anadromous/test_non_anadromous_eps_fill.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename, stringsAsFactors = FALSE)
  pop_mod_setup <- pop_model_setup(life_cycles = life_cycles)
  pop_mod_mat <- pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)
  expect_true(pop_mod_mat$life_histories$eps == 3000) # This must be equal to 3000
  A <- pop_mod_mat$projection_matrix
  lambda <- popbio::lambda(A)
  expect_true(round(lambda, 2) == 1.21) # Manual check - might be wrong

  filename <- system.file("extdata/simple_test/test_anadromous/test_setup_error.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename, stringsAsFactors = FALSE)
  pop_mod_setup <- pop_model_setup(life_cycles = life_cycles)
  pop_mod_mat <- pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)

  filename <- system.file("extdata/simple_test/test_anadromous/test_setup_error_2.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename, stringsAsFactors = FALSE)
  pop_mod_setup <- pop_model_setup(life_cycles = life_cycles)
  pop_mod_mat <- pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)

  filename <- system.file("extdata/simple_test/test_anadromous/test_setup_error_3.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename, stringsAsFactors = FALSE)
  pop_mod_setup <- pop_model_setup(life_cycles = life_cycles)
  pop_mod_mat <- pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)

  filename <- system.file("extdata/simple_test/test_anadromous/test_setup_error_4.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename, stringsAsFactors = FALSE)
  pop_mod_setup <- pop_model_setup(life_cycles = life_cycles)
  pop_mod_mat <- pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)

  filename <- system.file("extdata/simple_test/test_anadromous/test_setup_error_5.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename, stringsAsFactors = FALSE)
  pop_mod_setup <- pop_model_setup(life_cycles = life_cycles)
  pop_mod_mat <- pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)

  filename <- system.file("extdata/simple_test/test_anadromous/test_setup_error_6.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename, stringsAsFactors = FALSE)
  pop_mod_setup <- pop_model_setup(life_cycles = life_cycles)
  pop_mod_mat <- pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)

  filename <- system.file("extdata/simple_test/test_anadromous/test_setup_error_7.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename, stringsAsFactors = FALSE)
  pop_mod_setup <- pop_model_setup(life_cycles = life_cycles)
  pop_mod_mat <- pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)


  #----------------------------------------------
  # Start Anadromous Tests
  #----------------------------------------------

  # Load dummy CE_df
  filename <- system.file("extdata/simple_test/CE_df_testing.csv", package = "CEMPRA")
  CE_df <- read.csv(filename, stringsAsFactors = FALSE)


  #----------------------------------------------
  # Test the 2X2 Anadromous Matrix
  #----------------------------------------------
  # two stage with maturation at age-3
  filename <- system.file("extdata/simple_test/test_anadromous/stage_2_anadromous.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename)
  pop_mod_setup <- pop_model_setup(life_cycles = life_cycles)
  # Test each cell of 2X2
  expect_true(pop_mod_setup$projection_matrix_txt[1,1] == "0")
  expect_true(pop_mod_setup$projection_matrix_txt[2,1] == "0.3*0.38*1")
  expect_true(pop_mod_setup$projection_matrix_txt[1,2] == "(0.95 * 1 * 4000 * 0.1 * 0.8 * 0.5)/1")
  expect_true(pop_mod_setup$projection_matrix_txt[2,2] == "0")

  # Test the projection matrix
  pop_mod_mat <- pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)
  A <- pop_mod_mat$projection_matrix
  lambda <- popbio::lambda(A)
  expect_true(round(lambda, 2) == 4.16) # Manual check
  lambda <- popbio::lambda(pop_mod_setup$projection_matrix)
  expect_true(round(lambda, 2) == 4.16) # Manual check

  # Setup for Projection_DD()
  dat <- pop_mod_mat$life_histories
  M.mx <- pop_mod_mat$life_stages_symbolic
  D.mx <- pop_mod_mat$density_stage_symbolic

  # Basic Run
  baseline <-
    Projection_DD(
      M.mx = M.mx,
      D.mx = D.mx,
      dat = dat,
      K = dat$Ka,
      Nyears = 500,
      anadromous = pop_mod_setup$anadromous
    )
  dat$stage_names
  # Should act like even and odd year pink
  # plot(baseline$N[20:100,2], type = 'l')
  # summary(baseline$N[,2])
  diff <- abs(1 - median(baseline$N[,2]) / dat$Ka)
  expect_true(diff < 0.2) # Should roughly match Ka

  # Check cr lambdas
  M.list <-
    Projection_DD(
      M.mx = M.mx,
      D.mx = D.mx,
      dat = dat,
      K = dat$Ka,
      Nyears = 500,
      anadromous = pop_mod_setup$anadromous,
      alt_return = "qa_matrices"
    )
  lambda_dist <- sapply(M.list, function(x) {
    popbio::lambda(x)
  })

  diff <- abs(1 - median(lambda_dist))
  # With CR applied lambda should roughly equal 1.0
  expect_true(diff < 0.2)

  # Run with CE_df
  CE_run <-
    Projection_DD(
      M.mx = M.mx,
      D.mx = D.mx,
      dat = dat,
      CE_df = CE_df,
      K = dat$Ka,
      Nyears = 500,
      anadromous = pop_mod_setup$anadromous
    )
  # Compare CE to baseline
  m1 <- median(baseline$pop$N)
  m2 <- median(CE_run$pop$N)
  expect_true(m1 > m2) # that pop is higher in baseline than with CE




  #----------------------------------------------
  # Test the 3X3 matrix
  #----------------------------------------------

  # two stage with maturation at age-3
  filename <- system.file("extdata/simple_test/test_anadromous/stage_3_anadromous.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename)
  pop_mod_setup <- pop_model_setup(life_cycles = life_cycles)
  # Ensure the s names are correct
  t1 <- pop_mod_setup$life_histories$stage_names
  expect_true(length(setdiff(t1, c("stage_Pb_1", "stage_Pb_2", "stage_B_3"))) == 0)
  # Check some of the matrix elements
  t2 <- pop_mod_setup$projection_matrix
  expect_true(all(diag(t2) == 0))
  expect_true(t2[1,1] == 0)
  expect_true(t2[1,2] == 0)
  expect_true(t2[3,2] > 0)
  expect_true(t2[3,3] == 0)
  expect_true(all(t2[3,2:3] < 1))
  expect_true(pop_mod_setup$projection_matrix_txt[1,3] == "(0.95 * 1 * 4000 * 0.1 * 0.8 * 0.5)/1")
  expect_true(pop_mod_setup$projection_matrix_txt[2,1] == "0.3")

  # Test the projection matrix
  pop_mod_mat <- pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)
  A <- pop_mod_mat$projection_matrix
  lambda <- popbio::lambda(A)
  expect_true(round(lambda, 2) == 1.22) # Manual check
  lambda <- popbio::lambda(pop_mod_setup$projection_matrix)
  expect_true(round(lambda, 2) == 1.22) # Manual check

  # Setup for Projection_DD()
  dat <- pop_mod_mat$life_histories
  M.mx <- pop_mod_mat$life_stages_symbolic
  D.mx <- pop_mod_mat$density_stage_symbolic

  # Basic Run
  baseline <-
    Projection_DD(
      M.mx = M.mx,
      D.mx = D.mx,
      dat = dat,
      K = dat$Ka,
      Nyears = 500,
      anadromous = pop_mod_setup$anadromous
    )
  dat$stage_names
  # Should act like even and odd year pink
  # plot(baseline$N[20:100,2], type = 'l')
  # summary(baseline$N[,2])
  diff <- abs(1 - median(baseline$pop$N) / dat$Ka)
  expect_true(diff < 0.2) # Should roughly match Ka

  # Check cr lambdas
  M.list <-
    Projection_DD(
      M.mx = M.mx,
      D.mx = D.mx,
      dat = dat,
      K = dat$Ka,
      Nyears = 500,
      anadromous = pop_mod_setup$anadromous,
      alt_return = "qa_matrices"
    )
  lambda_dist <- sapply(M.list, function(x) {
    popbio::lambda(x)
  })
  diff <- abs(1 - median(lambda_dist))
  # With CR applied lambda should roughly equal 1.0
  expect_true(diff < 0.2)

  # Run with CE_df
  CE_run <-
    Projection_DD(
      M.mx = M.mx,
      D.mx = D.mx,
      dat = dat,
      CE_df = CE_df,
      K = dat$Ka,
      Nyears = 500,
      anadromous = pop_mod_setup$anadromous
    )
  # Compare CE to baseline
  m1 <- median(baseline$pop$N)
  m2 <- median(CE_run$pop$N)
  expect_true(m1 > m2) # that pop is higher in baseline than with CE





  # ================================================
  # Anadromous 4 stage with maturation at age-5 only
  # ================================================

  filename <- system.file("extdata/simple_test/test_anadromous/stage_5_anadromous.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename)
  pop_mod_setup <- pop_model_setup(life_cycles = life_cycles)
  names(pop_mod_setup)
  expect_true(pop_mod_setup$projection_matrix_txt[1,5] == "(0.95 * 1 * 4000 * 0.1 * 0.8 * 0.5)/1")
  t2 <- pop_mod_setup$projection_matrix
  expect_true(all(t2[1,1:4] == 0))
  expect_true(all(t2[1,5] > 1))
  expect_true(all(diag(t2) == 0))

  # Test the projection matrix
  pop_mod_mat <- pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)
  A <- pop_mod_mat$projection_matrix
  lambda <- popbio::lambda(A)
  expect_true(round(lambda, 2) == 0.86) # Manual check
  lambda <- popbio::lambda(pop_mod_setup$projection_matrix)
  expect_true(round(lambda, 2) == 0.86) # Manual check

  # Setup for Projection_DD()
  dat <- pop_mod_mat$life_histories
  M.mx <- pop_mod_mat$life_stages_symbolic
  D.mx <- pop_mod_mat$density_stage_symbolic

  # Basic Run
  baseline <-
    Projection_DD(
      M.mx = M.mx,
      D.mx = D.mx,
      dat = dat,
      K = dat$Ka,
      Nyears = 500,
      anadromous = pop_mod_setup$anadromous
    )
  dat$stage_names
  # Should act like even and odd year pink
  # plot(baseline$pop$N, type = 'l')
  # summary(baseline$N)
  # summary(baseline$N[, 5])
  diff <- median(baseline$N[,5]) / dat$Ka
  # Lambda is less than 1.0 so shouldn't reach k as quickly
  expect_true(diff < 0.8) # delete this

  # Check cr lambdas
  M.list <-
    Projection_DD(
      M.mx = M.mx,
      D.mx = D.mx,
      dat = dat,
      K = dat$Ka,
      Nyears = 500,
      anadromous = pop_mod_setup$anadromous,
      alt_return = "qa_matrices"
    )
  lambda_dist <- sapply(M.list, function(x) {
    popbio::lambda(x)
  })
  diff <- abs(1 - median(lambda_dist))
  # With CR applied lambda should roughly equal 1.0
  expect_true(diff < 0.2)


  # ==================================================
  # Anadromous max age 5 with maturation age 2 to 5
  # =================================================
  filename <- system.file("extdata/simple_test/test_anadromous/stage_25_anadromous.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename)
  pop_mod_setup <- pop_model_setup(life_cycles = life_cycles)
  t2 <- pop_mod_setup$projection_matrix
  expect_true(all(t2[1,1:4] == 0))
  expect_true(all(t2[1,5:8] > 1))
  expect_true(all(diag(t2) == 0))
  expect_true(all(t2[, 1:4] <= 1))

  # Test the projection matrix
  pop_mod_mat <- pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)
  A <- pop_mod_mat$projection_matrix
  lambda <- popbio::lambda(A)
  expect_true(round(lambda, 2) == 3.79) # Manual check
  lambda <- popbio::lambda(pop_mod_setup$projection_matrix)
  expect_true(round(lambda, 2) == 3.79) # Manual check


  # Setup for Projection_DD()
  dat <- pop_mod_mat$life_histories
  M.mx <- pop_mod_mat$life_stages_symbolic
  D.mx <- pop_mod_mat$density_stage_symbolic

  # Basic Run
  baseline <-
    Projection_DD(
      M.mx = M.mx,
      D.mx = D.mx,
      dat = dat,
      K = dat$Ka,
      Nyears = 500,
      anadromous = pop_mod_setup$anadromous
    )
  dat$stage_names
  # Should act like even and odd year pink
  # plot(baseline$N[20:100,2], type = 'l')
  # summary(baseline$N[,2])
  diff <- abs(1 - median(baseline$pop$N) / dat$Ka)
  expect_true(diff < 0.2) # Should roughly match Ka

  # Check cr lambdas
  M.list <-
    Projection_DD(
      M.mx = M.mx,
      D.mx = D.mx,
      dat = dat,
      K = dat$Ka,
      Nyears = 500,
      anadromous = pop_mod_setup$anadromous,
      alt_return = "qa_matrices"
    )
  lambda_dist <- sapply(M.list, function(x) {
    popbio::lambda(x)
  })
  diff <- abs(1 - median(lambda_dist))
  # With CR applied lambda should roughly equal 1.0
  expect_true(diff < 0.2)

  # Run with CE_df
  CE_run <-
    Projection_DD(
      M.mx = M.mx,
      D.mx = D.mx,
      dat = dat,
      CE_df = CE_df,
      K = dat$Ka,
      Nyears = 500,
      anadromous = pop_mod_setup$anadromous
    )
  # Compare CE to baseline
  m1 <- median(baseline$pop$N)
  m2 <- median(CE_run$pop$N)
  expect_true(m1 > m2) # that pop is higher in baseline than with CE






  # ==============================================
  # Anadromous test with Custom Chinook Matrix
  # ==============================================

  filename <- system.file("extdata/simple_test/test_anadromous/chinook_life_cycle.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename)
  pop_mod_setup <- pop_model_setup(life_cycles = life_cycles)
  # pop_mod_setup$projection_matrix_symbol
  t2 <- pop_mod_setup$projection_matrix
  expect_true(all(t2[1,1:4] == 0))
  expect_true(all(t2[1,5:7] > 1))
  expect_true(all(diag(t2) == 0))
  expect_true(all(t2[, 1:4] <= 1))

  # Test the projection matrix
  pop_mod_mat <- pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)
  A <- pop_mod_mat$projection_matrix
  lambda <- popbio::lambda(A)
  expect_true(round(lambda, 2) == 1.12) # Manual check
  lambda <- popbio::lambda(pop_mod_setup$projection_matrix)
  expect_true(round(lambda, 2) == 1.12) # Manual check


  # Setup for Projection_DD()
  dat <- pop_mod_mat$life_histories
  M.mx <- pop_mod_mat$life_stages_symbolic
  D.mx <- pop_mod_mat$density_stage_symbolic

  # Basic Run
  baseline <-
    Projection_DD(
      M.mx = M.mx,
      D.mx = D.mx,
      dat = dat,
      K = dat$Ka,
      Nyears = 500,
      anadromous = pop_mod_setup$anadromous
    )
  dat$stage_names
  # Should act like even and odd year pink
  # plot(baseline$N[20:100,2], type = 'l')
  # summary(baseline$N[,2])
  diff <- abs(1 - median(baseline$pop$N) / dat$Ka)
  expect_true(diff < 0.2) # Should roughly match Ka

  # Check cr lambdas
  M.list <-
    Projection_DD(
      M.mx = M.mx,
      D.mx = D.mx,
      dat = dat,
      K = dat$Ka,
      Nyears = 500,
      anadromous = pop_mod_setup$anadromous,
      alt_return = "qa_matrices"
    )
  lambda_dist <- sapply(M.list, function(x) {
    popbio::lambda(x)
  })
  diff <- abs(1 - median(lambda_dist))
  # With CR applied lambda should roughly equal 1.0
  expect_true(diff < 0.2)


  # -----------------------------
  # stage-specific K value tests
  # -----------------------------
  filename <- system.file("extdata/simple_test/test_anadromous/chinook_life_cycle.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename)
  pop_mod_setup <- pop_model_setup(life_cycles = life_cycles)
  pop_mod_mat <- pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)
  dat <- pop_mod_mat$life_histories
  M.mx <- pop_mod_mat$life_stages_symbolic
  D.mx <- pop_mod_mat$density_stage_symbolic

  CE_df_all <- data.frame(HUC = 1, Stressor = 1, dose = 1, sys.cap = 0.5, life_stage = "spawners", parameter = "capacity", Stressor_cat = 1)
  CE_df_all <- CE_df_all[rep(1, 9), ]
  CE_df_all$sys.cap <- c(0.1, 0.5, 1, 0.1, 0.5, 1, 0.1, 0.5, 1)
  bh_dd_stages_all <- c("bh_stage_12", "bh_stage_12", "bh_stage_12", "hs_spawners", "hs_spawners", "hs_spawners", "bh_spawners", "bh_spawners", "bh_spawners")
  expected_values <- c(13200, 13200, 13200, 1000, 5000, 10000, 770, 3121, 5422)
  for(j in 1:nrow(CE_df_all)) {
    mcheck <-
      Projection_DD(
        M.mx = M.mx,
        D.mx = D.mx,
        dat = dat,
        CE_df = CE_df_all[j, ],
        K = 10000,
        Nyears = 100,
        anadromous = TRUE,
        bh_dd_stages = bh_dd_stages_all[j],
        stage_k_override = rep(NA, dat$Nstage + 1)
      )
    # plot(mcheck$pop$N[50:100], type = 'l')
    print(paste0("j. ", j, ": ", round(median(mcheck$pop$N[50:100]), 0), "; ", bh_dd_stages_all[j]))

    # Check values are approx within expected range
    diff <- abs(1 - expected_values[j] / median(mcheck$pop$N[50:100]))

    # With CR applied lambda should roughly equal 1.0
    expect_true(diff < 0.2)
  }


  # ------------------------------------------------
  # Population should grow to infinity... or 10^100
  # ------------------------------------------------
  filename <- system.file("extdata/simple_test/test_anadromous/stage_4_anadromous.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename)
  pop_mod_setup <- pop_model_setup(life_cycles = life_cycles)
  pop_mod_mat <- pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)
  dat <- pop_mod_mat$life_histories
  M.mx <- pop_mod_mat$life_stages_symbolic
  D.mx <- pop_mod_mat$density_stage_symbolic

  mcheck <-
    Projection_DD(
      M.mx = M.mx,
      D.mx = D.mx,
      dat = dat,
      CE_df = NULL,
      K = 100,
      Nyears = 100,
      anadromous = TRUE,
      bh_dd_stages = c("hs_stage_13"),
      stage_k_override = rep(NA, dat$Nstage + 1)
    )
  # plot(mcheck$pop$N, type = 'l')
  # Population should grow exponentially
  expect_true(10000000 < median(mcheck$pop$N))


  # Intentional missmatch between bh_dd_stages and stage_k_override
  # population will grow to inf
  # dat$stage_names
  stage_k_override <- rep(NA, dat$Nstage + 1)
  stage_k_override[1] <- 10000
  mcheck <-
    Projection_DD(
      M.mx = M.mx,
      D.mx = D.mx,
      dat = dat,
      CE_df = NULL,
      K = 100,
      Nyears = 100,
      anadromous = TRUE,
      bh_dd_stages = c("hs_stage_pb_13"), # intentional miss-match
      stage_k_override = stage_k_override # intentional miss-match
    )
  # plot(mcheck$pop$N, type = 'l')
  # Population should STILL grow exponentially
  expect_true(10000000 < median(mcheck$pop$N))



  # MATCH --- stage 2 hockey stick
  # population will grow to 10000 stage_Pb_2
  stage_k_override <- rep(NA, dat$Nstage + 1)
  stage_k_override[2 + 1] <- 10000
  names(stage_k_override) <- c("K0", dat$stage_names)
  mcheck <-
    Projection_DD(
      M.mx = M.mx,
      D.mx = D.mx,
      dat = dat,
      CE_df = NULL,
      K = 100,
      Nyears = 100,
      anadromous = TRUE,
      bh_dd_stages = c("hs_stage_pb_2"), # MATCH
      stage_k_override = stage_k_override # MATCH
    )
  # plot(mcheck$pop$N, type = 'l')
  # Population should STILL grow exponentially
  expect_true(10000000 > median(mcheck$pop$N) && 1000 < median(mcheck$pop$N))
  diff <- abs(1 - (10000 / median(mcheck$N[, "K2"])))
  expect_true(diff < 0.1)


  # MATCH --- egg to fry hockey-stick
  # population will grow to 10000 fry (stage 0)
  # and presumably 10000 * 0.8 = 8000 stage 1
  stage_k_override <- rep(NA, dat$Nstage + 1)
  stage_k_override[1] <- 10000
  names(stage_k_override) <- c("K0", dat$stage_names)
  mcheck <-
    Projection_DD(
      M.mx = M.mx,
      D.mx = D.mx,
      dat = dat,
      CE_df = NULL,
      K = 100,
      Nyears = 100,
      anadromous = TRUE,
      bh_dd_stages = c("hs_stage_0"), # MATCH
      stage_k_override = stage_k_override # MATCH
    )
  # plot(mcheck$pop$N, type = 'l')
  # Population should STILL grow exponentially
  expect_true(10000000 > median(mcheck$pop$N) && 800 < median(mcheck$pop$N))
  diff <- abs(1 - (10000 / (median(mcheck$N[, "K1"]) / 0.8)))
  expect_true(diff < 0.1)


  # MATCH --- egg to fry hockey-stick with CE stressor
  # population will grow to 10000 fry (stage 0)
  # and presumably 10000 * 0.8 = 8000 stage 1
  stage_k_override <- rep(NA, dat$Nstage + 1)
  stage_k_override[1] <- 10000
  names(stage_k_override) <- c("K0", dat$stage_names)
  mcheck <-
    Projection_DD(
      M.mx = M.mx,
      D.mx = D.mx,
      dat = dat,
      CE_df = data.frame(HUC = 1, Stressor = 1, dose = 1, sys.cap = 0.5, life_stage = "stage0", parameter = "capacity", Stressor_cat = 1),
      K = 100,
      Nyears = 100,
      anadromous = TRUE,
      bh_dd_stages = c("hs_stage_0"), # MATCH
      stage_k_override = stage_k_override # MATCH
    )
  # plot(mcheck$pop$N, type = 'l')
  # CE stressor to fry with hs
  diff <- abs(1 - ((10000 * 0.5) / (median(mcheck$N[, "K1"]) / 0.8)))
  expect_true(diff < 0.1)



  # ------------------------------------------------
  # Test Basic Beverton-Holt DD Constraints
  # ------------------------------------------------

  filename <- system.file("extdata/simple_test/test_anadromous/stage_4_anadromous.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename)
  pop_mod_setup <- pop_model_setup(life_cycles = life_cycles)

  # Test the projection matrix
  pop_mod_mat <- pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)
  A <- pop_mod_mat$projection_matrix
  lambda <- popbio::lambda(A)
  expect_true(round(lambda, 2) == 2.25) # Manual check
  lambda <- popbio::lambda(pop_mod_setup$projection_matrix)
  expect_true(round(lambda, 2) == 2.25) # Manual check

  pop_mod_mat <- pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)
  dat <- pop_mod_mat$life_histories
  M.mx <- pop_mod_mat$life_stages_symbolic
  D.mx <- pop_mod_mat$density_stage_symbolic

  stage_k_override <- rep(NA, dat$Nstage + 1)
  names(stage_k_override) <- c("K0", dat$stage_names)
  stage_k_override["stage_Pb_2"] <- 5000
  stage_k_override["stage_B_4"] <- 50

  mcheck <-
    Projection_DD(
      M.mx = M.mx,
      D.mx = D.mx,
      dat = dat,
      CE_df = NULL,
      K = 100,
      Nyears = 100,
      anadromous = TRUE,
      bh_dd_stages = c("bh_stage_Pb_2", "bh_stage_B_4"), # MATCH
      stage_k_override = stage_k_override # MATCH
    )

  # dat$stage_names
  # plot(mcheck$pop$N, type = 'l')
  # CE stressor to fry with hs
  expect_true(median(mcheck$N[, "K2"]) < 5000 &  median(mcheck$N[, "K2"]) > 2000)

  stage_k_override <- rep(NA, dat$Nstage + 1)
  names(stage_k_override) <- c("K0", dat$stage_names)

  mcheck <-
    Projection_DD(
      M.mx = M.mx,
      D.mx = D.mx,
      dat = dat,
      CE_df = NULL,
      K = 200, # Try with 200 spawners
      Nyears = 100,
      anadromous = TRUE,
      bh_dd_stages = c("bh_spawners"), # MATCH
      stage_k_override = stage_k_override # MATCH
    )
  # plot(mcheck$pop$N, type = 'l')
  # CE stressor to fry with hs

  # Should be around 200 spawners
  expect_true(median(mcheck$pop$N) < 250 &  median(mcheck$pop$N) > 150)



  # Check if we can do parr capacity and spawner capacity
  stage_k_override <- rep(NA, dat$Nstage + 1)
  names(stage_k_override) <- c("K0", dat$stage_names)
  stage_k_override[1] <- 200000

  # 25159538.5 * 0.06

  mcheck <-
    Projection_DD(
      M.mx = M.mx,
      D.mx = D.mx,
      dat = dat,
      CE_df = NULL,
      K = 15000, # Try with 15000 spawners
      Nyears = 200,
      anadromous = TRUE,
      bh_dd_stages = c("hs_stage_0", "bh_spawners"), # MATCH
      stage_k_override = stage_k_override # MATCH
    )
  plot(mcheck$pop$N[50:200], type = 'l')

  # Should be around 200 spawners
  expect_true(median(mcheck$pop$N) < 15000 &  median(mcheck$pop$N) > 5000)

  if(FALSE) {
    # Manual Run Setup
    # Set the K.adj (K adjustment prior to pop model run)
    M.mx = M.mx
    D.mx = D.mx
    H.mx = NULL
    dat = dat
    K = dat$Ka
    Nyears = 500
    p.cat = 0
    CE_df = NULL
    stage_k_override = NULL
    bh_dd_stages = NULL
    K_adj = FALSE
    anadromous = pop_mod_setup$anadromous
    alt_return = NULL
  }


})

