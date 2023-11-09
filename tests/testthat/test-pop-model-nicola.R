test_that("test-pop-model-nicola", {

  #----------------------------------------------
  # Create a 5-stage population model for coho
  filename_lc <- system.file("extdata/nicola/life_cycles_chinook.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename_lc)
  # print(life_cycles)

  expect_true(nrow(life_cycles) > 5)
  expect_true(class(life_cycles$Value) == "numeric")
  expect_true(!(any(is.na(life_cycles$Value))))

  # Setup objects for population model
  pop_mod_setup <- pop_model_setup(life_cycles = life_cycles)
  expect_true(pop_mod_setup$possible_error_state == "All Good")

  # Build matrix elements for population model
  pop_mod_mat <- pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)
  # print(pop_mod_mat$projection_matrix)

  # Preview density-independent transition projection_matrix
  A <- pop_mod_mat$projection_matrix
  # Assign nicknames for each stage
  n_stage <- life_cycles$Value[life_cycles$Name == "Nstage"]
  snames <- paste0("stage_", 1:n_stage)

  rownames(A) <- colnames(A) <- snames
  # Simple density-independent lambda estimate
  (lambda <- popbio::lambda(A))

  # Set the K.adj (K adjustment prior to pop model run)
  life_histories <- pop_mod_mat$life_histories
  # Mathematical expression of the transition matrix
  life_stages_symbolic <- pop_mod_mat$life_stages_symbolic
  # Mathematical expression of the density matrix
  density_stage_symbolic <- pop_mod_mat$density_stage_symbolic

  # --------------------------------------------------------------------
  # Run simple population projection - project forward through time
  baseline <-
    Projection_DD(
      M.mx = life_stages_symbolic,
      # projection matrix expression
      D.mx = density_stage_symbolic,
      # density-dependence matrix
      H.mx = NULL,
      dat = life_histories,
      # life history data
      K = life_histories$Ka,
      # initial pop size as stage-structure vector
      Nyears = 500,
      # years to run simulation
      p.cat = 0,      # Probability of catastrophe
      CE_df = NULL,
      stage_k_override = c(NA, 238053, NA, NA, NA, NA),
      bh_dd_stages = c("bh_stage_1")
    )

  names(baseline)
  df <- baseline$pop
  df <- df[df$year > 50, ]
  plot(df$year, df$N, type = 'l')
  expect_true(median(df$N) > 0)
  df <- baseline$N
  colMeans(df)
  tail(df)


  # --------------------------------------------------------------------
  # Load stressor magnitude and stressor response files
  # --------------------------------------------------------------------

  if(FALSE) {

    filename_rm <- system.file("extdata", "./nicola/stressor-magnitude-nicola.xlsx", package = "CEMPRA")
    filename_sr <- system.file("extdata", "./nicola/stressor-response-nicola.xlsx", package = "CEMPRA")
    dose <- StressorMagnitudeWorkbook(filename = filename_rm)
    sr_wb_dat <- StressorResponseWorkbook(filename = filename_sr)

    # Habitat and DD
    filename_hab <- system.file("extdata", "./nicola/habitat_dd_k-nicola.xlsx", package = "CEMPRA")
    habitat_dd_k <- readxl::read_excel(filename_hab, sheet = 1)
    colnames(habitat_dd_k)
    head(habitat_dd_k)
    habitat_dd_k$k_stage_1_mean <- habitat_dd_k$k_stage_1_mean * 0.75

    dose$Up_Limit <- 99
    dose$Mean <- dose$Mean * 1.15



    # Choose target ID
    HUC_ID <- dose$HUC_ID[1]
    n_pop <- 0

    jm <- CEMPRA::JoeModel_Run(
      dose = dose,
      sr_wb_dat = sr_wb_dat,
      MC_sims = 1,
      adult_sys_cap = FALSE
    )


    for(i in 1:length(dose$HUC_ID)) {

      this_id <- dose$HUC_ID[i]

      mdr <- jm$sc.dose.df[jm$sc.dose.df$HUC == this_id, ]
      sys_cap <- mdr$sys.cap[mdr$Stressor == "Summer_temperature_parr"]

      life_histories_tmp <- life_histories
      life_histories_tmp$S["s1"] <- life_histories_tmp$S["s1"] * sys_cap

      mcap <- habitat_dd_k$k_stage_1_mean[habitat_dd_k$HUC_ID == this_id]

      # Load in the habitat data optional
      baseline <-
        Projection_DD(
          M.mx = life_stages_symbolic,
          # projection matrix expression
          D.mx = density_stage_symbolic,
          # density-dependence matrix
          H.mx = NULL,
          dat = life_histories_tmp,
          # life history data
          K = life_histories$Ka,
          # initial pop size as stage-structure vector
          Nyears = 500,
          # years to run simulation
          p.cat = 0,      # Probability of catastrophe
          CE_df = NULL,
          stage_k_override = c(NA, mcap, NA, NA, NA, NA),
          bh_dd_stages = c("bh_stage_1")
        )

      msum <- baseline$pop$N
      msum <- round(msum, 0)
      n_sum <- median(tail(msum, 50))
      n_pop <- sum(c(n_pop, n_sum), na.rm = TRUE)
      print(n_pop)
    }

    # Current: 4463
    # Temp decrease by 20%: (same)
    # Temp increase by 10%: 1691.5
    # Temp increase by 20%: 2236.5
    # Fry parr habitat decrease by 20%: 1955.5
    # Temp increase by 10% & fry parr habitat decrease by 20%: 1496

    bdat <- c(4463, 2236.5, 1691.5, 1955.5, 1496)
    par(mar = c(5, 14, 1, 3))
    barplot(rev(bdat), names.arg = rev(c("Current", "Temp.+10%", "Temp.+20%", "Parr Habitat.-20%", "Temp.+20% & Parr Habitat.-20%")), horiz = TRUE, las = 1,
            xlab = "N Spawners",
            xlim = c(0, 7000))
    grid()
    box()
    abline(v = 4463, lty = 2)


  }








})
