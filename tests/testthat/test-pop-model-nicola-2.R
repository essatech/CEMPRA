test_that("test-pop-model-nicola-2", {

  # Load dummy CE_df
  filename <- system.file("extdata/simple_test/CE_df_testing.csv", package = "CEMPRA")
  CE_df <- read.csv(filename, stringsAsFactors = FALSE)


  #----------------------------------------------
  # Test Custom Matrix
  #----------------------------------------------
  # two stage with maturation at age-3
  filename <- system.file("extdata/simple_test/test_anadromous/chinook_life_cycle_nicola_2.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename)

  # life_cycles$Value[life_cycles$Name == "S0"] <- 0.2

  pop_mod_setup <- pop_model_setup(life_cycles = life_cycles)

  # Test the projection matrix
  pop_mod_mat <- pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)
  A <- pop_mod_mat$projection_matrix
  lambda <- popbio::lambda(A)
  lambda
  popbio::generation.time(A)

  expect_true(round(lambda, 2) == 1.55) # Manual check
  lambda <- popbio::lambda(pop_mod_setup$projection_matrix)
  expect_true(round(lambda, 2) == 1.55) # Manual check

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

  # Think about Spius (380... 15 to 1850)
  # Redds per km = 4 (0 - 9)
  # spawner cap: 4000 * 4/km (hs)
  # Spawning habitat SP1 = 11.2
  # Spawning habitat SP2-1 = 7.9
  # Spawning habitat SP2-2 = 6.557
  # Spawning habitat MA1 = 20
  # spawn_km = 11.2 + 7.9 + 6.557 + 20
  # 380/spawn_km = 8.8 redds per km...

  # Stream habitat parr 0.05 per m2
  # assume 5-10m wide spawn_km * 1000 * 10 =  456570m2
  # 456570 * 0.25 = 228285 fry
  # 456570 * 0.025 = 11414 parr
  # Spius max spawn 1800 - 3000

  stage_k_override <- rep(NA, dat$Nstage + 1)
  names(stage_k_override) <- c("K0", dat$stage_names)
  stage_k_override["K0"] <- 250000
  # stage_k_override["K0"] <- 500000
  stage_k_override["stage_Pb_1"] <- 12000

  baseline <-
    Projection_DD(
      M.mx = M.mx,
      D.mx = D.mx,
      dat = dat,
      K = 1000,
      #bh_dd_stages = c("bh_stage_0", "hs_spawners", "bh_stage_1"),
      bh_dd_stages = c("bh_stage_0", "bh_stage_1", "bh_spawners"),
      stage_k_override = stage_k_override,
      Nyears = 500,
      anadromous = pop_mod_setup$anadromous
    )
  baseline$pop$N
  spius_pop <- median(baseline$pop$N)
  plot(baseline$pop$N[50:200], type = 'l', ylim = c(0, 800))


  # ---------------------------
  # Coldwater
  # ---------------------------

  # Think about Coldwater (650... 15 to 2100)
  # Redds per km = 4 (0 - 9)
  # spawner cap: 4000 * 4/km (hs)
  # Spawning habitat CW1-1 = R: 25 | S: 0 | w: 12 | a: 933,412
  # Spawning habitat CW1-2 = R: 10 | S: 0 | w: 12 | a: 392,362
  # Spawning habitat CW2-1 = R: 14 | S: 1 | w: 29 | a: 441,131
  # Spawning habitat CW2-2 = R: 25 | S: 25 |w: 12 | a: 769,252
  # R_km = 25 + 10 + 14 + 25 # 74
  # S_km = 0 + 0 + 1 + 25 # 26
  # a_sum = 933412 + 392362 + 441131 + 769252 # 2536157
  # 650/S_km = 25 redds per km...
  # 650/R_km = 8.8 redds per km...

  # Stream habitat parr 0.05 per m2
  # 2536157 * 0.25 = 634039 fry
  # 2536157 * 0.025 ~ 0.02 = 63403 parr
  # Coldwater max spawn 2100 - 6000

  # 50000/2536157 = 0.0197 Parr per m2


  stage_k_override <- rep(NA, dat$Nstage + 1)
  names(stage_k_override) <- c("K0", dat$stage_names)
  stage_k_override["K0"] <- 500000 # 600000
  # stage_k_override["K0"] <- 500000
  stage_k_override["stage_Pb_1"] <- 50000 # 63403

  baseline <-
    Projection_DD(
      M.mx = M.mx,
      D.mx = D.mx,
      dat = dat,
      K = 2000, # 2000
      #bh_dd_stages = c("bh_stage_0", "hs_spawners", "bh_stage_1"),
      bh_dd_stages = c("bh_stage_0", "bh_stage_1", "bh_spawners"),
      stage_k_override = stage_k_override,
      Nyears = 500,
      anadromous = pop_mod_setup$anadromous
    )

  median(baseline$pop$N)
  coldwater_pop <- median(baseline$pop$N)
  round(coldwater_pop / spius_pop, 2) # should be like 1.7-4X
  plot(baseline$pop$N[50:200], type = 'l', ylim = c(0, 5000))




  # ---------------------------
  # Nicola
  # ---------------------------

  # Think about Nicola (5500... 500 to 17777)
  # Redds per km = 4 (0 - 9)
  # spawner cap: 4000 * 4/km (hs)
  # Spawning habitat SK1   =  R: 0.65 | S: 0.65 | w: 5-10 | a: 3250
  # Spawning habitat GU1-1 =  R: 5.2  | S: 5.2  | w: 10   | a: 52000
  # Spawning habitat GU1-2 =  R: 25   | S: 0.5  | w: 5-10 | a: 125000
  # Spawning habitat N1-1  =  R: 33.4 | S: 0    | w: 20   | a: 668000
  # Spawning habitat N1-2  =  R: 15   | S: 15   | w: 21   | a: 920408
  # Spawning habitat N2-1  =  R: 13   | S: 1    | w: 41   | a: 639953
  # Spawning habitat N2-2  =  R: 8.6  | S: 0    | w: 40   | a: 406370
  # Spawning habitat N3    =  R: 25   | S: 25   | w: 35   | a: 539845
  # Ignore 4th remnant pop in Upper Nicola...

  loc_names <- c("SK1", "GU1-1", "GU1-2", "N1-1", "N1-2", "N2-1", "N2-2", "N3")
  R_km_vec <- c(0.65, 5.2, 25, 33.4, 15, 13, 8.6, 25)
  R_km <- sum(R_km_vec)
  S_km_vec <- c(0, 5.2, 0, 0, 15, 1, 0, 25)
  S_km <- sum(S_km_vec)
  a_vec    <- c(3250, 52000, 125000, 668000, 920408, 639953, 406370, 539845)
  p_riffle <- c(0.3, 0.3, 0.3, 0.55, 0.55, 0.41, 0.36, 0.15)
  names(p_riffle) <- loc_names
  p_riffle
  a_sum <- sum(a_vec)

  # Beechie et al 2021 assume large stream >20m
  5500/S_km # 116 redds per km... (TOO HIGH)
  5500/R_km # 43 redds per km... (TOO HIGH)

  # Get portion riffle (SHIMS: )
  a_sum_spawn <- a_vec * (S_km_vec/R_km_vec) # Portion spawning habitat
  a_riffle <- a_sum_spawn * p_riffle * 0.75  # Riffle area
  a_riffle_sum <- sum(a_riffle)
  total_redds <- a_riffle_sum/14.1 # 14.1m2/redd
  10000/total_redds
  total_redds <- 18000

  # Stream habitat parr 0.0 per m2

  stage_k_override <- rep(NA, dat$Nstage + 1)
  names(stage_k_override) <- c("K0", dat$stage_names)

  stage_k_override["K0"] <- a_sum * 0.5 # 0.25

  # stage_k_override["K0"] <- 500000
  stage_k_override["stage_Pb_1"] <- a_sum * 0.05 # 0.05

  baseline <-
    Projection_DD(
      M.mx = M.mx,
      D.mx = D.mx,
      dat = dat,
      K = total_redds, # 37820
      bh_dd_stages = c("bh_spawners", "bh_stage_0", "bh_stage_1"),
      stage_k_override = stage_k_override,
      Nyears = 500,
      anadromous = pop_mod_setup$anadromous
    )

  median(baseline$pop$N) # close to 5500
  nicola_pop <- median(baseline$pop$N)
  round(nicola_pop / coldwater_pop, 2) # should be like ~8X
  round(nicola_pop / spius_pop, 2)     # should be like ~14 - 24X
  plot(baseline$pop$N[50:200], type = 'l', ylim = c(0, 25000))






  # Ensure that we still get an effect of temperature and flow
  # JUV AND TEMP....
  ce <-
    Projection_DD(
      M.mx = M.mx,
      D.mx = D.mx,
      CE_df = data.frame(sys.cap = 0.8, life_stage = "stage_0, stage_1", parameter = "survival"),
      dat = dat,
      K = 20000, # 37820
      bh_dd_stages = c("bh_spawners", "bh_stage_0", "bh_stage_1"),
      stage_k_override = stage_k_override,
      Nyears = 500,
      anadromous = pop_mod_setup$anadromous
    )
  plot(baseline$pop$N[50:200], type = 'l', ylim = c(0, max(baseline$pop$N[50:200])*1.2))
  points(ce$pop$N[50:200], type = 'l', col = "red")
  expect_true(mean(ce$pop$N) < mean(baseline$pop$N))



  # Ensure that we still get an effect of temperature and flow
  # PRE-SPAWN....
  ce <-
    Projection_DD(
      M.mx = M.mx,
      D.mx = D.mx,
      CE_df = data.frame(sys.cap = 0.5, life_stage = "u", parameter = "survival"),
      dat = dat,
      K = total_redds, # 37820
      bh_dd_stages = c("bh_spawners", "bh_stage_0", "bh_stage_1"),
      stage_k_override = stage_k_override,
      Nyears = 500,
      anadromous = pop_mod_setup$anadromous
    )
  plot(baseline$pop$N[50:200], type = 'l', ylim = c(0, max(baseline$pop$N[50:200])*1.2))
  points(ce$pop$N[50:200], type = 'l', col = "red")
  expect_true(mean(ce$pop$N) < mean(baseline$pop$N))



  # Ensure that we still get an effect of temperature and flow
  # JUV AND TEMP....
  ce <-
    Projection_DD(
      M.mx = M.mx,
      D.mx = D.mx,
      CE_df = data.frame(sys.cap = 0.15, life_stage = "spawners", parameter = "capacity"),
      dat = dat,
      K = total_redds, # 37820
      bh_dd_stages = c("bh_spawners", "bh_stage_0", "bh_stage_1"),
      stage_k_override = stage_k_override,
      Nyears = 500,
      anadromous = pop_mod_setup$anadromous
    )
  plot(baseline$pop$N[50:200], type = 'l', ylim = c(0, max(baseline$pop$N[50:200])*1.2))
  points(ce$pop$N[50:200], type = 'l', col = "red")
  expect_true(mean(ce$pop$N) < mean(baseline$pop$N))

  ce <-
    Projection_DD(
      M.mx = M.mx,
      D.mx = D.mx,
      CE_df = data.frame(sys.cap = 0.5, life_stage = "stage_0", parameter = "capacity"),
      dat = dat,
      K = total_redds, # 37820
      bh_dd_stages = c("bh_spawners", "bh_stage_0", "bh_stage_1"),
      stage_k_override = stage_k_override,
      Nyears = 500,
      anadromous = pop_mod_setup$anadromous
    )
  plot(baseline$pop$N[50:200], type = 'l', ylim = c(0, max(baseline$pop$N[50:200])*1.2))
  points(ce$pop$N[50:200], type = 'l', col = "red")
  expect_true(mean(ce$pop$N) < mean(baseline$pop$N))

  ce <-
    Projection_DD(
      M.mx = M.mx,
      D.mx = D.mx,
      CE_df = data.frame(sys.cap = 0.5, life_stage = "stage_1", parameter = "capacity"),
      dat = dat,
      K = total_redds, # 37820
      bh_dd_stages = c("bh_spawners", "bh_stage_0", "bh_stage_1"),
      stage_k_override = stage_k_override,
      Nyears = 500,
      anadromous = pop_mod_setup$anadromous
    )
  plot(baseline$pop$N[50:200], type = 'l', ylim = c(0, max(baseline$pop$N[50:200])*1.2))
  points(ce$pop$N[50:200], type = 'l', col = "red")
  expect_true(mean(ce$pop$N) < mean(baseline$pop$N))




})
