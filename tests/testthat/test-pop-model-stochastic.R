test_that("Test pop model stochastic", {


  expect_true(1 == 1)


  #----------------------------------------------------------
  # p.cat - probability of catastrophe
  #----------------------------------------------------------

  # per generation

  # Test density independent growth
  filename_lc <-
    system.file("extdata", "life_cycles.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename_lc)

  quick_test <- function(life_cycles, p.cat = 0.05) {
    pop_mod_setup <- pop_model_setup(life_cycles = life_cycles)
    pop_mod_mat <-
      pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)
    life_histories <- pop_mod_mat$life_histories
    life_stages_symbolic <- pop_mod_mat$life_stages_symbolic
    density_stage_symbolic <- pop_mod_mat$density_stage_symbolic
    baseline <-
      Projection_DD(
        M.mx = life_stages_symbolic,
        D.mx = density_stage_symbolic,
        H.mx = NULL,
        dat = life_histories,
        K = life_histories$Ka,
        Nyears = 1000,
        p.cat = p.cat,
        CE_df = NULL
      )
    df <- baseline$pop
    return(df)
  }

  par(mfrow = c(2,2))

  df <- quick_test(life_cycles, p.cat = 0)
  head(df)
  plot(df$year, df$N, type = 'l', ylim = c(0, 150), xlab = "Simulation Year", ylab = "N Adults", main = "p.cat = 0")
  check1 <- sd(df$N)

  df <- quick_test(life_cycles, p.cat = 0.05)
  head(df)
  plot(df$year, df$N, type = 'l', ylim = c(0, 150), xlab = "Simulation Year", ylab = "N Adults", main = "p.cat = 0.05")
  check2 <- sd(df$N)

  df <- quick_test(life_cycles, p.cat = 0.20)
  head(df)
  plot(df$year, df$N, type = 'l', ylim = c(0, 150), xlab = "Simulation Year", ylab = "N Adults", main = "p.cat = 0.20")
  check3 <- sd(df$N)

  df <- quick_test(life_cycles, p.cat = 0.80)
  head(df)
  plot(df$year, df$N, type = 'l', ylim = c(0, 150), xlab = "Simulation Year", ylab = "N Adults", main = "p.cat = 0.80")
  check4 <- sd(df$N)

  expect_true(check4 > check3)
  expect_true(check3 > check2)
  # expect_true(check2 > check1)


  # ==============================================
  # Review p.cat
  par(mfrow = c(1,1))
  p.cat <- 0.999
  Nyears <- 100000
  gen_time <- 3

  # Scaled to the average generation time from the projection matrix
  Catastrophe <- sample(
    c(1, 0),
    Nyears,
    replace = TRUE,
    prob = c(p.cat / gen_time, 1 - p.cat / gen_time)
  )

  e.cat <- sapply(Catastrophe, function(x) {
    ifelse(x == 0, NA, stats::rbeta(1, shape1 = 0.762, shape2 = 1.5) * (1 - .5) + .5)
  })
  vals <- e.cat[!(is.na(e.cat))]
  d <- density(vals) # returns the density data
  plot(d, main = "Catastrophic Events (Reed et al. 2003)", xlab = "Global Mortality (effect size per catastrophe)", ylab = "Relative frequency per catastrophe")


  # Annual probability of catastrophe is dependent on the generation
  # time of the population
  mfill <- matrix(NA, 5, 5)
  for(i in 1:5) {
    for(j in 1:5) {
      this_prob <- seq(0.05, 0.25, by = 0.05)[j]
      mfill[i, j] <- round(this_prob / i, 3)
    }
  }
  colnames(mfill) <- seq(0.05, 0.25, by = 0.05)
  rownames(mfill) <- seq(1, 5, by = 1)



  #----------------------------------------------------------
  # Evaluate and test stochastic population model parameters
  #----------------------------------------------------------
  # eps_sd   --  variance in eggs per female: 1000
  # egg_rho  --  correlation in egg fecundity through time: 0.1
  # M.cv     --  coefficient of variation in stage-specific mortality: 0.1
  # M.rho    --  correlation in mortality through time: 0.1


  #----------------------------------------------------------
  # eps_sd   --  stdev in eggs per female: 1000
  #----------------------------------------------------------
  eps = c(1000)
  eps_sd = 250
  egg_rho = 0.5
  Nyears = 250
  f_rand(mn = eps, sigma = eps_sd, rho = egg_rho)


  par(mfrow = c(2, 2))
  egg_rho = 0.1

  # Look at egg function
  ft <- lapply(1:(Nyears + 1), function(x) {
    f_temp <- f_rand(mn = eps,
                     sigma = eps_sd,
                     rho = egg_rho)
    f_temp <- ifelse(is.na(f_temp), eps, f_temp)
    names(f_temp) <- "eps"
    return(f_temp)
  })
  ft <- unlist(ft)
  ft1 <- ft

  hist(
    ft,
    breaks = 30,
    xlab = "Eggs Per Spawner (eps)",
    ylab = "Sample Frequency",
    main = NA
  )
  abline(v = eps, col = 'red', lwd = 3)
  abline(
    v = eps + eps_sd,
    col = 'blue',
    lwd = 2,
    lty = 2
  )
  abline(
    v = eps - eps_sd,
    col = 'blue',
    lwd = 2,
    lty = 2
  )

  plot(
    ft,
    type = 'l',
    ylab = "Eggs Per Spawner (eps)",
    xlab = "Simulation Years",
    ylim = c(0, 4000)
  )
  abline(h = eps, col = 'red', lwd = 3)
  abline(
    h = eps + eps_sd,
    col = 'blue',
    lwd = 2,
    lty = 2
  )
  abline(
    h = eps - eps_sd,
    col = 'blue',
    lwd = 2,
    lty = 2
  )

  mtext(
    paste0("Stochastic Fecundity: eps = ", eps, "; eps_sd = ", eps_sd),
    side = 3,
    line = -2.5,
    outer = TRUE
  )


  # INCREASE
  eps = c(1000)
  eps_sd = 750
  egg_rho = 0.5
  Nyears = 250
  f_rand(mn = eps, sigma = eps_sd, rho = egg_rho)


  ft <- lapply(1:(Nyears + 1), function(x) {
    f_temp <- f_rand(mn = eps,
                     sigma = eps_sd,
                     rho = egg_rho)
    f_temp <- ifelse(is.na(f_temp), eps, f_temp)
    names(f_temp) <- "eps"
    return(f_temp)
  })
  ft <- unlist(ft)
  ft2 <- ft

  hist(
    ft,
    breaks = 30,
    xlab = "Eggs Per Spawner (eps)",
    ylab = "Sample Frequency",
    main = NA
  )
  abline(v = eps, col = 'red', lwd = 3)
  abline(
    v = eps + eps_sd,
    col = 'blue',
    lwd = 2,
    lty = 2
  )
  abline(
    v = eps - eps_sd,
    col = 'blue',
    lwd = 2,
    lty = 2
  )

  plot(
    ft,
    type = 'l',
    ylab = "Eggs Per Spawner (eps)",
    xlab = "Simulation Years",
    ylim = c(0, 4000)
  )
  abline(h = eps, col = 'red', lwd = 3)
  abline(
    h = eps + eps_sd,
    col = 'blue',
    lwd = 2,
    lty = 2
  )
  abline(
    h = eps - eps_sd,
    col = 'blue',
    lwd = 2,
    lty = 2
  )

  mtext(
    paste0("Stochastic Fecundity: eps = ", eps, "; eps_sd = ", eps_sd),
    side = 3,
    line = -21,
    outer = TRUE
  )



  # Do some quick tests......
  expect_true(sd(ft1) < sd(ft2))
  diff <- abs(mean(ft1) / mean(ft2) - 1)
  expect_true(diff < 1)



  #----------------------------------------------------------
  # egg_rho   --  correlation in egg fecundity through time
  #----------------------------------------------------------
  eps = c(500, 1000, 1500)
  eps_sd = c(500, 1000, 1500) * 0.4
  Nyears = 250
  f_rand(mn = eps, sigma = eps_sd, rho = egg_rho)

  par(mfrow = c(2, 2))


  egg_rho = 0.95
  # Look at egg function
  ft <- lapply(1:(Nyears + 1), function(x) {
    f_temp <- f_rand(mn = eps,
                     sigma = eps_sd,
                     rho = egg_rho)
    f_temp <- ifelse(is.na(f_temp), eps, f_temp)
    names(f_temp) <- "eps"
    return(f_temp)
  })
  fecvec <- do.call("rbind", ft)
  colnames(fecvec) <- c("class_1", "class_2", "class_3")
  fecvec <- as.data.frame(fecvec)



  plot(
    fecvec$class_3[1:50],
    type = 'l',
    ylab = "Eggs Per Spawner (eps)",
    xlab = "Simulation Years",
    main = NA,
    ylim = c(0, 3000),
    col = "red"
  )
  points(fecvec$class_2[1:50], type = 'l', col = "green")
  points(fecvec$class_1[1:50], type = 'l', col = "blue")

  # Run a test here
  plot(fecvec$class_1, fecvec$class_3, xlab = NA, ylab = NA)
  mtext("eps: class 1", 1, col = "blue", line = 3)
  mtext("eps: class 3", 2, col = "red", line = 3)
  z <- lm(fecvec$class_2 ~ fecvec$class_1)
  y <- fecvec$class_2 / mean(fecvec$class_2)
  x <- fecvec$class_1 / mean(fecvec$class_1)
  z <- lm(y ~ x)
  coef_95 <- coef(z)[[2]]

  mtext(
    paste0("egg_rho = ", egg_rho, " (high)"),
    side = 3,
    line = -2,
    outer = TRUE
  )



  egg_rho = 0.05
  # Look at egg function
  ft <- lapply(1:(Nyears + 1), function(x) {
    f_temp <- f_rand(mn = eps,
                     sigma = eps_sd,
                     rho = egg_rho)
    f_temp <- ifelse(is.na(f_temp), eps, f_temp)
    names(f_temp) <- "eps"
    return(f_temp)
  })
  fecvec <- do.call("rbind", ft)
  colnames(fecvec) <- c("class_1", "class_2", "class_3")
  fecvec <- as.data.frame(fecvec)

  plot(
    fecvec$class_3[1:50],
    type = 'l',
    ylab = "Eggs Per Spawner (eps)",
    xlab = "Simulation Years",
    main = NA,
    ylim = c(0, 3000),
    col = "red"
  )
  points(fecvec$class_2[1:50], type = 'l', col = "green")
  points(fecvec$class_1[1:50], type = 'l', col = "blue")


  # Run a test here
  plot(fecvec$class_1, fecvec$class_3, xlab = NA, ylab = NA)
  mtext("eps: class 1", 1, col = "blue", line = 3)
  mtext("eps: class 2", 2, col = "red", line = 3)

  mtext(
    paste0("egg_rho = ", egg_rho, " (low)"),
    side = 3,
    line = -21,
    outer = TRUE
  )

  #legend("topright", c("Stage A","Stage B", "Stage C"),
  #       lwd = 1, col = c("red", "green", "blue"))

  # Run a test here
  # plot(fecvec$class_1/mean(fecvec$class_1), fecvec$class_2/mean(fecvec$class_2))
  y <- fecvec$class_2 / mean(fecvec$class_2)
  x <- fecvec$class_1 / mean(fecvec$class_1)
  z <- lm(y ~ x)
  coef_05 <- coef(z)[[2]]
  expect_true(coef_95 > coef_05)






  #----------------------------------------------------------
  # M.cv     --  coefficient of variation in stage-specific mortality: 0.1
  # M.rho    --  correlation in mortality through time: 0.1
  #----------------------------------------------------------

  par(mfrow = c(2, 2))


  M.rho = 0.95
  S = c(0.5, 0.15, 0.1, 0.05)
  M.cv = 0.4
  Nyears = 200
  s_temp <- s_rand(mn = S, cv = M.cv, rho = M.rho)

  # Look at egg function
  st <- lapply(1:(Nyears + 1), function(x) {
    s_temp <- s_rand(S, M.cv, rho = M.rho)
    s_temp <- ifelse(is.na(s_temp), 0, s_temp)
    s_temp
  })

  survec <- do.call("rbind", st)
  colnames(survec) <- paste0("class_", seq(1:ncol(survec)))
  survec <- as.data.frame(survec)
  head(survec, 3)

  plot(
    survec$class_1[1:50],
    type = 'l',
    ylab = "Survival Prob. (0 - 1)",
    xlab = "Simulation Years",
    ylim = c(0, 0.7),
    col = "black",
    lty = 2,
    main = NA
  )
  points(survec$class_2[1:50], type = 'l', col = "red")
  points(survec$class_3[1:50], type = 'l', col = "green")
  points(survec$class_4[1:50], type = 'l', col = "blue")

  # Run a test here
  plot(survec$class_3, survec$class_4, xlab = NA, ylab = NA,
       xlim = c(0, 0.4), ylim = c(0, 0.3))
  mtext("survival: class 2", 1, col = "blue", line = 3)
  mtext("survival: class 3", 2, col = "red", line = 3)

  mtext(
    paste0("M.rho = ", M.rho, " (high)"),
    side = 3,
    line = -2,
    outer = TRUE
  )


  y <- survec$class_2 / mean(survec$class_2)
  x <- survec$class_1 / mean(survec$class_1)
  z <- lm(y ~ x)
  coef_egg <- coef(z)[[2]]

  y <- survec$class_2 / mean(survec$class_2)
  x <- survec$class_3 / mean(survec$class_3)
  z <- lm(y ~ x)
  coef_adult_95 <- coef(z)[[2]]

  expect_true(coef_adult_95 > coef_egg)


  #####################################################




  M.rho = 0.05
  S = c(0.5, 0.15, 0.1, 0.05)
  M.cv = 0.4
  Nyears = 200
  s_temp <- s_rand(mn = S, cv = M.cv, rho = M.rho)

  # Look at egg function
  st <- lapply(1:(Nyears + 1), function(x) {
    s_temp <- s_rand(S, M.cv, rho = M.rho)
    s_temp <- ifelse(is.na(s_temp), 0, s_temp)
    s_temp
  })

  survec <- do.call("rbind", st)
  colnames(survec) <- paste0("class_", seq(1:ncol(survec)))
  survec <- as.data.frame(survec)
  head(survec, 3)

  plot(
    survec$class_1[1:50],
    type = 'l',
    ylab = "Survival Prob. (0 - 1)",
    xlab = "Simulation Years",
    ylim = c(0, 0.7),
    col = "black",
    lty = 2,
    main = NA
  )
  points(survec$class_2[1:50], type = 'l', col = "red")
  points(survec$class_3[1:50], type = 'l', col = "green")
  points(survec$class_4[1:50], type = 'l', col = "blue")

  # Run a test here
  plot(survec$class_3, survec$class_4, xlab = NA, ylab = NA,
       xlim = c(0, 0.4), ylim = c(0, 0.3))
  mtext("survival: class 2", 1, col = "blue", line = 3)
  mtext("survival: class 3", 2, col = "red", line = 3)

  mtext(
    paste0("M.rho = ", M.rho, " (low)"),
    side = 3,
    line = -21,
    outer = TRUE
  )





  y <- survec$class_2 / mean(survec$class_2)
  x <- survec$class_3 / mean(survec$class_3)
  z <- lm(y ~ x)
  coef_adult_05 <- coef(z)[[2]]

  expect_true(coef_adult_95 > coef_adult_05)



  #####################################################


})
