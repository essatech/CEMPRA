test_that("test-pop-model-bh_dd", {

  # Test with static values
  st_tmp <- list()
  st_tmp[[1]] <- c(0.1368, 0.2909, 0.3383, 0.263, 0.904, 0.898)
  names(st_tmp[[1]]) <- c("sE", "s0", "s1", "s2", "s3", "s4")

  N_tmp <- c(4000, 2000, 1000, 500)
  N_prev_tmp <- c(2000, 1000, 500, 250)

  dat_tmp <- list()
  dat_tmp$Nstage <- 4


  # 1. Test that stage 1 survival is suppressed to 100 from fry to stage 1 k
  dat_tmp$Ke <- NA # Shouldn't ever be used
  dat_tmp$K0 <- 100
  dat_tmp$K <- c(NA, NA, NA, NA)
  bh_dd_stages <- c("dd_hs_0")

  res <-
    dd.N.bh(
      dat = dat_tmp,
      t = 1,
      st = st_tmp,
      N = N_tmp,
      N_prev = N_prev_tmp,
      bh_dd_stages = bh_dd_stages
    )

  expect_true(res[[1]] < 100)
  expect_true(res[[2]] ==  N_tmp[[2]])

  # 2. Assume egg and fry surv are 100% - we should see hockey stick type DD
  st_tmp2 <- st_tmp
  st_tmp2[[1]][[1]] <- 1
  st_tmp2[[1]][[2]] <- 1
  res2 <-
    dd.N.bh(
      dat = dat_tmp,
      t = 1,
      st = st_tmp2,
      N = N_tmp,
      N_prev = N_prev_tmp,
      bh_dd_stages = bh_dd_stages
    )

  expect_true(res2[[1]] == 100)
  expect_true(res[[2]] ==  N_tmp[[2]])

  # 3. If we have both egg-to-fry (sE and s0) DD and fry s0 to s1 DD
  bh_dd_stages <- c("dd_hs_0", "bh_stage_1")
  dat_tmp$K[[1]] <- 200
  dat_tmp$K0 <- 1000000000000

  res3 <-
    dd.N.bh(
      dat = dat_tmp,
      t = 1,
      st = st_tmp2,
      N = N_tmp,
      N_prev = N_prev_tmp,
      bh_dd_stages = bh_dd_stages
    )

  expect_true(res3[[1]] > 150)
  expect_true(res3[[2]] ==  N_tmp[[2]])


  # 4. If turn off the dd_hs_0 constraint we should get essentially the same
  # value as before
  bh_dd_stages <- c("bh_stage_1")
  dat_tmp$K[[1]] <- 200
  dat_tmp$K0 <- 1000000000000

  res4 <-
    dd.N.bh(
      dat = dat_tmp,
      t = 1,
      st = st_tmp2,
      N = N_tmp,
      N_prev = N_prev_tmp,
      bh_dd_stages = bh_dd_stages
    )

  expect_true(round(res3[[1]]) == round(res4[[1]]))
  expect_true(res3[[2]] ==  N_tmp[[2]])


  # 5. Send a NULL or NA value returns original vector
  bh_dd_stages <- NA

  res5 <-
    dd.N.bh(
      dat = dat_tmp,
      t = 1,
      st = st_tmp2,
      N = N_tmp,
      N_prev = N_prev_tmp,
      bh_dd_stages = bh_dd_stages
    )

  expect_true(all(res5 == N_tmp))





  # RESET static values
  st_tmp <- list()
  st_tmp[[1]] <- c(0.1368, 0.2909, 0.3383, 0.263, 0.904, 0.898)
  names(st_tmp[[1]]) <- c("sE", "s0", "s1", "s2", "s3", "s4")

  N_tmp <- c(4000, 2000, 1000, 500)
  N_prev_tmp <- c(2000, 1000, 500, 250)

  dat_tmp <- list()
  dat_tmp$Nstage <- 4


  # 6. Test that stage 1 to 2 transitions work
  dat_tmp$Ke <- NA # Shouldn't ever be used
  dat_tmp$K0 <- NA
  dat_tmp$K <- c(NA, 100, NA, NA)
  bh_dd_stages <- c("bh_stage_2")

  res6 <-
    dd.N.bh(
      dat = dat_tmp,
      t = 1,
      st = st_tmp2,
      N = N_tmp,
      N_prev = N_prev_tmp,
      bh_dd_stages = bh_dd_stages
    )

  expect_true(res6[[1]] == N_tmp[[1]])
  expect_true(res6[[2]] < 100)
  expect_true(res6[[3]] == N_tmp[[3]])

  dat_tmp$K <- c(NA, NA, 100, NA)
  bh_dd_stages <- c("bh_stage_3")

  res6 <-
    dd.N.bh(
      dat = dat_tmp,
      t = 1,
      st = st_tmp2,
      N = N_tmp,
      N_prev = N_prev_tmp,
      bh_dd_stages = bh_dd_stages
    )

  expect_true(res6[[1]] == N_tmp[[1]])
  expect_true(res6[[3]] < 100)
  expect_true(res6[[2]] == N_tmp[[2]])
  expect_true(res6[[4]] == N_tmp[[4]])


  # 7. Test that multiple adult transitions work
  dat_tmp$K <- c(NA, 100, 100, 100)
  bh_dd_stages <- c("bh_stage_2", "bh_stage_3", "bh_stage_4")

  res7 <-
    dd.N.bh(
      dat = dat_tmp,
      t = 1,
      st = st_tmp2,
      N = N_tmp,
      N_prev = N_prev_tmp,
      bh_dd_stages = bh_dd_stages
    )

  expect_true(res7[[4]] < 100)
  expect_true(res7[[3]] < 100)
  expect_true(res7[[2]] < 100)
  expect_true(res7[[1]] == N_tmp[[1]])




  dat_tmp$K <- c(100, 100, 100, 100)
  bh_dd_stages <- c("bh_stage_1", "bh_stage_2", "bh_stage_3", "bh_stage_4")

  res7 <-
    dd.N.bh(
      dat = dat_tmp,
      t = 1,
      st = st_tmp2,
      N = N_tmp,
      N_prev = N_prev_tmp,
      bh_dd_stages = bh_dd_stages
    )

  expect_true(res7[[4]] < 100)
  expect_true(res7[[3]] < 100)
  expect_true(res7[[2]] < 100)
  expect_true(res7[[1]] < 100)


})
