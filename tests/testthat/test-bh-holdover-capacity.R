# Holdover-aware total-capacity Beverton-Holt density dependence.
# Stage K values cap the TOTAL stage abundance (holdovers + recruits);
# recruits are admitted into the residual capacity (K - holdovers).
# Egg/fry capacities (Ke, K0) remain within-year flow caps.

test_that("residual-capacity BH: holdovers kept, recruits admitted into K - H", {

  # 4-stage model; stage 2 is multi-year (realized stay rate 0.4)
  A <- matrix(0, 4, 4)
  A[2, 1] <- 0.30  # stage-1 -> stage-2 transition
  A[2, 2] <- 0.40  # survive-and-stay within stage 2
  A[3, 2] <- 0.10
  A[4, 3] <- 0.50
  N_prev <- c(3000, 500, 100, 50)
  N <- as.vector(A %*% N_prev)  # stage 2 projected: 900 inflow + 200 holdover = 1100

  st <- list(c(sE = 0.1, s0 = 0.3, s1 = 0.3, s2 = 0.3, s3 = 0.9, s4 = 0.9))
  st[[2]] <- st[[1]]

  dat <- list(Nstage = 4, Ke = NA, K0 = NA, anadromous = FALSE,
              stage_names = paste0("K", 1:4))
  dat$K <- c(NA, 1000, NA, NA)
  names(dat$K) <- paste0("K", 1:4)

  res <- dd.N.bh(dat = dat, t = 1, st = st, N = N, N_prev = N_prev,
                 bh_dd_stages = "bh_stage_2", A = A)

  # 200 holdovers preserved; 900 candidates admitted into residual 800:
  # 900 / (1 + 900/800) = 423.53 -> total 623.53
  expect_equal(res[[2]], 200 + 900 / (1 + 900 / 800), tolerance = 1e-8)
  # unflagged stages untouched
  expect_equal(res[[1]], N[[1]])
  expect_equal(res[[3]], N[[3]])
  expect_equal(res[[4]], N[[4]])
})


test_that("overfull stage: residents trimmed to K, no recruits admitted", {

  A <- matrix(0, 4, 4)
  A[2, 1] <- 0.30
  A[2, 2] <- 0.40
  N_prev <- c(3000, 500, 100, 50)
  N <- as.vector(A %*% N_prev)

  st <- list(c(sE = 0.1, s0 = 0.3, s1 = 0.3, s2 = 0.3, s3 = 0.9, s4 = 0.9))
  st[[2]] <- st[[1]]

  dat <- list(Nstage = 4, Ke = NA, K0 = NA, anadromous = FALSE,
              stage_names = paste0("K", 1:4))

  # K (150) below the 200 holdovers - e.g., capacity stressor shrank K
  dat$K <- c(NA, 150, NA, NA)
  names(dat$K) <- paste0("K", 1:4)
  res <- dd.N.bh(dat = dat, t = 1, st = st, N = N, N_prev = N_prev,
                 bh_dd_stages = "bh_stage_2", A = A)
  expect_equal(res[[2]], 150)

  # NA capacity on a flagged stage = unconstrained (projection kept)
  dat$K <- c(NA, NA, NA, NA)
  names(dat$K) <- paste0("K", 1:4)
  res2 <- dd.N.bh(dat = dat, t = 1, st = st, N = N, N_prev = N_prev,
                  bh_dd_stages = "bh_stage_2", A = A)
  expect_equal(res2[[2]], N[[2]])

  # Extinction threshold applied to the trimmed total (K < 1 -> 0)
  dat$K <- c(NA, 0.5, NA, NA)
  names(dat$K) <- paste0("K", 1:4)
  res3 <- dd.N.bh(dat = dat, t = 1, st = st, N = N, N_prev = N_prev,
                  bh_dd_stages = "bh_stage_2", A = A)
  expect_equal(res3[[2]], 0)
})


test_that("stage-1 flow caps act on recruits only; holdovers preserved", {

  # Stage 1 is multi-year (stay rate 0.2); recruits arrive via fecundity
  A <- matrix(0, 4, 4)
  A[1, 1] <- 0.20        # stage-1 holdover rate (year_1 > 1)
  A[1, 3] <- 5           # fecundity into stage 1
  A[2, 1] <- 0.05
  N_prev <- c(1000, 100, 100, 10)
  N <- as.vector(A %*% N_prev)  # N[1] = 200 holdover + 500 recruits = 700

  st <- list(c(sE = 0.1, s0 = 0.3, s1 = 0.3, s2 = 0.3, s3 = 0.9, s4 = 0.9))
  st[[2]] <- st[[1]]

  dat <- list(Nstage = 4, Ke = NA, K0 = 600, anadromous = FALSE,
              stage_names = paste0("K", 1:4))
  dat$K <- c(800, NA, NA, NA)
  names(dat$K) <- paste0("K", 1:4)

  # dd_hs_0 flow cap only: recruits 500 -> fry 500/0.3 = 1666.7 -> capped
  # at K0 = 600 -> recruits become 600 * 0.3 = 180; 200 holdovers untouched
  res <- dd.N.bh(dat = dat, t = 1, st = st, N = N, N_prev = N_prev,
                 bh_dd_stages = "dd_hs_0", A = A)
  expect_equal(res[[1]], 200 + 600 * 0.3, tolerance = 1e-8)

  # add bh_stage_1: capped recruits admitted into residual 800 - 200 = 600
  res2 <- dd.N.bh(dat = dat, t = 1, st = st, N = N, N_prev = N_prev,
                  bh_dd_stages = c("dd_hs_0", "bh_stage_1"), A = A)
  inflow1 <- 600 * 0.3
  expect_equal(res2[[1]], 200 + inflow1 / (1 + inflow1 / 600), tolerance = 1e-8)

  # stage-1 overfull: K1 below the 200 holdovers -> trim to K1
  dat$K <- c(120, NA, NA, NA)
  names(dat$K) <- paste0("K", 1:4)
  res3 <- dd.N.bh(dat = dat, t = 1, st = st, N = N, N_prev = N_prev,
                  bh_dd_stages = c("dd_hs_0", "bh_stage_1"), A = A)
  expect_equal(res3[[1]], 120)
})


test_that("non-binding stage K is a no-op on the full projection", {

  filename_lc <- system.file("extdata", "life_cycles.csv", package = "CEMPRA")
  lc <- read.csv(filename_lc, stringsAsFactors = FALSE)
  me <- pop_model_matrix_elements(pop_model_setup(lc))
  lh <- me$life_histories

  # Canonical profile has multi-year stages (year_2 = 2, year_4 = 5).
  # Stage-2 K = Inf can never bind: flagged run must equal unflagged run.
  ovr <- c(NA, NA, Inf, NA, NA)

  set.seed(99)
  base <- suppressWarnings(
    Projection_DD(M.mx = me$life_stages_symbolic,
                  D.mx = me$density_stage_symbolic,
                  dat = lh, Nyears = 100, K = as.numeric(lh$Ka), p.cat = 0,
                  stage_k_override = ovr)
  )
  set.seed(99)
  flagged <- suppressWarnings(
    Projection_DD(M.mx = me$life_stages_symbolic,
                  D.mx = me$density_stage_symbolic,
                  dat = lh, Nyears = 100, K = as.numeric(lh$Ka), p.cat = 0,
                  stage_k_override = ovr, bh_dd_stages = c("bh_stage_2"))
  )

  expect_equal(flagged$pop$N, base$pop$N)
  expect_equal(flagged$N, base$N)
})


test_that("catastrophe mortality is retained on BH-flagged stages", {

  filename_lc <- system.file("extdata", "life_cycles.csv", package = "CEMPRA")
  lc <- read.csv(filename_lc, stringsAsFactors = FALSE)
  me <- pop_model_matrix_elements(pop_model_setup(lc))
  lh <- me$life_histories

  ovr <- c(NA, NA, 200, NA, NA)  # binding stage-2 K

  set.seed(5)
  run <- suppressWarnings(
    Projection_DD(M.mx = me$life_stages_symbolic,
                  D.mx = me$density_stage_symbolic,
                  dat = lh, Nyears = 150, K = as.numeric(lh$Ka), p.cat = 0.8,
                  stage_k_override = ovr, bh_dd_stages = c("bh_stage_2"))
  )

  cat_years <- which(run$Cat.$Cat. == 1)
  expect_true(length(cat_years) > 0)

  # Catastrophic mortality applies IN ADDITION to the normal annual
  # projection: the year is projected and capacity-capped first, then the
  # kill fraction is applied last, so nothing can rebuild killed fish.
  for (tt in cat_years) {
    kill_frac <- 1 - run$Cat.$e.cat[tt]

    # The capped stage (K2 = 200) can hold at most 200 before the kill,
    # so after the kill it must sit at or below 200 * (1 - e.cat):
    # no density-dependence pathway may resurrect individuals.
    expect_true(run$N[tt + 1, 2] <= 200 * kill_frac + 1e-8)

    # The year is NOT a frozen rescale of the previous vector: aging,
    # spawning and natural mortality still ran, so stages change by
    # different factors rather than one common scaling.
    prev <- run$N[tt, ]
    if (all(prev > 0)) {
      ratios <- run$N[tt + 1, ] / prev
      expect_true(max(ratios) - min(ratios) > 1e-6)
    }
  }
})


test_that("binding K still constrains under the new semantics", {

  filename_lc <- system.file("extdata", "life_cycles.csv", package = "CEMPRA")
  lc <- read.csv(filename_lc, stringsAsFactors = FALSE)
  me <- pop_model_matrix_elements(pop_model_setup(lc))
  lh <- me$life_histories

  set.seed(21)
  loose <- suppressWarnings(
    Projection_DD(M.mx = me$life_stages_symbolic,
                  D.mx = me$density_stage_symbolic,
                  dat = lh, Nyears = 150, K = as.numeric(lh$Ka), p.cat = 0,
                  stage_k_override = c(NA, NA, 1e6, NA, NA),
                  bh_dd_stages = c("bh_stage_2"))
  )
  set.seed(21)
  tight <- suppressWarnings(
    Projection_DD(M.mx = me$life_stages_symbolic,
                  D.mx = me$density_stage_symbolic,
                  dat = lh, Nyears = 150, K = as.numeric(lh$Ka), p.cat = 0,
                  stage_k_override = c(NA, NA, 50, NA, NA),
                  bh_dd_stages = c("bh_stage_2"))
  )

  # Stage-2 abundance must respect the total cap every year
  expect_true(all(tight$N[-1, 2] <= 50 + 1e-8))
  # and the tight cap must suppress the population relative to the loose one
  expect_true(mean(tight$pop$N[51:151]) < mean(loose$pop$N[51:151]))
})
