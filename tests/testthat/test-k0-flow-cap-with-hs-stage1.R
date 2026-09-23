# The K0 egg->fry flow cap must survive an hs_stage_1 flag (issue 1.9,
# fixed 2026-09-22).
#
# Flagging a fry flow cap (k_stage_0 via bh_stage_0/hs_stage_0/dd_hs_0)
# together with a stage-1 hockey-stick cap (hs_stage_1) is a legal
# configuration: a nursery bottleneck plus a juvenile-habitat ceiling.
# Previously the hs_stage_1 branch of the stage-1 chain skipped the
# recruit adjustment entirely, silently discarding the flow cap: with
# K0 = 50 and s0 = 0.5, 25 recruits should be admitted, but adding a
# non-binding hs_stage_1 let all 5,000 projected recruits through.
# Fixed: the branch now takes min(HS-capped total, holdovers +
# flow-capped recruits), honouring both ceilings.

toy_dat <- function(K1 = 1e6, K0 = 50) {
  list(anadromous = FALSE, Nstage = 3, stage_names = c("K1", "K2", "K3"),
       mat = c(mat2 = 0, mat3 = 1), K = c(K1, NA, NA), K0 = K0,
       stage_k_spawners = NA,
       Surv_annual = c(sE = 0.1, s0 = 0.5, s1 = 0.3, s2 = 0.5, s3 = 0.9))
}
st_t <- c(sE = 0.1, s0 = 0.5, s1 = 0.3, s2 = 0.5, s3 = 0.9)
run_dd <- function(dat, N, flags, A = matrix(0, 3, 3), N_prev = N) {
  dd.N.bh(dat = dat, t = 1, st = list(st_t, st_t), N = N, N_prev = N_prev,
          bh_dd_stages = flags, A = A)
}

test_that("K0 flow cap survives a non-binding hs_stage_1", {
  N <- c(5000, 100, 20)   # 5000 projected recruits, no holdovers (A = 0)

  # flow cap alone: 50 fry * s0 = 25 recruits
  expect_equal(run_dd(toy_dat(), N, c("hs_stage_0"))[1], 25)

  # + non-binding hs_stage_1 (K1 = 1e6): flow cap must still hold
  # (previously all 5000 passed)
  expect_equal(run_dd(toy_dat(), N, c("hs_stage_0", "hs_stage_1"))[1], 25)

  # Beverton-Holt flow variant with hs_stage_1: BH-capped fry * s0
  r_bh <- run_dd(toy_dat(), N, c("bh_stage_0", "hs_stage_1"))[1]
  expect_lt(r_bh, 26)     # BH asymptote is below the K0 hard cap
  expect_gt(r_bh, 0)
})

test_that("binding hs_stage_1 still wins when it is the tighter ceiling", {
  N <- c(5000, 100, 20)
  # K1 = 10 < flow-capped recruits (25): the HS total cap binds
  out <- run_dd(toy_dat(K1 = 10), N, c("hs_stage_0", "hs_stage_1"))
  expect_equal(out[1], 10)
})

test_that("holdovers are preserved - only recruits are flow-capped", {
  # A[1,1] = 0.4 with N_prev[1] = 1000 -> 400 holdovers; projected total
  # 5400 = 400 holdovers + 5000 recruits
  A <- matrix(0, 3, 3); A[1, 1] <- 0.4
  N_prev <- c(1000, 100, 20)
  N <- c(5400, 100, 20)
  out <- run_dd(toy_dat(), N, c("hs_stage_0", "hs_stage_1"), A = A,
                N_prev = N_prev)
  # 400 holdovers + 25 flow-capped recruits
  expect_equal(out[1], 425)
})

test_that("hs_stage_1 without flow flags is unchanged (no-op branch)", {
  N <- c(5000, 100, 20)
  # no stage-0 flags: hs_stage_1 total cap is the only stage-1 constraint
  expect_equal(run_dd(toy_dat(K1 = 3000), N, c("hs_stage_1"))[1], 3000)
  expect_equal(run_dd(toy_dat(K1 = 1e6), N, c("hs_stage_1"))[1], 5000)
})
