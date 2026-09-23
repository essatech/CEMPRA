# Spawner capacity must count maturity-weighted spawners (issue 1.8,
# fixed 2026-09-22, Option A per Matt's ruling).
#
# Non-anadromous stages can be partially mature (0 < mat < 1). The
# bh_spawners/hs_spawners caps previously summed the FULL abundance of
# every stage with mat > 0: with mat = (0.1, 1) and N = (1000, 50) the cap
# saw 1,050 "spawners" instead of the true 150, fired far too early,
# killed ~814 immature fish, and left only ~14 true spawners against a cap
# of 100. Fixed: the cap counts N * mat (the Nb_est convention) and scales
# the spawner stages so the weighted count lands exactly on the cap.
# Anadromous B classes (100% spawners) and full-maturity profiles are
# unchanged (weights all 1 -> identical arithmetic).

toy_dat <- function(mat2 = 0.1, mat3 = 1.0, cap = 100) {
  list(anadromous = FALSE, Nstage = 3, stage_names = c("K1", "K2", "K3"),
       mat = c(mat2 = mat2, mat3 = mat3), K = c(NA, NA, NA), K0 = NA,
       stage_k_spawners = cap,
       Surv_annual = c(sE = 0.1, s0 = 0.3, s1 = 0.3, s2 = 0.5, s3 = 0.9))
}
st_t <- c(sE = 0.1, s0 = 0.3, s1 = 0.3, s2 = 0.5, s3 = 0.9)
run_cap <- function(dat, N, flags) {
  dd.N.bh(dat = dat, t = 1, st = list(st_t, st_t), N = N, N_prev = N,
          bh_dd_stages = flags, A = diag(3))
}

test_that("hs_spawners counts maturity-weighted spawners", {
  N <- c(10, 1000, 50)      # true spawners = 0.1*1000 + 1*50 = 150

  out <- run_cap(toy_dat(), N, "hs_spawners")

  # weighted spawner count lands exactly on the cap
  expect_equal(0.1 * out[2] + out[3], 100)
  # both spawner stages scaled by the same factor f = 100/150
  expect_equal(unname(out[2:3]), c(1000, 50) * (100 / 150))
  # non-spawner stage untouched
  expect_equal(out[1], 10)
  # immature fish mostly preserved (~300 casualties, not ~814)
  expect_gt(0.9 * out[2], 590)
})

test_that("bh_spawners counts maturity-weighted spawners", {
  N <- c(10, 1000, 50)
  out <- run_cap(toy_dat(), N, "bh_spawners")
  # holdover-only case: per-stage K trims land on the same scaled values
  expect_equal(0.1 * out[2] + out[3], 100, tolerance = 1e-8)
  expect_equal(unname(out[2:3]), c(1000, 50) * (100 / 150), tolerance = 1e-8)
})

test_that("caps do not fire when true spawners are under the cap", {
  # old code: saw 1,050 unweighted "spawners" > cap 200 and fired;
  # true spawners are 150 < 200, so nothing should change
  N <- c(10, 1000, 50)
  out <- run_cap(toy_dat(cap = 200), N, "hs_spawners")
  expect_equal(unname(out), N)
})

test_that("full-maturity profiles are unchanged (weighted == unweighted)", {
  N <- c(10, 1000, 50)
  dat <- toy_dat(mat2 = 0, mat3 = 1, cap = 30)
  out <- run_cap(dat, N, "hs_spawners")
  # only the fully-mature stage is capped; others untouched
  expect_equal(unname(out), c(10, 1000, 30))
})

test_that("anadromous spawner caps are unchanged (B classes weight 1)", {
  # seeded sockeye K-mode run (bh_spawners): identical to the pre-change
  # anchors captured for the CR-DD maturation fix
  lc <- read.csv(system.file("extdata/sockeye/life_cycles_sockeye.csv",
                             package = "CEMPRA"))
  s <- pop_model_setup(life_cycles = lc)
  m <- pop_model_matrix_elements(s)
  set.seed(22)
  r <- Projection_DD(M.mx = m$life_stages_symbolic,
                     D.mx = m$density_stage_symbolic,
                     H.mx = NULL, dat = m$life_histories,
                     K = 5000, Nyears = 200, p.cat = 0, CE_df = NULL,
                     anadromous = TRUE, bh_dd_stages = c("bh_spawners"),
                     stage_k_override = rep(NA, m$life_histories$Nstage + 1))
  expect_equal(r$pop$N[c(2, 101, 201)],
               c(4752.303290, 0.234592, 0.000379), tolerance = 1e-6)
})
