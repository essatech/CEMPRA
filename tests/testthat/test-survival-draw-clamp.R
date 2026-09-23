# Stochastic survival draws must never exceed 100% (issue 1.10, fixed
# 2026-09-22).
#
# s_rand() draws instantaneous mortality from a stretched beta whose
# support minimum goes NEGATIVE once cv exceeds ~0.21; a negative
# mortality draw back-transforms to survival > 1 (at cv = 1.0 roughly 8%
# of draws exceeded 1, max ~6). Fixed: mortality draws are floored at 0,
# capping survival at exactly 100%. The floor is mathematically a no-op
# for cv <= ~0.21 (the support is entirely positive), so all shipped
# profiles (M.cv <= 0.205) are bit-identical - enforced by the seeded
# anchors throughout the suite. The same fix covers the anadromous
# prespawn (u) and migration (smig) draws, which use the same sampler.

test_that("survival draws are capped at 1 for high cv", {
  mn <- c(sE = 0.1, s0 = 0.3, s1 = 0.3, s2 = 0.3, s3 = 0.9, s4 = 0.9)
  for (cv in c(0.5, 1.0)) {
    set.seed(1)
    d <- replicate(4000, s_rand(mn, cv, 0.1))
    expect_true(all(d <= 1), label = paste0("all draws <= 1 at cv = ", cv))
    # the clamp genuinely engages at these cvs (point mass at 1 exists)
    expect_gt(mean(d == 1), 0)
  }
})

test_that("draws below the cv threshold are untouched (no point mass at 1)", {
  mn <- c(sE = 0.1, s0 = 0.3, s1 = 0.3, s2 = 0.3, s3 = 0.9, s4 = 0.9)
  set.seed(1)
  d <- replicate(4000, s_rand(mn, 0.205, 0.1))
  expect_true(all(d < 1))
  expect_equal(mean(d == 1), 0)
})

test_that("pipeline survival draws never exceed 1 at high M.cv", {
  lc <- read.csv(system.file("extdata", "life_cycles.csv", package = "CEMPRA"))
  lc$Value[lc$Name == "M.cv"] <- 0.8
  s <- pop_model_setup(life_cycles = lc)
  expect_message(m <- pop_model_matrix_elements(s), "truncated at zero")
  set.seed(9)
  r <- Projection_DD(M.mx = m$life_stages_symbolic,
                     D.mx = m$density_stage_symbolic,
                     H.mx = NULL, dat = m$life_histories,
                     K = m$life_histories$Ka,
                     Nyears = 100, p.cat = 0, CE_df = NULL)
  # pre-fix this run carried impossible survival values up to ~3.1
  expect_true(all(r$vars$st <= 1))
})
