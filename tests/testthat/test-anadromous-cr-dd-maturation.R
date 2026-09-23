# Anadromous CR-mode density dependence must apply to maturation transitions.
#
# The density-multiplier matrix previously carried each stage's s_i symbol
# only on the stay-immature sub-diagonal; the maturation branch of the SAME
# cohort (Pb_i -> B_(i+1), element s_i*mat*smig) kept a multiplier of 1. The
# maturing portion of every cohort escaped compensation-ratio density
# dependence, and the terminal pre-breeder stage - whose only outflow is
# maturation - had a completely dead cr. Fixed 2026-09-22.
#
# Guarantee verified at fix time: K-mode runs (bh_dd_stages) and CR-mode runs
# with all cr = 1 are bit-identical to pre-fix outputs (D is all ones there).

test_that("D-matrix carries survival symbols on maturation transitions", {
  lc <- read.csv(system.file(
    "extdata/simple_test/test_anadromous/chinook_life_cycle.csv",
    package = "CEMPRA"))
  lc <- pop_model_dat_clean(lc)
  setup <- pop_model_setup(life_cycles = lc)

  sn <- setup$life_histories$stage_names
  Nst <- length(sn)
  Dsym <- matrix(as.character(setup$density_stage_symbolic), Nst, Nst, byrow = TRUE)

  # spawning ages 3/4/5: maturation cells must carry the Pb stage's own s{i}
  for (i in 2:4) {
    b_row <- grep(paste0("_B_", i + 1, "$"), sn)
    expect_equal(Dsym[b_row, i], paste0("s", i),
                 label = paste0("D[", sn[b_row], ", ", sn[i], "]"))
  }

  # stay-immature and maturation branches of one cohort share one symbol
  expect_equal(Dsym[grep("_B_4$", sn), 3], Dsym[4, 3])

  # spawner-class columns still carry no compensation (terminal stages)
  b_cols <- grep("_B_", sn)
  expect_true(all(!grepl("^s[0-9]", Dsym[, b_cols])))
})


test_that("terminal pre-breeder cr is no longer dead", {
  lc <- read.csv(system.file(
    "extdata/simple_test/test_anadromous/chinook_life_cycle.csv",
    package = "CEMPRA"))
  lc <- pop_model_dat_clean(lc)

  run_ch <- function(lcx) {
    s <- pop_model_setup(life_cycles = lcx)
    m <- pop_model_matrix_elements(s)
    set.seed(11)
    Projection_DD(M.mx = m$life_stages_symbolic,
                  D.mx = m$density_stage_symbolic,
                  H.mx = NULL, dat = m$life_histories,
                  K = 5000, Nyears = 150, p.cat = 0, CE_df = NULL,
                  anadromous = TRUE)
  }

  r_base <- run_ch(lc)
  lc2 <- lc
  lc2$Value[lc2$Name == "cr_4"] <- 5
  r_cr4 <- run_ch(lc2)

  # pre-fix these were bit-identical (cr_4 had zero effect)
  expect_false(identical(r_base$pop$N, r_cr4$pop$N))

  # equilibrium spawners stay in a sane band around the intended K = 5000
  sp <- r_base$pop$N[75:151]
  expect_true(median(sp) > 3500 & median(sp) < 6500)
})


test_that("K-mode and cr=1 anadromous runs are unchanged by the maturation fix", {
  # Reference values captured from the PRE-FIX code (2026-09-22): these runs
  # have D = all ones (cr = 1 everywhere), so the fix must not move them.
  lc <- read.csv(system.file("extdata/sockeye/life_cycles_sockeye.csv",
                             package = "CEMPRA"))
  setup <- pop_model_setup(life_cycles = lc)
  m <- pop_model_matrix_elements(setup)

  # K mode: bh_spawners constraint
  set.seed(22)
  r_k <- Projection_DD(M.mx = m$life_stages_symbolic,
                       D.mx = m$density_stage_symbolic,
                       H.mx = NULL, dat = m$life_histories,
                       K = 5000, Nyears = 200, p.cat = 0, CE_df = NULL,
                       anadromous = TRUE, bh_dd_stages = c("bh_spawners"),
                       stage_k_override = rep(NA, m$life_histories$Nstage + 1))
  expect_equal(r_k$pop$N[c(2, 101, 201)],
               c(4752.303290, 0.234592, 0.000379), tolerance = 1e-6)

  # CR mode with all cr = 1
  set.seed(33)
  r_c <- Projection_DD(M.mx = m$life_stages_symbolic,
                       D.mx = m$density_stage_symbolic,
                       H.mx = NULL, dat = m$life_histories,
                       K = 5000, Nyears = 200, p.cat = 0, CE_df = NULL,
                       anadromous = TRUE)
  expect_equal(r_c$pop$N[c(2, 101, 201)],
               c(4798.644620, 0.070627, 0.000319), tolerance = 1e-6)
})
