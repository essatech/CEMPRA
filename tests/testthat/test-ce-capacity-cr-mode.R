# CE capacity stressors must bite in compensation-ratio (CR) mode.
# Regression test for the 2026-09-12 fix: capacity effects are written into
# K_list/Ke_list/K0_list by pop_model_ce_apply_year(), and Projection_DD now
# refreshes new_dat$K/Ke/K0 from those lists at the top of EVERY year loop
# (unconditionally), so d.vec.f() sees them without any bh_dd_stages
# configured. Before the fix this was a silent no-op in CR mode.

test_that("capacity CE reduces the population in compensation-ratio mode", {
  set.seed(42) # deterministic stochastic outcomes, independent of test file order

  filename_lc <- system.file("extdata", "life_cycles.csv", package = "CEMPRA")
  lc <- read.csv(filename_lc, stringsAsFactors = FALSE)
  me <- pop_model_matrix_elements(pop_model_setup(lc))
  lh <- me$life_histories

  run_ce <- function(ce_df) {
    set.seed(7)
    Projection_DD(M.mx = me$life_stages_symbolic,
                  D.mx = me$density_stage_symbolic,
                  dat = lh, K = as.numeric(lh$Ka), Nyears = 100,
                  p.cat = 0, CE_df = ce_df)
  }

  mk_ce <- function(sys_cap, life_stage) {
    data.frame(HUC = 1, Stressor = "cap_stressor", dose = 1,
               sys.cap = sys_cap, life_stage = life_stage,
               parameter = "capacity", Stressor_cat = "cap_stressor")
  }

  baseline <- run_ce(NULL)
  base_mean <- mean(baseline$pop$N[51:101])

  # sys.cap = 1 is the multiplicative identity: identical trajectory
  neutral <- run_ce(mk_ce(1, "adult"))
  expect_equal(neutral$pop$N, baseline$pop$N)

  # Halving stage-1 capacity must substantially reduce the population
  # (no bh_dd_stages set - pure compensation-ratio mode; canonical profile
  # has cr_1 = 2.5 so density dependence is active on that transition)
  k1_cap <- run_ce(mk_ce(0.5, "stage_1"))
  expect_true(mean(k1_cap$pop$N[51:101]) < 0.9 * base_mean)

  # NUANCE (correct behavior, not a bug): in CR mode a capacity value only
  # acts through cr/(1 + (cr - 1) * N/K), so stages with cr = 1 ignore K
  # entirely. The canonical profile has cr_4 = 1, so an "adult" capacity
  # stressor is inert there...
  adult_cap <- run_ce(mk_ce(0.5, "adult"))
  expect_equal(adult_cap$pop$N, baseline$pop$N)

  # ...but becomes active once the adult stage has a compensation ratio > 1
  lc2 <- lc
  lc2$Value[lc2$Name == "cr_4"] <- 2
  me2 <- pop_model_matrix_elements(pop_model_setup(lc2))
  lh2 <- me2$life_histories
  run_ce2 <- function(ce_df) {
    set.seed(7)
    Projection_DD(M.mx = me2$life_stages_symbolic,
                  D.mx = me2$density_stage_symbolic,
                  dat = lh2, K = as.numeric(lh2$Ka), Nyears = 100,
                  p.cat = 0, CE_df = ce_df)
  }
  base2 <- run_ce2(NULL)
  adult2 <- run_ce2(mk_ce(0.5, "adult"))
  expect_true(mean(adult2$pop$N[51:101]) < mean(base2$pop$N[51:101]))
})
