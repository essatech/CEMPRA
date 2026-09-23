# Multiple From/To activation windows per stressor hard-error (issue
# 1.11, ruled Option A 2026-09-22).
#
# Two dose rows for one (HUC, Stressor) with different From_Year/To_Year
# windows previously collapsed silently: the per-replicate dedup dropped
# the second window's rows (verified: a window that crashes the
# population by 92% when flagged alone had ZERO effect when supplied as a
# second window), and the dose pooling blended the magnitudes first.
# Now a hard error with a workaround message. The workaround - one
# stressor name per window, both pointing at the same response curve -
# provides genuine multi-window behaviour and is tested below.

nan_fixtures <- function() {
  lc <- read.csv(system.file(
    "extdata/nanaimo/species_profiles/nanaimo_comp_ocean_summer.csv",
    package = "CEMPRA"))
  for (nm in c("eps_sd", "M.cv", "egg_rho", "M.rho")) {
    lc$Value[lc$Name == nm] <- 0    # deterministic
  }
  dose <- StressorMagnitudeWorkbook(filename = system.file(
    "extdata/nanaimo/stressor_magnitude_nanaimo.xlsx", package = "CEMPRA"))
  sr <- StressorResponseWorkbook(filename = system.file(
    "extdata/nanaimo/stressor_response_nanaimo.xlsx", package = "CEMPRA"))
  hab <- read.csv(system.file(
    "extdata/nanaimo/habitat_capacities_nanaimo.csv", package = "CEMPRA"),
    stringsAsFactors = FALSE)
  dose$SD <- 0
  list(lc = lc, dose = dose, sr = sr, hab = hab)
}

run_pm <- function(f, d, sr = f$sr) {
  set.seed(5)
  out <- suppressWarnings(PopulationModel_Run(
    dose = d, sr_wb_dat = sr, life_cycle_params = f$lc,
    HUC_ID = 1, n_years = 30, MC_sims = 1, stressors = NA,
    habitat_dd_k = f$hab))
  out$ce[[1]]$pop$N
}

base_row <- function(f, sname) {
  f$dose[f$dose$HUC_ID == 1 & f$dose$Stressor == sname, ][1, ]
}

test_that("two windows for one stressor is a hard error", {
  f <- nan_fixtures()
  sname <- f$dose$Stressor[f$dose$HUC_ID == 1][1]
  r <- base_row(f, sname)
  r1 <- r; r1$From_Year <- 1;  r1$To_Year <- 5
  r2 <- r; r2$From_Year <- 20; r2$To_Year <- 25
  expect_error(run_pm(f, rbind(r1, r2)),
               "Multiple windows per stressor are not supported")

  # differing windows even when one row is unwindowed (NA) also error
  r3 <- r; r3$From_Year <- NA; r3$To_Year <- NA
  expect_error(run_pm(f, rbind(r1, r3)),
               "Multiple windows per stressor are not supported")
})

test_that("identical windows and single windows do not error", {
  f <- nan_fixtures()
  sname <- f$dose$Stressor[f$dose$HUC_ID == 1][1]
  r <- base_row(f, sname)
  r1 <- r; r1$From_Year <- 1; r1$To_Year <- 5

  # single windowed row runs fine
  expect_silent(suppressWarnings(suppressMessages(n1 <- run_pm(f, r1))))
  expect_length(n1, 31)

  # exact-duplicate windows keep today's (pooling) behaviour - no error
  expect_silent(suppressWarnings(suppressMessages(n2 <- run_pm(f, rbind(r1, r1)))))
  expect_length(n2, 31)
})

test_that("distinct-name workaround delivers real multi-window behaviour", {
  f <- nan_fixtures()
  sname <- f$dose$Stressor[f$dose$HUC_ID == 1][1]
  # harmful dose so the second window's effect is unmistakable
  crv <- f$sr$sr_dat[[sname]]
  bad_dose <- crv[[1]][which.min(crv$mean_system_capacity)]

  # clone the stressor-response entry under a second name
  sr2 <- f$sr
  main2 <- sr2$main_sheet[sr2$main_sheet$Stressors == sname, ]
  main2$Stressors <- paste0(sname, "_late")
  sr2$main_sheet <- rbind(sr2$main_sheet, main2)
  sr2$stressor_names <- c(sr2$stressor_names, paste0(sname, "_late"))
  sr2$sr_dat[[paste0(sname, "_late")]] <- sr2$sr_dat[[sname]]

  r <- base_row(f, sname)
  r$Mean <- bad_dose
  r1 <- r; r1$From_Year <- 1;  r1$To_Year <- 5
  r2 <- r; r2$Stressor <- paste0(sname, "_late")
  r2$From_Year <- 20; r2$To_Year <- 25

  n_both <- run_pm(f, rbind(r1, r2), sr = sr2)
  n_first <- run_pm(f, r1, sr = sr2)

  # the late window must now genuinely bite (previously inert): the
  # two-window run diverges from the first-window-only run in years 20+
  expect_false(isTRUE(all.equal(n_both[21:31], n_first[21:31])))
  expect_lt(min(n_both[22:31]), min(n_first[22:31]))
})
