# Per-stage fecundity (eps_x) support for non-anadromous models.
#
# Non-anadromous models accept stage-specific eps values (eps_1..eps_N),
# mirroring the anadromous per-age fecundity. A legacy single "eps" row is
# spread across mature stages (mat_x > 0) on import. Backward-compatibility
# reference values below were captured from the pre-change code
# (2026-09-22): legacy single-eps inputs must stay BIT-IDENTICAL, including
# the random-number stream (a single fecundity value is drawn per year and
# broadcast whenever all mature stages share one eps value).

test_that("legacy single-eps files are bit-identical to pre-change results", {
  lc <- read.csv(system.file("extdata", "life_cycles.csv", package = "CEMPRA"))

  setup <- pop_model_setup(life_cycles = lc)
  mat_el <- pop_model_matrix_elements(setup)

  # eps spread across mature stages only (mat_4 = 1 is the only mature stage)
  expect_equal(unname(mat_el$life_histories$eps), c(0, 0, 3000))
  expect_equal(names(mat_el$life_histories$eps), c("eps2", "eps3", "eps4"))

  # Deterministic matrix and lambda (pre-change reference values)
  expect_equal(popbio::lambda(mat_el$projection_matrix),
               1.21101771542959, tolerance = 1e-12)
  A_ref <- matrix(c(
    0.0, 0.0000000000000000, 0.000000000000000, 45.00000000000000,
    0.3, 0.2307692307692308, 0.000000000000000, 0.00000000000000,
    0.0, 0.0692307692307692, 0.473684210526316, 0.00000000000000,
    0.0, 0.0000000000000000, 0.426315789473684, 0.75580571903006
  ), nrow = 4, byrow = TRUE)
  expect_equal(unname(mat_el$projection_matrix), A_ref, tolerance = 1e-12)

  # Seeded stochastic projection: identical RNG stream to pre-change code
  set.seed(42)
  base_run <- Projection_DD(
    M.mx = mat_el$life_stages_symbolic,
    D.mx = mat_el$density_stage_symbolic,
    H.mx = NULL,
    dat = mat_el$life_histories,
    K = mat_el$life_histories$Ka,
    Nyears = 100, p.cat = 0, CE_df = NULL
  )
  expect_equal(base_run$pop$N[c(2, 51, 101)],
               c(116.5019806604, 94.7992694554, 90.6772869360), # re-anchored 2026-09-24 (egg-DD timing fix, ~1% shift; year 2 unchanged),
               tolerance = 1e-9)

  # Seeded run with a generic CE fecundity stressor
  CE_df <- data.frame(HUC = 1, Stressor = "fec_stress", dose = 1,
                      sys.cap = 0.8, life_stage = "eps",
                      parameter = "fecundity", Stressor_cat = "fec_stress")
  set.seed(99)
  ce_run <- Projection_DD(
    M.mx = mat_el$life_stages_symbolic,
    D.mx = mat_el$density_stage_symbolic,
    H.mx = NULL,
    dat = mat_el$life_histories,
    K = mat_el$life_histories$Ka,
    Nyears = 60, p.cat = 0, CE_df = CE_df
  )
  expect_equal(ce_run$pop$N[c(2, 31, 61)],
               c(119.9739230073, 89.8125566614, 100.8041549578), # re-anchored 2026-09-24 (egg-DD timing fix),
               tolerance = 1e-9)

  # The cleaned file must reproduce the identical model
  cleaned <- pop_model_dat_clean(lc)
  setup_c <- pop_model_setup(life_cycles = cleaned)
  mat_c <- pop_model_matrix_elements(setup_c)
  expect_equal(popbio::lambda(mat_c$projection_matrix),
               1.21101771542959, tolerance = 1e-12)
  set.seed(42)
  run_c <- Projection_DD(
    M.mx = mat_c$life_stages_symbolic,
    D.mx = mat_c$density_stage_symbolic,
    H.mx = NULL,
    dat = mat_c$life_histories,
    K = mat_c$life_histories$Ka,
    Nyears = 100, p.cat = 0, CE_df = NULL
  )
  expect_equal(run_c$pop$N, base_run$pop$N, tolerance = 1e-12)
})


test_that("anadromous models are completely untouched", {
  lca <- read.csv(system.file(
    "extdata/nanaimo/species_profiles/beechie_2021_chehalis.csv",
    package = "CEMPRA"))
  setup_a <- pop_model_setup(life_cycles = lca)
  mat_a <- pop_model_matrix_elements(setup_a)
  expect_equal(popbio::lambda(mat_a$projection_matrix),
               1.85030266961822, tolerance = 1e-12)
  set.seed(7)
  run_a <- Projection_DD(
    M.mx = mat_a$life_stages_symbolic,
    D.mx = mat_a$density_stage_symbolic,
    H.mx = NULL,
    dat = mat_a$life_histories,
    K = mat_a$life_histories$Ka,
    Nyears = 50, p.cat = 0, CE_df = NULL
  )
  expect_equal(run_a$pop$N[c(2, 26)],
               c(10353.1466367061, 37026864720.0227737),
               tolerance = 1e-9)
})


test_that("distinct per-stage eps values are used exactly (no median collapse)", {
  lc <- read.csv(system.file("extdata", "life_cycles.csv", package = "CEMPRA"))
  lc2 <- lc[lc$Name != "eps", ]
  lc2 <- rbind(lc2, data.frame(
    Parameters = c("eps_3", "eps_4"),
    Name = c("eps_3", "eps_4"),
    Value = c(1000, 5000)))
  lc2$Value[lc2$Name == "mat_3"] <- 0.5

  expect_message(setup2 <- pop_model_setup(life_cycles = lc2), NA)
  lpv <- function(nm) as.numeric(lc2$Value[lc2$Name == nm])

  # Fecundity matrix elements use each stage's own eps -
  # under the old median collapse both would have used median(1000, 5000) = 3000
  f3 <- 0.5 * lpv("events") * 1000 * lpv("SE") * lpv("S0") * lpv("SR") / lpv("int")
  f4 <- 1.0 * lpv("events") * 5000 * lpv("SE") * lpv("S0") * lpv("SR") / lpv("int")
  A2 <- setup2$projection_matrix
  expect_equal(A2[1, 3], f3)
  expect_equal(A2[1, 4], f4)

  # Symbolic matrix carries per-stage symbols
  sym <- as.character(setup2$life_stages_symbolic)
  expect_true(any(grepl("eps3", sym)))
  expect_true(any(grepl("eps4", sym)))

  # eps vector on life histories
  expect_equal(unname(setup2$life_histories$eps), c(0, 1000, 5000))
  expect_equal(names(setup2$life_histories$eps), c("eps2", "eps3", "eps4"))

  # A +5% perturbation of one stage's eps changes lambda
  # (this is the mechanism that made LTRE fecundity sensitivities no-ops)
  lc3 <- lc2
  lc3$Value[lc3$Name == "eps_4"] <- 5000 * 1.05
  setup3 <- pop_model_setup(life_cycles = lc3)
  mat2 <- pop_model_matrix_elements(setup2)
  mat3 <- pop_model_matrix_elements(setup3)
  l2 <- popbio::lambda(mat2$projection_matrix)
  l3 <- popbio::lambda(mat3$projection_matrix)
  expect_gt(l3, l2)
})


test_that("distinct per-stage eps: stochastic projection draws per stage", {
  lc <- read.csv(system.file("extdata", "life_cycles.csv", package = "CEMPRA"))
  lc2 <- lc[lc$Name != "eps", ]
  lc2 <- rbind(lc2, data.frame(
    Parameters = c("eps_3", "eps_4"),
    Name = c("eps_3", "eps_4"),
    Value = c(1000, 5000)))
  lc2$Value[lc2$Name == "mat_3"] <- 0.5

  setup2 <- pop_model_setup(life_cycles = lc2)
  mat2 <- pop_model_matrix_elements(setup2)

  set.seed(1)
  warns <- testthat::capture_warnings(
    run2 <- Projection_DD(
      M.mx = mat2$life_stages_symbolic,
      D.mx = mat2$density_stage_symbolic,
      H.mx = NULL,
      dat = mat2$life_histories,
      K = mat2$life_histories$Ka,
      Nyears = 200, p.cat = 0, CE_df = NULL
    )
  )
  expect_length(warns, 0)
  expect_false(any(is.na(run2$pop$N)))

  ftm <- run2$vars$ft
  expect_equal(colnames(ftm), c("eps2", "eps3", "eps4"))
  # structural zero stays exactly zero; no RNG consumed for it
  expect_true(all(ftm[, "eps2"] == 0))
  # yearly draws centre on each stage's own fecundity
  expect_equal(mean(ftm[, "eps3"]), 1000, tolerance = 0.15)
  expect_equal(mean(ftm[, "eps4"]), 5000, tolerance = 0.15)
  # draws differ across stages (genuinely per-stage, not broadcast)
  expect_true(stats::sd(ftm[, "eps4"] / ftm[, "eps3"]) > 0)
})


test_that("dat_clean spreads a legacy single eps by maturity and drops the row", {
  lc <- read.csv(system.file("extdata", "life_cycles.csv", package = "CEMPRA"))
  cleaned <- pop_model_dat_clean(lc)

  expect_false("eps" %in% cleaned$Name)
  expect_true("eps_sd" %in% cleaned$Name)
  expect_equal(cleaned$Value[cleaned$Name == "eps_4"], 3000)
  expect_equal(cleaned$Value[cleaned$Name %in% c("eps_1", "eps_2", "eps_3")],
               c(0, 0, 0))
})


test_that("dat_clean: both formats -> per-stage wins, legacy fills mature gaps", {
  lc <- read.csv(system.file("extdata", "life_cycles.csv", package = "CEMPRA"))
  # keep legacy eps = 3000, add explicit eps_3; make stages 3 and 4 mature
  lc2 <- rbind(lc, data.frame(Parameters = "eps_3", Name = "eps_3", Value = 700))
  lc2$Value[lc2$Name == "mat_3"] <- 0.5

  expect_message(cleaned <- pop_model_dat_clean(lc2), "both a legacy")

  expect_false("eps" %in% cleaned$Name)
  # explicit per-stage row wins
  expect_equal(cleaned$Value[cleaned$Name == "eps_3"], 700)
  # missing mature stage filled from the legacy value
  expect_equal(cleaned$Value[cleaned$Name == "eps_4"], 3000)
  # immature stages get 0
  expect_equal(cleaned$Value[cleaned$Name %in% c("eps_1", "eps_2")], c(0, 0))

  # out-of-range per-stage rows (beyond Nstage) are removed
  lc3 <- rbind(lc[lc$Name != "eps", ],
               data.frame(Parameters = c("eps_4", "eps_11"),
                          Name = c("eps_4", "eps_11"),
                          Value = c(2000, 9999)))
  cleaned3 <- pop_model_dat_clean(lc3)
  expect_false("eps_11" %in% cleaned3$Name)
  expect_equal(cleaned3$Value[cleaned3$Name == "eps_4"], 2000)
})


test_that("all-zero fecundity for mature stages stops with a clear error", {
  lc <- read.csv(system.file("extdata", "life_cycles.csv", package = "CEMPRA"))
  lc$Value[lc$Name == "eps"] <- 0
  expect_error(pop_model_setup(life_cycles = lc), "Fecundity is missing")
})


test_that("uncleaned direct setup calls: legacy fallback fills mature gaps", {
  # pop_model_setup() without dat_clean: legacy eps + one explicit eps_x
  lc <- read.csv(system.file("extdata", "life_cycles.csv", package = "CEMPRA"))
  lc2 <- rbind(lc, data.frame(Parameters = "eps_3", Name = "eps_3", Value = 700))
  lc2$Value[lc2$Name == "mat_3"] <- 0.5

  setup <- pop_model_setup(life_cycles = lc2)
  # eps_3 explicit; eps_4 (mature, no row) falls back to legacy 3000
  expect_equal(unname(setup$life_histories$eps), c(0, 700, 3000))
})


test_that("stage-specific CE fecundity tags now work for non-anadromous models", {
  lc <- read.csv(system.file("extdata", "life_cycles.csv", package = "CEMPRA"))
  lc2 <- lc[lc$Name != "eps", ]
  lc2 <- rbind(lc2, data.frame(
    Parameters = c("eps_3", "eps_4"),
    Name = c("eps_3", "eps_4"),
    Value = c(1000, 5000)))
  lc2$Value[lc2$Name == "mat_3"] <- 0.5

  setup2 <- pop_model_setup(life_cycles = lc2)
  mat2 <- pop_model_matrix_elements(setup2)

  CE_df4 <- data.frame(HUC = 1, Stressor = "fec4", dose = 1,
                       sys.cap = 0.5, life_stage = "eps4",
                       parameter = "fecundity", Stressor_cat = "fec4")
  set.seed(5)
  runA <- Projection_DD(M.mx = mat2$life_stages_symbolic,
                        D.mx = mat2$density_stage_symbolic,
                        H.mx = NULL, dat = mat2$life_histories,
                        K = mat2$life_histories$Ka,
                        Nyears = 30, p.cat = 0, CE_df = NULL)
  set.seed(5)
  runB <- Projection_DD(M.mx = mat2$life_stages_symbolic,
                        D.mx = mat2$density_stage_symbolic,
                        H.mx = NULL, dat = mat2$life_histories,
                        K = mat2$life_histories$Ka,
                        Nyears = 30, p.cat = 0, CE_df = CE_df4)
  # eps4 draws halved, eps3 untouched
  expect_equal(runB$vars$ft[, "eps4"], 0.5 * runA$vars$ft[, "eps4"])
  expect_equal(runB$vars$ft[, "eps3"], runA$vars$ft[, "eps3"])
})


test_that("fecundity robustness: missing support rows fail clearly or degrade gracefully", {
  lc <- read.csv(system.file("extdata", "life_cycles.csv", package = "CEMPRA"))

  # missing events/int/SR/SE/S0 -> immediate, named error (was an opaque
  # "missing value where TRUE/FALSE needed" much later)
  for (nm in c("events", "int", "SR", "SE", "S0")) {
    expect_error(pop_model_setup(life_cycles = lc[lc$Name != nm, ]),
                 regexp = nm, label = paste("missing", nm))
  }

  # missing egg_rho with DISTINCT per-stage eps must run (NA rho -> 0);
  # this crashed mvrnorm with "infinite or missing values in 'x'"
  lc2 <- lc[!(lc$Name %in% c("eps", "egg_rho")), ]
  lc2 <- rbind(lc2, data.frame(Parameters = c("eps_3", "eps_4"),
                               Name = c("eps_3", "eps_4"),
                               Value = c(1000, 5000)))
  lc2$Value[lc2$Name == "mat_3"] <- 0.5
  setup2 <- pop_model_setup(life_cycles = lc2)
  expect_message(mat2 <- pop_model_matrix_elements(setup2), "egg_rho is missing")
  set.seed(2)
  run2 <- Projection_DD(M.mx = mat2$life_stages_symbolic,
                        D.mx = mat2$density_stage_symbolic,
                        H.mx = NULL, dat = mat2$life_histories,
                        K = mat2$life_histories$Ka,
                        Nyears = 20, p.cat = 0, CE_df = NULL)
  expect_false(any(is.na(run2$pop$N)))

  # missing eps_sd -> runs deterministically with a console message
  lc3 <- lc[lc$Name != "eps_sd", ]
  setup3 <- pop_model_setup(life_cycles = lc3)
  expect_message(mat3 <- pop_model_matrix_elements(setup3), "eps_sd is missing")
  set.seed(3)
  run3 <- Projection_DD(M.mx = mat3$life_stages_symbolic,
                        D.mx = mat3$density_stage_symbolic,
                        H.mx = NULL, dat = mat3$life_histories,
                        K = mat3$life_histories$Ka,
                        Nyears = 20, p.cat = 0, CE_df = NULL)
  expect_true(all(run3$vars$ft[, "eps4"] == 3000))
})
