# Tests for the 2026-09-23 Joe Model / interaction matrix batch:
# - `stressors` argument scopes both regular stressors and MInt matrices
#   (matrix-only runs allowed; deselecting a matrix works; matrix + one
#   main effect no longer crashes)
# - additive multi-component dose rows restored (exact duplicates deduped)
# - blank Low/Up limits with SD > 0 draw unbounded instead of erasing the
#   stressor (CE = 1.0 bug)
# - interaction matrix axis names trimmed, adult filter case-insensitive,
#   descending axes sorted, NA doses tolerated, duplicate dose rows deduped
# - tbeta_rnd degenerate branches clamp to the curve limits
# - a workbook with MInt sheets cannot break the population model

fr <- system.file("extdata/matrix_test/stressor_magnitude_matrix.xlsx", package = "CEMPRA")
fs <- system.file("extdata/matrix_test/stressor_response_matrix.xlsx", package = "CEMPRA")

dose_fix <- StressorMagnitudeWorkbook(filename = fr)
sr_fix <- suppressWarnings(StressorResponseWorkbook(filename = fs))
# Fixture layout: stressors A..E; MInt_AB (Main_Effect = Included),
# MInt_DE (Main_Effect = Excluded)

run_joe <- function(...) {
  set.seed(42)
  suppressMessages(JoeModel_Run(dose = dose_fix, sr_wb_dat = sr_fix,
                                MC_sims = 3, ...))
}

test_that("default run applies all stressors and matrices (legacy behavior)", {
  jm <- run_joe()
  got <- sort(unique(as.character(jm$sc.dose.df$Stressor)))
  # Included MInt_AB absorbs A and B; Excluded MInt_DE keeps D and E
  expect_equal(got, sort(c("C", "D", "E", "MInt_AB", "MInt_DE")))
  expect_true(all(jm$ce.df$CE >= 0 & jm$ce.df$CE <= 1))
})

test_that("deselecting matrices excludes them (mains-only selection)", {
  jm <- run_joe(stressors = c("A", "B", "C", "D", "E"))
  got <- sort(unique(as.character(jm$sc.dose.df$Stressor)))
  expect_equal(got, c("A", "B", "C", "D", "E"))
})

test_that("matrix-only selection runs just the matrix", {
  jm <- run_joe(stressors = "MInt_AB")
  got <- unique(as.character(jm$sc.dose.df$Stressor))
  expect_equal(got, "MInt_AB")
  expect_true(all(is.finite(jm$ce.df$CE)))
})

test_that("two-matrix-only selection runs both matrices and nothing else", {
  jm <- run_joe(stressors = c("MInt_AB", "MInt_DE"))
  got <- sort(unique(as.character(jm$sc.dose.df$Stressor)))
  expect_equal(got, c("MInt_AB", "MInt_DE"))
})

test_that("matrix plus one main effect no longer crashes", {
  # Included matrix absorbs its mains whether or not selected
  jm <- run_joe(stressors = c("A", "MInt_AB"))
  expect_equal(unique(as.character(jm$sc.dose.df$Stressor)), "MInt_AB")

  # Excluded matrix keeps the SELECTED main standalone; the unselected
  # axis helper (E) is excluded from the CE
  jm2 <- run_joe(stressors = c("D", "MInt_DE"))
  got2 <- sort(unique(as.character(jm2$sc.dose.df$Stressor)))
  expect_equal(got2, c("D", "MInt_DE"))
})

test_that("matrix plus both mains (Excluded) keeps all three", {
  jm <- run_joe(stressors = c("D", "E", "MInt_DE"))
  got <- sort(unique(as.character(jm$sc.dose.df$Stressor)))
  expect_equal(got, c("D", "E", "MInt_DE"))
})

test_that("unknown matrix names warn and are ignored", {
  expect_warning(
    jm <- suppressMessages(JoeModel_Run(dose = dose_fix, sr_wb_dat = sr_fix,
                                        MC_sims = 2,
                                        stressors = c("A", "MInt_ZZZ"))),
    "Unknown interaction matrix"
  )
  expect_equal(unique(as.character(jm$sc.dose.df$Stressor)), "A")
})

# ---------------------------------------------------------------------
# Additive dose components

make_dose <- function(means, stressor = "A", huc = 1) {
  data.frame(
    HUC_ID = huc,
    NAME = "Test",
    Stressor = stressor,
    Stressor_cat = stressor,
    Mean = means,
    SD = 0,
    Distribution = "normal",
    Low_Limit = 0,
    Up_Limit = 1,
    stringsAsFactors = FALSE
  )
}

test_that("multiple distinct dose rows combine additively (1 - prod(1 - x))", {
  dd <- make_dose(c(0.2, 0.9))
  jm <- suppressMessages(JoeModel_Run(dose = dd, sr_wb_dat = sr_fix,
                                      MC_sims = 2, stressors = "A"))
  expect_equal(unique(jm$sc.dose.df$dose), 1 - (1 - 0.2) * (1 - 0.9))
})

test_that("exact duplicate dose rows are deduped, not double-counted", {
  dd <- make_dose(c(0.2, 0.2))
  jm <- suppressMessages(JoeModel_Run(dose = dd, sr_wb_dat = sr_fix,
                                      MC_sims = 2, stressors = "A"))
  expect_equal(unique(jm$sc.dose.df$dose), 0.2)
})

# ---------------------------------------------------------------------
# Blank limits with SD > 0

test_that("blank Up/Low limits with SD > 0 draw unbounded (no CE = 1 erasure)", {
  dd <- make_dose(0.5)
  dd$SD <- 0.2
  dd$Low_Limit <- NA
  dd$Up_Limit <- NA
  set.seed(7)
  jm <- suppressMessages(JoeModel_Run(dose = dd, sr_wb_dat = sr_fix,
                                      MC_sims = 50, stressors = "A"))
  # Previously: every draw NA -> stressor dropped -> CE exactly 1
  expect_false(any(is.na(jm$sc.dose.df$sys.cap)))
  expect_true(var(jm$sc.dose.df$dose) > 0) # genuinely stochastic draws
  expect_false(all(jm$ce.df$CE == 1))
})

test_that("magnitude reader coerces limit columns to numeric", {
  expect_true(is.numeric(dose_fix$Low_Limit))
  expect_true(is.numeric(dose_fix$Up_Limit))
})

# ---------------------------------------------------------------------
# interaction_matrix_sys_cap robustness (direct calls)

# Small deterministic matrix builder: SD = 0 so predictions equal the
# interpolated mean surface exactly
make_mint <- function(col_vals, row_vals, Z,
                      cols = "X", rows = "Y",
                      main_effect = "Excluded",
                      life_stages = "adult",
                      name = "MInt_T") {
  blockify <- function(M, label) {
    b <- data.frame(label = row_vals, check.names = FALSE)
    colnames(b)[1] <- label
    for (k in seq_along(col_vals)) {
      b[[as.character(col_vals[k])]] <- M[, k]
    }
    b
  }
  Z0 <- Z * 0
  list(
    Matrix_Name = name, Columns = cols, Rows = rows,
    Main_Effect = main_effect, Life_stages = life_stages,
    Parameters = "capacity", Model = NA,
    mat_msc = blockify(Z, "msc"),
    mat_sd = blockify(Z0, "sd"),
    mat_ll = blockify(Z0, "ll"),
    mat_ul = blockify(Z0 + 100, "ul")
  )
}

make_scdose <- function(x_doses, y_doses, cols = "X", rows = "Y") {
  n <- length(x_doses)
  rbind(
    data.frame(HUC = seq_len(n), Stressor = cols, simulation = 1,
               dose = x_doses, sys.cap = 0.5),
    data.frame(HUC = seq_len(n), Stressor = rows, simulation = 1,
               dose = y_doses, sys.cap = 0.5)
  )
}

# Asymmetric surface: rows = y (0, 10), cols = x (0, 10)
Z_asc <- rbind(c(10, 30),
               c(50, 90))

test_that("corner probes interpolate correctly (orientation baseline)", {
  mint <- make_mint(c(0, 10), c(0, 10), Z_asc)
  scd <- make_scdose(c(0, 10, 0, 10), c(0, 0, 10, 10))
  out <- interaction_matrix_sys_cap(mint, scd, adult_sys_cap = FALSE)
  mat_rows <- out[out$Stressor == "MInt_T", ]
  expect_equal(mat_rows$sys.cap[order(mat_rows$HUC)],
               c(0.10, 0.30, 0.50, 0.90))
})

test_that("descending axis values are sorted and give identical results", {
  # Same surface authored with both axes descending
  Z_desc <- Z_asc[2:1, 2:1]
  mint <- make_mint(c(10, 0), c(10, 0), Z_desc)
  scd <- make_scdose(c(0, 10, 0, 10), c(0, 0, 10, 10))
  out <- interaction_matrix_sys_cap(mint, scd, adult_sys_cap = FALSE)
  mat_rows <- out[out$Stressor == "MInt_T", ]
  expect_equal(mat_rows$sys.cap[order(mat_rows$HUC)],
               c(0.10, 0.30, 0.50, 0.90))
})

test_that("axis names with stray whitespace still match", {
  mint <- make_mint(c(0, 10), c(0, 10), Z_asc, cols = " X ", rows = "Y ")
  scd <- make_scdose(5, 5)
  out <- interaction_matrix_sys_cap(mint, scd, adult_sys_cap = FALSE)
  expect_true("MInt_T" %in% out$Stressor)
})

test_that("one missing axis stressor skips cleanly instead of crashing", {
  mint <- make_mint(c(0, 10), c(0, 10), Z_asc)
  scd <- make_scdose(5, 5)
  scd <- scd[scd$Stressor != "Y", ] # only X present
  expect_message(
    out <- interaction_matrix_sys_cap(mint, scd, adult_sys_cap = FALSE),
    "skipped"
  )
  expect_false("MInt_T" %in% out$Stressor)
})

test_that("adult filter is case-insensitive", {
  mint <- make_mint(c(0, 10), c(0, 10), Z_asc, life_stages = "Adult")
  scd <- make_scdose(5, 5)
  out <- interaction_matrix_sys_cap(mint, scd, adult_sys_cap = TRUE)
  expect_true("MInt_T" %in% out$Stressor)
})

test_that("NA doses yield NA matrix capacity instead of aborting", {
  mint <- make_mint(c(0, 10), c(0, 10), Z_asc)
  scd <- make_scdose(c(5, NA), c(5, 5))
  expect_message(
    out <- interaction_matrix_sys_cap(mint, scd, adult_sys_cap = FALSE),
    "NA dose"
  )
  mat_rows <- out[out$Stressor == "MInt_T", ]
  expect_equal(sum(is.na(mat_rows$sys.cap)), 1)
  expect_equal(sum(!is.na(mat_rows$sys.cap)), 1)
})

test_that("duplicate dose rows are deduped before dcast (no count garbage)", {
  mint <- make_mint(c(0, 10), c(0, 10), Z_asc)
  scd <- make_scdose(0, 0)
  scd <- rbind(scd, scd) # exact duplicates for the same HUC/sim/stressor
  expect_message(
    out <- interaction_matrix_sys_cap(mint, scd, adult_sys_cap = FALSE),
    "duplicate dose rows"
  )
  mat_rows <- out[out$Stressor == "MInt_T", ]
  expect_equal(unique(mat_rows$sys.cap), 0.10)
})

# ---------------------------------------------------------------------
# tbeta_rnd degenerate branches respect limits

test_that("tbeta_rnd clamps deterministic branches to the limits", {
  # SD = 0 branch
  expect_equal(CEMPRA:::tbeta_rnd(0.9, 0, low.limit = 0, up.limit = 0.5), 0.5)
  expect_equal(CEMPRA:::tbeta_rnd(0.1, 0, low.limit = 0.3, up.limit = 1), 0.3)
  # NA sd branch
  expect_equal(CEMPRA:::tbeta_rnd(0.9, NA, low.limit = 0, up.limit = 0.5), 0.5)
  # In-range mean untouched
  expect_equal(CEMPRA:::tbeta_rnd(0.4, 0, low.limit = 0, up.limit = 1), 0.4)
})

# ---------------------------------------------------------------------
# Optional SD / low.limit / up.limit blocks default to 0 / 0 / 100

write_mint_sheet <- function(extra_blocks = list()) {
  rows <- list(
    c("Matrix Name", "MInt_T", "Life_stages", "adult"),
    c("Columns", "X", "Parameters", "capacity"),
    c("Rows", "Y", "Model", "All"),
    c("Main Effect", "Excluded", NA, NA),
    c(NA, NA, NA, NA),
    c(NA, "Mean System Capacity (%)", "0", "10"),
    c(NA, "0", "10", "30"),
    c(NA, "10", "50", "90")
  )
  for (blk in extra_blocks) {
    rows <- c(rows, list(c(NA, NA, NA, NA)), blk)
  }
  cells <- do.call(rbind, rows)
  df <- as.data.frame(cells, stringsAsFactors = FALSE)
  tmp <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list(MInt_T = df), path = tmp, col_names = FALSE)
  tmp
}

test_that("absent SD/low.limit/up.limit blocks default to 0/0/100", {
  testthat::skip_if_not_installed("writexl")
  tmp <- write_mint_sheet()
  mm <- suppressMessages(
    interaction_matrix_load(mint_names = "MInt_T", filename = tmp)
  )[["MInt_T"]]

  expect_equal(dim(mm$mat_sd), dim(mm$mat_msc))
  expect_equal(dim(mm$mat_ll), dim(mm$mat_msc))
  expect_equal(dim(mm$mat_ul), dim(mm$mat_msc))
  expect_true(all(unlist(mm$mat_sd[, -1]) == 0))
  expect_true(all(unlist(mm$mat_ll[, -1]) == 0))
  expect_true(all(unlist(mm$mat_ul[, -1]) == 100))

  # And the matrix behaves deterministically end to end
  scd <- make_scdose(c(0, 10), c(0, 10))
  out <- interaction_matrix_sys_cap(mm, scd, adult_sys_cap = FALSE)
  mat_rows <- out[out$Stressor == "MInt_T", ]
  expect_equal(mat_rows$sys.cap[order(mat_rows$HUC)], c(0.10, 0.90))
  unlink(tmp)
})

test_that("blank cells inside a present SD block default to 0", {
  testthat::skip_if_not_installed("writexl")
  sd_block <- list(
    c(NA, "SD", "0", "10"),
    c(NA, "0", "5", NA), # one blank SD cell
    c(NA, "10", "0", "0")
  )
  tmp <- write_mint_sheet(extra_blocks = list(sd_block))
  mm <- suppressMessages(
    interaction_matrix_load(mint_names = "MInt_T", filename = tmp)
  )[["MInt_T"]]
  expect_equal(as.numeric(unlist(mm$mat_sd[1, -1])), c(5, 0))
  expect_equal(as.numeric(unlist(mm$mat_sd[2, -1])), c(0, 0))
  # ll / ul still synthesized
  expect_true(all(unlist(mm$mat_ll[, -1]) == 0))
  expect_true(all(unlist(mm$mat_ul[, -1]) == 100))
  unlink(tmp)
})

# ---------------------------------------------------------------------
# Population model is inert to interaction matrices in the workbook

test_that("a workbook with MInt sheets does not break the population model", {
  filename_lc <- system.file("extdata", "life_cycles.csv", package = "CEMPRA")
  life_cycles <- read.csv(filename_lc)
  expect_error(
    pm <- suppressWarnings(suppressMessages(
      PopulationModel_Run(
        dose = dose_fix,
        sr_wb_dat = sr_fix,
        life_cycle_params = life_cycles,
        HUC_ID = 1,
        n_years = 5,
        MC_sims = 1
      )
    )),
    NA
  )
  expect_true(is.list(pm))
})
