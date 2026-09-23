# A stressor may target each vital rate at most once. Different life_stage
# tokens can alias the same underlying rate (e.g. "s1", "stage_1" and
# "surv_1" all target stage-1 survival); before this check, listing two
# aliases silently applied the effect twice (score squared). Duplicate
# targeting within one stressor - via aliases, repeated tokens, or
# duplicate rows - is now a hard error. Two distinctly named stressors on
# the same endpoint remain legal and multiply.

# --- local helpers --------------------------------------------------------
dupt_rates <- function(n_len = 6) {
  list(
    ft = replicate(n_len, c(eps = 100), simplify = FALSE),
    st = replicate(n_len, c(sE = 0.9, s0 = 0.5, s1 = 0.6, s2 = 0.7, s3 = 0.8), simplify = FALSE),
    ut = NULL, smigt = NULL,
    K_list = replicate(n_len, c(K1 = 1000, K2 = 500, K3 = 200), simplify = FALSE),
    Ke_list = replicate(n_len, 50000, simplify = FALSE),
    K0_list = replicate(n_len, 10000, simplify = FALSE),
    Kspawners_list = replicate(n_len, 0, simplify = FALSE)
  )
}

dupt_rates_anad <- function(n_len = 6) {
  list(
    ft = replicate(n_len, c(eps4 = 4000, eps5 = 4000), simplify = FALSE),
    st = replicate(n_len, c(sE = 0.9, s0 = 0.5, s1 = 0.6, s2 = 0.7, s3 = 0.8), simplify = FALSE),
    ut = replicate(n_len, c(u4 = 0.9, u5 = 0.9), simplify = FALSE),
    smigt = replicate(n_len, c(smig4 = 0.95, smig5 = 0.95), simplify = FALSE),
    K_list = replicate(n_len, c(stage_Pb_1 = 1000, stage_Pb_2 = 800,
                                stage_B_4 = 200, stage_B_5 = 100), simplify = FALSE),
    Ke_list = replicate(n_len, 50000, simplify = FALSE),
    K0_list = replicate(n_len, 10000, simplify = FALSE),
    Kspawners_list = replicate(n_len, 5000, simplify = FALSE)
  )
}

dupt_ce <- function(life_stage, parameter, sys.cap = 0.5,
                    stressor = "test_stressor") {
  data.frame(
    life_stage = life_stage, parameter = parameter, sys.cap = sys.cap,
    Stressor = stressor, simulation = 1, HUC = 1, stringsAsFactors = FALSE
  )
}

dupt_err <- "same vital rate more than once"


test_that("comma list with two aliases of one rate errors: 's1, stage_1'", {
  expect_error(
    pop_model_ce_apply_year(rates = dupt_rates(),
                            CE_df = dupt_ce("s1, stage_1", "survival"),
                            anadromous = FALSE, mat_stage = c(s3 = 1)),
    regexp = dupt_err
  )
})


test_that("separate rows with aliases of one rate error: 's1' + 'surv_1'", {
  ce <- rbind(dupt_ce("s1", "survival", 0.5),
              dupt_ce("surv_1", "survival", 0.7))
  expect_error(
    pop_model_ce_apply_year(rates = dupt_rates(), CE_df = ce,
                            anadromous = FALSE, mat_stage = c(s3 = 1)),
    regexp = dupt_err
  )
})


test_that("capacity aliases of one stage error: 'k2, stage_2'", {
  expect_error(
    pop_model_ce_apply_year(rates = dupt_rates(),
                            CE_df = dupt_ce("k2, stage_2", "capacity"),
                            anadromous = FALSE, mat_stage = c(s3 = 1)),
    regexp = dupt_err
  )
})


test_that("fecundity aliases of one age error: 'eps3, stage_b_3'", {
  expect_error(
    pop_model_ce_apply_year(rates = dupt_rates(),
                            CE_df = dupt_ce("eps3, stage_b_3", "fecundity"),
                            anadromous = FALSE, mat_stage = c(s3 = 1)),
    regexp = dupt_err
  )
})


test_that("anadromous 'adult, u' errors (adult aliases prespawn survival)", {
  expect_error(
    pop_model_ce_apply_year(rates = dupt_rates_anad(),
                            CE_df = dupt_ce("adult, u", "survival"),
                            anadromous = TRUE, mat_stage = NULL),
    regexp = dupt_err
  )
})


test_that("the same token repeated in one list errors: 'u4, u_4'", {
  expect_error(
    pop_model_ce_apply_year(rates = dupt_rates_anad(),
                            CE_df = dupt_ce("u4, u_4", "survival"),
                            anadromous = TRUE, mat_stage = NULL),
    regexp = dupt_err
  )
})


test_that("two distinctly named stressors on the same rate remain legal", {
  ce <- rbind(dupt_ce("s1", "survival", 0.3, stressor = "sediment"),
              dupt_ce("stage_1", "survival", 0.4, stressor = "temperature"))
  res <- pop_model_ce_apply_year(rates = dupt_rates(), CE_df = ce,
                                 anadromous = FALSE, mat_stage = c(s3 = 1))
  s1 <- vapply(res$st, function(x) unname(x["s1"]), numeric(1))
  expect_equal(s1, rep(0.6 * 0.3 * 0.4, 6))
})


test_that("broad + specific tokens ('surv' + 's1') are NOT flagged", {
  # "surv" targets ALL stage survivals; "s1" targets stage 1 only. They
  # overlap on stage 1 but are distinct targets - out of scope for the
  # duplicate check (intentional layered effects are possible).
  ce <- rbind(dupt_ce("surv", "survival", 0.5),
              dupt_ce("s1", "survival", 0.5))
  res <- pop_model_ce_apply_year(rates = dupt_rates(), CE_df = ce,
                                 anadromous = FALSE, mat_stage = c(s3 = 1))
  s1 <- vapply(res$st, function(x) unname(x["s1"]), numeric(1))
  s2 <- vapply(res$st, function(x) unname(x["s2"]), numeric(1))
  expect_equal(s1, rep(0.6 * 0.5 * 0.5, 6))   # both applied to s1
  expect_equal(s2, rep(0.7 * 0.5, 6))         # only the broad one on s2
})


test_that("per-year rows for one stressor (supported pipeline shape) pass", {
  ce <- dupt_ce("s1", "survival", sys.cap = rep(0.5, 5))
  ce$Year <- 1:5
  res <- pop_model_ce_apply_year(rates = dupt_rates(), CE_df = ce,
                                 anadromous = FALSE, mat_stage = c(s3 = 1))
  s1 <- vapply(res$st, function(x) unname(x["s1"]), numeric(1))
  expect_equal(s1, rep(0.3, 6))
})
