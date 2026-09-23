test_that("log-scale stressor-response curves interpolate on a log axis", {

  # Worked example: declining curve in raw units, dose = 50.
  # On a log axis, 50 sits ~69% of the way from 10 to 100, so the
  # system capacity should be ~62%, not ~97% (raw-axis lookup of ln(50)).

  curve <- data.frame(
    value = c(1, 10, 100, 1000),
    mean_system_capacity = c(100, 90, 50, 10),
    sd = 0, lwr = 0, upr = 100
  )

  make_main <- function(scale) data.frame(
    Stressors = "teststr", Stressor_cat = "teststr",
    Interaction = NA, Linked = NA,
    Stress_Scale = scale, Function = "continuous",
    Life_stages = "fry", Parameters = "capacity",
    stringsAsFactors = FALSE
  )

  dose_df <- data.frame(
    HUC_ID = 1, NAME = "x", Stressor = "teststr", Stressor_cat = "teststr",
    Mean = 50, SD = 0, Distribution = "normal",
    Low_Limit = 1, Up_Limit = 1000, stringsAsFactors = FALSE
  )

  run_sc <- function(scale) {
    main <- make_main(scale)
    mrl <- mean_Response(n.stressors = 1,
                         str.list = list(teststr = curve),
                         main = main)
    sc <- SystemCapacity(f.dose.df = dose_df, f.main.df = main[1, ],
                         f.stressor.df = curve, f.mean.resp.list = mrl[[1]],
                         n.sims = 5)
    unique(sc$sys.cap)
  }

  # Curve x-axis and dose both transformed with log(x + 1)
  exp_log <- 0.90 - (log(51) - log(11)) / (log(101) - log(11)) * 0.40
  exp_lin <- 0.90 - (50 - 10) / 90 * 0.40

  expect_equal(run_sc("log"), exp_log, tolerance = 1e-8)

  # Linear scale unchanged by the log-axis handling
  expect_equal(run_sc("linear"), exp_lin, tolerance = 1e-8)

  # Stress_Scale flag is case/whitespace tolerant
  expect_equal(run_sc("Log"), exp_log, tolerance = 1e-8)
  expect_equal(run_sc(" log "), exp_log, tolerance = 1e-8)

  # Missing/NA Stress_Scale falls back to linear instead of erroring
  expect_equal(run_sc(NA), exp_lin, tolerance = 1e-8)

  # Doses must discriminate on curves whose raw x-range starts above ln(x_max)
  # (previously every dose collapsed to the first curve value)
  curve2 <- curve
  curve2$value <- c(10, 50, 100, 1000)
  mrl2 <- mean_Response(1, list(teststr = curve2), make_main("log"))
  d20 <- dose_df; d20$Mean <- 20; d20$Low_Limit <- 10; d20$Up_Limit <- 1000
  d500 <- d20; d500$Mean <- 500
  s20 <- unique(SystemCapacity(d20, make_main("log")[1, ], curve2,
                               mrl2[[1]], n.sims = 3)$sys.cap)
  s500 <- unique(SystemCapacity(d500, make_main("log")[1, ], curve2,
                                mrl2[[1]], n.sims = 3)$sys.cap)
  expect_true(s20 > s500)

  # Log scale with curve x-values <= -1 is invalid (log(x + 1) undefined)
  curve3 <- curve
  curve3$value <- c(-2, 0, 10, 100)
  expect_error(
    mean_Response(1, list(teststr = curve3), make_main("log")),
    "x-values include values <= -1"
  )
})
