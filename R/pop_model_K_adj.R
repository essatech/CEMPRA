#' K_adj
#'
#' @description Define K_adj value. DEPRECATED legacy pathway.
#'
#' @details DEPRECATED: this is a legacy stochastic capacity calibration
#' retained for backwards compatibility with older scripts. It is never
#' called by PopulationModel_Run or the Shiny app, is only meaningful in
#' compensation-ratio mode, and is deliberately not exercised by the test
#' suite (the replicate simulations are slow). Repaired 2026-09-15
#' (previously received NULL projection matrices from Projection_DD and
#' crashed).
#'
#' Runs replicate stochastic simulations, measures the realized long-run
#' mean adult abundance, and returns K.adj = K / mean so callers can
#' inflate the working capacity (stochastic variability drags the long-run
#' mean below the deterministic K). A 51-year burn-in is discarded from
#' each replicate, so Nyears must exceed 51.
#'
#' @param replicates Number of replicates
#' @param dat Life history data
#' @param mx A projection matrix expression
#' @param dx A matrix of density-dependence effect
#' @param Nyears Years to run simulation (must be greater than 51)
#' @param H.mx A harm projection matrix
#' @param K Adult carrying capacity being calibrated. Defaults to dat$Ka
#' for backwards compatibility; Projection_DD passes its own K argument so
#' the calibration measures the same model it is calibrating.
#'
#' @importFrom rlang .data
#'
#' @returns A K_adj object to make adjustments
#'
#' @export
pop_model_K_adj <- function(replicates = 100,
                            dat,
                            mx,
                            dx,
                            Nyears = 250,
                            H.mx = NULL,
                            K = NULL
                            ) {

  # DEPRECATED legacy pathway - see Projection_DD(K_adj = TRUE) notes.

  # Backwards-compatible fallback to the life cycle file's adult K
  if (is.null(K)) {
    K <- dat$Ka
  }
  K <- as.numeric(K)
  if (length(K) == 0 || is.na(K)) {
    stop("pop_model_K_adj requires a K value (or dat$Ka) to calibrate against...")
  }

  # A 51-year burn-in is discarded below; shorter runs would silently
  # produce NaN adjustments
  if (Nyears <= 51) {
    stop("pop_model_K_adj requires Nyears > 51 (51-year burn-in per replicate)...")
  }

  pop_data <- list()

  for(i in 1:replicates) {

    res <- Projection_DD(M.mx = mx,
                         D.mx = dx,
                         H.mx = H.mx,
                         dat = dat,
                         Nyears = Nyears,
                         K = K,
                         p.cat = 0,
                         CE_df = NULL,
                         K_adj = FALSE)

    out <- res$pop[-(1:51),]

    pop_data[[i]] <- out

  }

  # mean pop size (adult density) in each replicate
  meanK <- sapply(pop_data, function(x) {
    mean((x$N))
  })
  mean(meanK)
  round(K / mean(meanK), 3) # K.adj

  # Variance (inter-annual) in adult density in each replicate
  varK <- sapply(pop_data, function(x) {
    stats::var((x$N))
  })
  mean(varK)
  sqrt(mean(varK)) / mean(meanK) # CV - target 15% (Paul et al. 2003)
  return(list(
    "K.adj" = round(K / mean(meanK), 3),
    "CV" = sqrt(mean(varK)) / mean(meanK)
  ))
}
