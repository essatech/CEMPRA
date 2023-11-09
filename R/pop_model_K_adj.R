#' K_adj
#'
#' @description Define K_adj value.
#'
#' @details Initialize populations and find carrying capacity for each life stage.
#' Carrying capacity is defined at the adult stage and estimated for all other
#' life-stages using the stable-stage distribution (i.e., if the carrying
#' capacity of the adult stage was set to 100 the carrying capacity
#' for a stage below (stage x) is the amount of stage x necessary to result
#' in 100 adults at equilibrium conditions).
#'
#' @param replicates Number of replicates
#' @param dat Life history data
#' @param mx A projection matrix expression
#' @param dx A matrix of density-dependence effect
#' @param Nyears Years to run simulation
#' @param H.mx A harm projection matrix
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
                            H.mx = NULL
                            ) {

  pop_data <- list()

  for(i in 1:replicates) {

    res <- Projection_DD(M.mx = mx,
                         D.mx = dx,
                         H.mx = H.mx,
                         dat = dat,
                         Nyears = Nyears,
                         K = dat$Ka,
                         p.cat = 0,
                         CE_df = NULL,
                         K_adj = FALSE)

    out <- res$pop[-(1:51),]

    pop_data[[i]] <- out

  }


  K <- dat$Ka

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
