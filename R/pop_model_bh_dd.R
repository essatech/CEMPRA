#' bh_stage_f
#' @description survival/transitions. Classical BH function
#'
#' @keywords internal
bh_stage_f <- function(alpha = NA,
                       k = NA,
                       Nt1 = NA) {
  # Beverton-Holt survival
  # (alpha * Nt1) / (1 + ((alpha/k) * Nt1))
  # recruits <- (S  * p) / (1 + (p / c) * S)
  return(expression(((alpha * Nt1) / (1 + ((alpha/k) * Nt1)))))
}

#' dd.N.bh
#' @description Apply location and stage-specific Beverton-Holt density dependent constraints
#'
#' @keywords internal
dd.N.bh <- function(dat = NA, t = NA, st = NA, N = NA, N_prev = NA, bh_dd_stages = NA) {

  # Exist function if values not set
  if(length(bh_dd_stages) == 0) {
    return(N)
  } else {
    if(all(is.na(bh_dd_stages))) {
      return(N)
    }
  }


  if(length(dat$K) != dat$Nstage) {
    stop("error in dd.N.bh... Nstage does not match K vector")
  }
  if(length(bh_dd_stages) > (dat$Nstage + 1)) {
    stop("error in dd.N.bh... bh_dd_stages can only include dd_hs_0 and bh_stage_X values for each class")
  }


  # Nothing left..
  if(all(N < 1)) {
    return(N)
  }

  # Create a new population vector with revised values
  N_adj <- N

  # ----------------------------------------------
  # Calculate max fry (s0) given egg abundance
  # assume egg to fry (dd), if specified, follows
  # simple hockey-stick type DD

  # Fry survival for this transition
  step_s0 <- st[[t]]["s0"]

  # Projected fry abundance for this transition
  N_stage_0 <- N[[1]] / step_s0

  # Should DD be applied to egg to fry (sE to s0) transition?
  # Set egg to 1 if no DD effect
  if("dd_hs_0" %in% bh_dd_stages) {
    # Truncate surviving fry to max egg capacity (max fry)
    N_stage_E_max <- dat$K0
    # Update and set abundance to egg capacity (if exceeded)
    N_stage_0 <- ifelse(N_stage_0 > N_stage_E_max, N_stage_E_max, N_stage_0)
  }


  # ----------------------------------------------
  # Calculate max stage 1 abundance
  # given fry (s0) abundance using BH-DD
  # (stage 1 abundance) / (fry surv)
  # Capacity for stage 1
  k_stage_1 <- dat$K[[1]]

  # Should BH-DD constraint be applied to
  # stage_0 to stage_1 transition?
  # Note that all of this happens in the same year sE*s0*Fec...

  if("bh_stage_1" %in% bh_dd_stages) {
    # Max stage 1 allowed given BH equation
    N_stage_1_BH <- eval(
      bh_stage_f(),
      list(
        alpha = as.numeric(step_s0),
        k = as.numeric(k_stage_1),
        Nt1 = as.numeric(N_stage_0)
      )
    )
    N_stage_1 <- N_stage_1_BH
  } else {
    # Do not apply BH-DD constraint
    # but recalculate with fry adjusted for
    # possible egg survival constraint
    N_stage_1 <- N_stage_0 * step_s0
  }

  # Apply updates to population vector
  # only update the first stage
  N_adj[[1]] <- N_stage_1


  # -------------------------------------
  # Repeat for other subsequent life stages
  # Gather survival values for subsequent stages
  surv <- st[[t]][paste0("s", 1:dat$Nstage)]
  sNt1 <- N_prev
  sNt2 <- rep(NA, length(sNt1))
  k_stage <- dat$K

  # Skip the first stage
  for (i in 2:length(sNt1))
  {
    if(is.na(sNt1[i])) { next }
    # Max K allowed to advance given DD
    sNt2[i] <-
      eval(
        bh_stage_f(),
        list(
          alpha = as.numeric(surv[i - 1]), # Reference surv for previous stage...
          k = as.numeric(k_stage[i]),      # K is set for target stage
          Nt1 = as.numeric(sNt1[i - 1])    # Reference N for previous stage...
        )
      )
  }

  # Update values according to user-specified BH-DD relationships
  sNt2[sNt2 < 1] <- 0 # cleans up basins with less than 1 individual


  # Check other life stages - set to 1 if no DD effect
  # important: skip stage 1. Start at stage 2.
  for(j in 2:(dat$Nstage)) {
    this_stage <- paste0("bh_stage_", j)
    if(this_stage %in% bh_dd_stages) {
      N_adj[[j]] <- sNt2[[j]]
    }
  }

  # Non-linear stage structured matrix model
  # some staged will have more than 1 year
  # only apply BH curve if projected values
  # are greater than BH limit

  # MJB added Sept 27 2023
  N_adj[1] <- ifelse(is.na(N_adj[1]), 0, N_adj[1])


  # N_adj <- ifelse(N_adj > N, N, N_adj)

  if(any(is.na(N_adj))) { stop("NA values in dd.N.bh") }

  return(N_adj)

}
