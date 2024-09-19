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
  # See Beechie et al., 2022: Ecosphere. 2023 14:e4402.
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

  # Should always be lowercase
  bh_dd_stages <- tolower(bh_dd_stages)


  if(length(dat$K) != dat$Nstage) {
    stop("error in dd.N.bh... Nstage does not match K vector")
  }

  if(length(bh_dd_stages) > (dat$Nstage + 1)) {
    stop("error in dd.N.bh... bh_dd_stages can only include dd_hs_0 and bh_stage_X values for each class")
  }


  # Nothing left virtually extinct...
  if(all(N < 1)) {
    return(N)
  }

  # Create a new population vector with revised values
  # Recall vector is S1,S2,S3,S4...B1,B2,B3,B4... (no eggs no SE; no fry s0)
  N_adj <- N

  # ----------------------------------------------
  # Calculate max fry (s0) given egg abundance
  # assume egg to fry (dd), if specified, follows
  # simple hockey-stick type DD

  # Fry survival for this transition
  step_s0 <- st[[t + 1]]["s0"]

  # Projected fry abundance for this transition
  # Stage_1 abundance / fry survival
  N_stage_0 <- N[[1]] / step_s0

  # Should DD be applied to egg to fry (sE to s0) transition
  # using a hockey-stick type DD?
  # Set egg to 1 if no DD effect
  if(any(c("dd_hs_0", "hs_stage_0") %in% bh_dd_stages)) {
    # Truncate surviving fry to max egg capacity (max fry)
    N_stage_Et_to_0_max <- dat$K0
    # Update and set abundance to egg capacity (if exceeded)
    N_stage_0 <- ifelse(N_stage_0 > N_stage_Et_to_0_max, N_stage_Et_to_0_max, N_stage_0)
  }

  # ------------------------------------------------------------
  # Apply hockey-stick DD to other specific life stages (if set)
  # ------------------------------------------------------------
  if(any(grepl("hs", bh_dd_stages))) {

    # Apply hockey-stick-style DD to other life stages (if set)
    for (i in 1:dat$Nstage) {

      # Determine if DD constraint for specific stage is defined...
      check_2 <- paste0("hs_stage_", i)

      # if neither are present skip to next stage
      if(check_2 %in% bh_dd_stages) {
        # Apply hockey-stick DD to life stage K
        N_current <- N_adj[i] # Recal s1, s2, s3... (no egg or fry)
        N_cap <- dat$K[paste0("K", i)]

        # Apply simple hockey stick DD
        N_current <- ifelse(N_current > N_cap, N_cap, N_current)
        N_adj[i] <- N_current # Update vector
      }

      # Check if target is applied to "stage_pb_X"
      check_3 <- paste0("hs_stage_pb_", i)
      if(check_3 %in% bh_dd_stages) {
        # Apply hockey-stick DD to life stage K
        stage_index <- which(paste0("stage_Pb_", i) == dat$stage_names)[1]
        N_current <- N_adj[stage_index]
        N_cap <- dat$K[paste0("K", i)]
        # Apply simple hockey stick DD
        N_current <- ifelse(N_current > N_cap, N_cap, N_current)
        N_adj[stage_index] <- N_current # Update vector
      }

      # Check if target is applied to "stage_b_X"
      check_4 <- paste0("hs_stage_b_", i)
      if(check_4 %in% bh_dd_stages) {
        # Apply hockey-stick DD to life stage K
        stage_index <- which(paste0("stage_B_", i) == dat$stage_names)[1]
        N_current <- N_adj[stage_index]
        N_cap <- dat$K[paste0("K", i)]
        # Apply simple hockey stick DD
        N_current <- ifelse(N_current > N_cap, N_cap, N_current)
        N_adj[stage_index] <- N_current # Update vector

      }
    }
  }


  # Apply hockey-stick DD to all spawners (if set)
  if("hs_spawners" %in% bh_dd_stages) {
    # Get spawner cap
    spawner_cap <- dat$stage_k_spawners
    # spawner_cap <- 1000
    # Get current spawners
    stage_index <- which(grepl("stage_B_", dat$stage_names))
    current_spawners <- N_adj[stage_index]
    current_spawners_s <- sum(current_spawners)

    if(current_spawners_s > spawner_cap) {
      # Apply simple hockey stick DD
      # Reduce by fraction
      N_adj[stage_index] <- current_spawners * (spawner_cap / current_spawners_s)
    }
  }


  # ==============================================
  # Adjust capacities with Beverton-Holt style DD
  # ==============================================


  # ----------------------------------------------
  # Calculate max egg to fry with Beverton-Holt
  if (any(c("bh_stage_0") %in% bh_dd_stages)) {
    # Max fry from egg to fry from BH pathway
    alpha_eggs <- st[[t + 1]]["sE"]  # Alpha Egg Survival
    n_eggs <- N_stage_0 / alpha_eggs # Current Egg Count
    N_stage_Et_to_0_max <- dat$K0    # Max possible fry
    N_stage_0_BH <- eval(bh_stage_f(),
                         list(
                           alpha = alpha_eggs,
                           k = N_stage_Et_to_0_max,
                           Nt1 = n_eggs
                         ))
    # Overwrite above value for fry after BH DD
    N_stage_0 <- N_stage_0_BH
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
  # Naming convention should follow: dd_hs_sE_to_s0, dd_bh_stage_0_to_1

  if(any(c("bh_stage_1") %in% bh_dd_stages)) {
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
    # but recalculate stage_1 with fry adjusted for
    # possible egg survival constraint
    N_stage_1 <- N_stage_0 * step_s0
  }

  # Apply updates to population vector
  # only update the first stage
  N_adj[[1]] <- N_stage_1


  # ----------------------------------------------
  # Repeat BH for other subsequent life stages
  # ----------------------------------------------
  # Gather survival values for subsequent stages
  surv <- st[[t + 1]][paste0("s", 1:dat$Nstage)]
  sNt1 <- N_prev
  sNt2 <- rep(NA, length(sNt1))
  k_stage <- dat$K


  # Special bh_spawners (all classes)
  # If bh_spawners is present adjust K for spawners in proportion to
  # relative abundance in each spawning class
  if("bh_spawners" %in% bh_dd_stages) {

    # Get spawner cap
    spawner_cap <- dat$stage_k_spawners
    # spawner_cap <- 100
    # Get current spawners
    stage_index <- which(grepl("stage_B_", dat$stage_names))
    current_spawners <- N_adj[stage_index]
    current_spawners_s <- sum(current_spawners)

    if(current_spawners_s > spawner_cap) {
      # Apply simple hockey stick DD
      # Reduce KStage by relative fraction
      k_stage[stage_index] <- current_spawners * (spawner_cap / current_spawners_s)

      # Append BH classes to DD vector
      add_these <- paste0("bh_", tolower(dat$stage_names[which(grepl("stage_B_", dat$stage_names))]))
      bh_dd_stages <- c(bh_dd_stages, add_these)
      bh_dd_stages <- unique(bh_dd_stages)

    }
  } # end of special bh_spawners (all classes)





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

    # Will deal with B spawner classes below

    # Special case for anadromous spawner _B_ classes
    if(dat$anadromous) {
      if(grepl("_B_", dat$stage_names[i])) {
        # Get custom alpha value
        # stage_B_4 = s3*mat4*smig4; stage_B_5 = s4*mat5*smig5

        # Get the spawn year
        msplit <- strsplit(dat$stage_names[i], "_")[[1]]
        msplit <- as.numeric(msplit[length(msplit)])

        # Remember s is set back by one stage
        alpha_B <- st[[t + 1]][paste0("s", msplit - 1)] * dat$mat[paste0("mat", msplit)] * dat$smig[paste0("smig", msplit)]

        # Get the number at time Nt1
        Nt1_B <- sNt1[paste0("K", msplit - 1)]
        if(is.na(Nt1_B)) {
          Nt1_B <- sNt1[msplit - 1]
        }

        # Get the capacity for the spawning class
        k_B <- as.numeric(k_stage[i]) # Do not apply any offset

        # Run the BH function and overwrite the value
        sNt2[i] <-
          eval(
            bh_stage_f(),
            list(
              alpha = alpha_B,  # Reference surv for previous stage...
              k = k_B,          # K is set for target stage
              Nt1 = Nt1_B       # Reference N for previous stage...
            )
          )
      }
    }

    # End of loop thourgh i stages
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

    # Anadromous non-spawner stages
    this_stage <- paste0("bh_stage_pb_", j)
    if(this_stage %in% bh_dd_stages) {
      N_adj[[j]] <- sNt2[[j]]
    }

    if(dat$anadromous) {
      # Apply BH to spawner stages
      if(grepl("_B_", dat$stage_names[j])) {
        this_stage <- paste0("bh_", tolower(dat$stage_names[j]))
        if(this_stage %in% bh_dd_stages) {
          N_adj[[j]] <- sNt2[[j]]
        }
      }
    }


  }

  # Non-linear stage structured matrix model
  # some staged will have more than 1 year
  # only apply BH curve if projected values
  # are greater than BH limit

  # MJB added Sept 27 2023
  N_adj[1] <- ifelse(is.na(N_adj[1]), 0, N_adj[1])


  # N_adj <- ifelse(N_adj > N, N, N_adj)
  if (any(is.na(N_adj))) {
    # K might be zero for stage...
    N_adj <- ifelse(is.na(N_adj), k_stage, N_adj)
    # if still NA then likley an erorr
    if (any(is.na(N_adj))) {
      stop("NA values in dd.N.bh")
    }
  }

  return(N_adj)

}
