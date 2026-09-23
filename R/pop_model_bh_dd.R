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


#' bh_inflow_f
#' @description Beverton-Holt saturation applied to a realized inflow of
#' recruits approaching a capacity k. Algebraically identical to bh_stage_f
#' with inflow = alpha * Nt1:
#' (alpha * Nt1) / (1 + (alpha/k) * Nt1) = inflow / (1 + inflow / k).
#' Returns the inflow unchanged as k approaches Inf and saturates at k.
#'
#' @keywords internal
bh_inflow_f <- function(inflow, k) {
  inflow / (1 + inflow / k)
}


#' dd.N.bh
#' @description Apply location and stage-specific density-dependent capacity
#' constraints (Beverton-Holt or hockey-stick) to the projected population
#' vector.
#'
#' @details Stage capacities (K1, K2, ...) are the maximum TOTAL abundance of
#' each stage: holdover individuals remaining in a multi-year stage plus this
#' year's newcomers. Newcomers are admitted into the residual capacity
#' (K minus holdovers) through a Beverton-Holt curve (bh_ flags), while hs_
#' flags cap the stage total directly (hard ceiling). If holdovers alone
#' exceed K (e.g., the year after a capacity stressor reduces K), the stage
#' is trimmed down to K and no newcomers are admitted (habitat loss displaces
#' residents). The egg and fry capacities (Ke, K0) remain flow caps: the
#' egg -> fry -> stage-1 transition always completes within a single census
#' year, so those transitions can never carry holdovers.
#'
#' @param dat Life history data for the current year (K, K0, Nstage, ...).
#' @param t Current year index.
#' @param st List of stochastic survival vectors by year.
#' @param N Projected population vector for the current year (N = A %*% N_prev).
#' @param N_prev Population vector from the previous year.
#' @param bh_dd_stages Character vector of DD flags (bh_stage_X, hs_stage_X, ...).
#' @param A Realized projection matrix for the current year (stochastic draws,
#' compensation-ratio and harm effects included). Used to decompose each stage
#' into holdovers (`A[i,i] * N_prev[i]`) and inflow (`N[i]` - holdovers). If NULL,
#' all stages are assumed to spend one year per stage (no holdovers).
#'
#' @keywords internal
dd.N.bh <- function(dat = NA, t = NA, st = NA, N = NA, N_prev = NA,
                    bh_dd_stages = NA, A = NULL) {

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

  # dd_hs_spawners is an accepted alias of hs_spawners (for direct
  # callers; Projection_DD already normalizes it)
  bh_dd_stages <- ifelse(bh_dd_stages == "dd_hs_spawners",
                         "hs_spawners", bh_dd_stages)


  if(length(dat$K) != dat$Nstage) {
    stop("error in dd.N.bh... Nstage does not match K vector")
  }

  # Allow extra entry for bh_spawners/hs_spawners
  n_spawner_flags <- sum(c("bh_spawners", "hs_spawners") %in% bh_dd_stages)
  if(length(bh_dd_stages) > (dat$Nstage + 1 + n_spawner_flags)) {
    stop("error in dd.N.bh... bh_dd_stages can only include dd_hs_0 and bh_stage_X values for each class")
  }


  # Nothing left virtually extinct...
  if(all(N < 1)) {
    return(N)
  }

  # Create a new population vector with revised values
  # Recall vector is S1,S2,S3,S4...B1,B2,B3,B4... (no eggs no SE; no fry s0)
  N_adj <- N

  # Holdovers: individuals that survived and remain within the same
  # (multi-year) stage this year. A[i,i] is the realized stay rate; it is
  # zero for 1-year stages (including all anadromous stages) and treated as
  # zero for every stage when A is not supplied.
  if (is.null(A)) {
    holdovers <- rep(0, length(N))
  } else {
    holdovers <- diag(A) * N_prev
    holdovers <- ifelse(is.na(holdovers), 0, holdovers)
  }

  # ----------------------------------------------
  # Stage 1: separate holdovers from this year's recruits.
  # Recruits arrive through the within-year egg -> fry -> stage-1 chain
  # (row-1 fecundity terms of the projection matrix). That chain always
  # completes within one census year, so egg (Ke) and fry (K0) capacities
  # are pure flow caps with no holdover component.

  # Fry survival for this transition
  step_s0 <- st[[t + 1]]["s0"]

  # Stage-1 recruits this year (excludes stage-1 holdovers)
  newcomers_1 <- max(N[[1]] - holdovers[1], 0)

  # Fry (age-0) produced this year, back-calculated from stage-1 recruits
  N_stage_0 <- newcomers_1 / step_s0
  if (!is.finite(N_stage_0)) {
    N_stage_0 <- 0
  }

  # Should DD be applied to egg to fry (sE to s0) transition
  # using a hockey-stick type DD? (flow cap)
  if(any(c("dd_hs_0", "hs_stage_0") %in% bh_dd_stages)) {
    if(!(is.na(dat$K0))) {
      # Truncate surviving fry to max egg capacity (max fry)
      N_stage_Et_to_0_max <- as.numeric(dat$K0)
      N_stage_0 <- ifelse(N_stage_0 > N_stage_Et_to_0_max, N_stage_Et_to_0_max, N_stage_0)
    }
  }

  # ------------------------------------------------------------
  # Apply hockey-stick DD to other specific life stages (if set)
  # hs_ flags are hard ceilings on the TOTAL abundance of a stage
  # ------------------------------------------------------------
  if(any(grepl("hs", bh_dd_stages))) {

    # Apply hockey-stick-style DD to other life stages (if set)
    for (i in 1:dat$Nstage) {

      # Determine if DD constraint for specific stage is defined...
      check_2 <- paste0("hs_stage_", i)

      # if neither are present skip to next stage
      if(check_2 %in% bh_dd_stages) {
        # Apply hockey-stick DD to life stage K
        N_cap <- as.numeric(dat$K[[i]])
        if(!(is.na(N_cap))) {
          N_adj[i] <- ifelse(N_adj[i] > N_cap, N_cap, N_adj[i])
        }
      }

      # Check if target is applied to "stage_pb_X"
      check_3 <- paste0("hs_stage_pb_", i)
      if(check_3 %in% bh_dd_stages) {
        # Apply hockey-stick DD to life stage K
        stage_index <- which(paste0("stage_Pb_", i) == dat$stage_names)[1]
        if(!(is.na(stage_index))) {
          N_cap <- as.numeric(dat$K[[stage_index]])
          if(!(is.na(N_cap))) {
            N_adj[stage_index] <- ifelse(N_adj[stage_index] > N_cap, N_cap, N_adj[stage_index])
          }
        }
      }

      # Check if target is applied to "stage_b_X"
      check_4 <- paste0("hs_stage_b_", i)
      if(check_4 %in% bh_dd_stages) {
        # Apply hockey-stick DD to life stage K
        stage_index <- which(paste0("stage_B_", i) == dat$stage_names)[1]
        if(!(is.na(stage_index))) {
          N_cap <- as.numeric(dat$K[[stage_index]])
          if(!(is.na(N_cap))) {
            N_adj[stage_index] <- ifelse(N_adj[stage_index] > N_cap, N_cap, N_adj[stage_index])
          }
        }
      }
    }
  }


  # Apply hockey-stick DD to all spawners (if set)
  if("hs_spawners" %in% bh_dd_stages) {
    # Get spawner cap
    spawner_cap <- dat$stage_k_spawners
    # Get current spawners - identify by stage_B_ (anadromous) or maturity (non-anadromous)
    if(dat$anadromous) {
      # Anadromous B classes are 100% spawners by construction (weight 1)
      stage_index <- which(grepl("stage_B_", dat$stage_names))
      spawn_w <- rep(1, length(stage_index))
    } else {
      # Non-anadromous: mature stages identified via maturity vector.
      # Stages can be PARTIALLY mature (0 < mat < 1), so abundances are
      # weighted by maturity - the cap counts actual spawners (the same
      # Nb_est convention used everywhere else), not the full abundance
      # of every stage that contains some mature fish.
      mat_pos <- dat$mat[!is.na(dat$mat) & dat$mat > 0]
      stage_index <- as.numeric(gsub("mat", "", names(mat_pos)))
      spawn_w <- as.numeric(mat_pos)
    }
    current_spawners <- N_adj[stage_index] * spawn_w
    current_spawners_s <- sum(current_spawners)

    if(current_spawners_s > spawner_cap) {
      # Apply simple hockey stick DD: scale the spawner stages so the
      # maturity-weighted spawner count lands exactly on the cap.
      # Immature fish sharing a partially-mature stage are reduced
      # proportionally (the state vector cannot separate them).
      N_adj[stage_index] <- N_adj[stage_index] * (spawner_cap / current_spawners_s)
    }
  }


  # ==============================================
  # Adjust capacities with Beverton-Holt style DD
  # ==============================================


  # ----------------------------------------------
  # Calculate max egg to fry with Beverton-Holt (flow cap - the egg -> fry
  # transition always completes within one census year)
  if (any(c("bh_stage_0") %in% bh_dd_stages)) {
    if(!(is.na(dat$K0))) {
      # BH on the egg -> fry flow. With alpha = sE and Nt1 = eggs this is
      # algebraically identical to inflow / (1 + inflow / K0) where
      # inflow = fry produced this year.
      N_stage_0 <- bh_inflow_f(N_stage_0, as.numeric(dat$K0))
    }
  }

  # ----------------------------------------------
  # Stage 1 total: holdovers plus (possibly flow-capped) recruits.

  # Functionality for stage 1 is the same as anadromous vs non anadromous so adjust here
  bh_dd_stages <- ifelse(bh_dd_stages == "bh_stage_pb_1", "bh_stage_1", bh_dd_stages)

  # Capacity for stage 1
  k_stage_1 <- as.numeric(dat$K[[1]])

  stage1_flow_flagged <- any(c("dd_hs_0", "hs_stage_0", "bh_stage_0") %in% bh_dd_stages)
  stage1_modified <- FALSE

  if(("bh_stage_1" %in% bh_dd_stages) && !(is.na(k_stage_1))) {
    if(holdovers[1] >= k_stage_1) {
      # Residents alone exceed the stage capacity (e.g., K was reduced by a
      # capacity stressor): trim to K and admit no newcomers - habitat loss
      # displaces residents.
      N_adj[[1]] <- k_stage_1
    } else {
      # Admit this year's recruits into the residual capacity
      # (K minus holdovers) through the BH curve
      inflow_1 <- N_stage_0 * step_s0
      N_adj[[1]] <- holdovers[1] + bh_inflow_f(inflow_1, k_stage_1 - holdovers[1])
    }
    stage1_modified <- TRUE
  } else if(any(c("hs_stage_1", "hs_stage_pb_1") %in% bh_dd_stages)) {
    # Total-abundance cap was already applied in the hockey-stick block
    # above - do NOT overwrite N_adj[1] with an admission calculation.
    # But if an egg/fry flow cap (K0) is ALSO flagged, the inflow must
    # still honour it: admit at most the flow-capped recruits on top of
    # the holdovers, and never exceed the HS total cap already applied.
    # (Previously the flow cap was silently discarded in this branch.)
    if (stage1_flow_flagged) {
      N_adj[[1]] <- min(N_adj[[1]], holdovers[1] + N_stage_0 * step_s0)
      stage1_modified <- TRUE
    }
  } else if(stage1_flow_flagged) {
    # Only egg/fry flow caps are set: capped recruits plus untouched holdovers
    N_adj[[1]] <- holdovers[1] + N_stage_0 * step_s0
    stage1_modified <- TRUE
  }
  # (no stage-1 related flags: leave the projected value unchanged)


  # ----------------------------------------------
  # Repeat BH for other subsequent life stages
  # ----------------------------------------------
  k_stage <- dat$K


  # Special bh_spawners (all classes)
  # If bh_spawners is present adjust K for spawners in proportion to
  # relative abundance in each spawning class

  if("bh_spawners" %in% bh_dd_stages) {

    # Get spawner cap
    spawner_cap <- dat$stage_k_spawners
    # Get current spawners - identify by stage_B_ (anadromous) or maturity (non-anadromous)
    if(dat$anadromous) {
      # Anadromous B classes are 100% spawners by construction (weight 1)
      stage_index <- which(grepl("stage_B_", dat$stage_names))
      spawn_w <- rep(1, length(stage_index))
    } else {
      # Non-anadromous: mature stages identified via maturity vector.
      # Weighted by maturity so the cap counts actual spawners (Nb_est
      # convention) - see the hs_spawners block above.
      mat_pos <- dat$mat[!is.na(dat$mat) & dat$mat > 0]
      stage_index <- as.numeric(gsub("mat", "", names(mat_pos)))
      spawn_w <- as.numeric(mat_pos)
    }
    current_spawners <- N_adj[stage_index] * spawn_w
    current_spawners_s <- sum(current_spawners)

    if(current_spawners_s > spawner_cap) {
      # Reduce the per-stage K values by the relative fraction so that,
      # after the BH admission below, the maturity-weighted spawner count
      # approaches the cap
      k_stage[stage_index] <- N_adj[stage_index] * (spawner_cap / current_spawners_s)

      # Append BH classes to DD vector
      if(dat$anadromous) {
        add_these <- paste0("bh_", tolower(dat$stage_names[stage_index]))
      } else {
        add_these <- paste0("bh_stage_", stage_index)
      }
      bh_dd_stages <- c(bh_dd_stages, add_these)
      bh_dd_stages <- unique(bh_dd_stages)

    }
  } # end of special bh_spawners (all classes)


  # For each stage, decompose the projected abundance into holdovers
  # (A[i,i] * N_prev[i]) and inflow (N[i] - holdovers), then admit the
  # inflow into the residual capacity (K - holdovers) with the BH curve.
  # Because N = A %*% N_prev this is exact for every inflow pathway,
  # including the anadromous maturation terms (s * mat * smig).
  sNt2 <- rep(NA_real_, length(N))

  for (i in 2:length(N)) {

    k_i <- as.numeric(k_stage[i])
    if(is.na(k_i)) { next }  # NA capacity = unconstrained stage

    H_i <- holdovers[i]
    inflow_i <- N[[i]] - H_i
    if(is.na(inflow_i)) { next }
    inflow_i <- max(inflow_i, 0)

    if(H_i >= k_i) {
      # Residents alone exceed capacity (e.g., K reduced by a capacity
      # stressor): trim to K, no newcomers admitted
      sNt2[i] <- k_i
    } else {
      sNt2[i] <- H_i + bh_inflow_f(inflow_i, k_i - H_i)
    }

    # End of loop thourgh i stages
  }

  # Extinction threshold applied to the stage totals
  # cleans up basins with less than 1 individual
  sNt2[sNt2 < 1] <- 0

  # Update values according to user-specified BH-DD relationships
  # important: skip stage 1. Start at stage 2.
  for(j in 2:(dat$Nstage)) {

    if(is.na(sNt2[j])) { next }

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

  # Extinction threshold for the stage-1 total (only when modified above)
  if(stage1_modified) {
    if(!(is.na(N_adj[[1]])) && N_adj[[1]] < 1) {
      N_adj[[1]] <- 0
    }
  }

  # MJB added Sept 27 2023
  N_adj[1] <- ifelse(is.na(N_adj[1]), 0, N_adj[1])


  if (any(is.na(N_adj))) {
    # K might be zero for stage...
    N_adj <- ifelse(is.na(N_adj), k_stage, N_adj)
    # if still NA then likley an erorr
    if (any(is.na(N_adj))) {
      stop("NA values in dd.N.bh")
    }
  }

  # Density dependence can only remove individuals, never add them: use the
  # BH/HS values as a ceiling on the projected vector.
  N_adj <- ifelse(is.na(N), N_adj, pmin(N_adj, N))

  return(N_adj)

}
