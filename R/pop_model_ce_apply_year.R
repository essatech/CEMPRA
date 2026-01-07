#' Population Model CE Application to Vital Rates Year-Specific
#'
#' @description Utility function to apply cumulative effects to vital rates.
#'
#' @param rates Life history vital rates list object.
#' @param CE_df Cumulative effects data frame.
#' @param anadromous Anadromous TRUE/FALSE.
#' @param mat_stage Named numeric vector of mature life stages.
#'
#' @returns a modified dat vital rates object after applying CE effects.
#'
#' @export
#'
pop_model_ce_apply_year <- function(
    rates = NULL,
    CE_df = NULL,
    anadromous = FALSE,
    mat_stage = NULL
    ) {

  # if CE df is null then skip and exit function...
  if (is.null(CE_df)) {
    return(rates)
  }
  if (all(is.na(CE_df))) {
    return(rates)
  }
  if (nrow(CE_df) == 0) {
    return(rates)
  }

  # For testing import temp CE_df
  # filename <- system.file("extdata/simple_test/CE_df_testing.csv", package = "CEMPRA")
  # CE_df <- read.csv(filename, stringsAsFactors = FALSE)


  #-----------------------------------------------
  # Create vector of life stages for application
  #-----------------------------------------------

  # Need to split up life stage linkages
  # many will be comma separated
  CE_df$life_stage <- tolower(CE_df$life_stage)

  # Remove spaces
  CE_df$life_stage <- gsub(" ", "", CE_df$life_stage)

  # and remove underscores throughout
  CE_df$life_stage <- gsub("_", "", CE_df$life_stage)
  CE_df$life_stage <- tolower(CE_df$life_stage)

  # Split CE_df and make unique rows for each life stage
  CE_df_rebuid <- list()

  for(i in 1:nrow(CE_df)) {
    trow <- CE_df[i, ]
    stages <- unlist(strsplit(trow$life_stage, ","))
    brow <- trow[rep(1, length(stages)), ]
    brow$life_stage <- stages
    CE_df_rebuid[[i]] <- brow
  }
  CE_df_rebuid <- do.call("rbind", CE_df_rebuid)

  # overwrite original
  CE_df <- CE_df_rebuid

  # People mix up survival and survivorship
  CE_df$parameter <- ifelse(CE_df$parameter == "survivorship", "survival", CE_df$parameter)

  # Make sure simulation column is in the df
  if (!"simulation" %in% colnames(CE_df)) {
    CE_df$simulation <- 1
  }

  # Make sure HUC and stressor name is in DF and add it if not
  colnames(CE_df)[which(colnames(CE_df) == "Stressor")] <- "stressor"
  if (!"stressor" %in% colnames(CE_df)) {
    CE_df$stressor <- "stressor"
  }
  if (!"HUC" %in% colnames(CE_df)) {
    CE_df$HUC <- 1
  }

  # otherwise modify dat with CE stressors
  CE_df$parameter <- tolower(CE_df$parameter)
  CE_cap <- CE_df[CE_df$parameter == "capacity", ]
  CE_surv <- CE_df[CE_df$parameter == "survival", ]
  CE_fecund <- CE_df[CE_df$parameter == "fecundity", ]

  CE_cap_type <- CE_cap[, c("stressor", "life_stage", "parameter", "simulation", "HUC")]
  CE_surv_type <- CE_surv[, c("stressor", "life_stage", "parameter", "simulation", "HUC")]
  CE_fecund_type <- CE_fecund[, c("stressor", "life_stage", "parameter", "simulation", "HUC")]

  CE_cap_type    <- CE_cap_type[!(duplicated(CE_cap_type)), ]
  CE_surv_type   <- CE_surv_type[!(duplicated(CE_surv_type)), ]
  CE_fecund_type <- CE_fecund_type[!(duplicated(CE_fecund_type)), ]



  # ============================================================================
  # cumulative effects stressor is acting on fecundity
  # ============================================================================
  if (nrow(CE_fecund_type) > 0) {

    for (i in 1:nrow(CE_fecund_type)) {

      # Multiply effects additively for survival
      index_row <- CE_fecund_type[i, ]

      # Get all matching CE_fecund records from CE_fecund_type index columns
      trow <- CE_fecund[CE_fecund$stressor == index_row$stressor &
                          CE_fecund$simulation == index_row$simulation &
                          CE_fecund$parameter == index_row$parameter &
                          CE_fecund$life_stage == index_row$life_stage &
                          CE_fecund$HUC == index_row$HUC, ]

      if (any(is.na(trow$sys.cap))) {
        # Skip is missing data
        next
      }

      # Fecundity is knocked down by a stressor
      if (index_row$life_stage == "eps" | index_row$life_stage == "stage_b" | index_row$life_stage == "spawners" | index_row$life_stage == "adults" | index_row$life_stage == "stageb") {

        sys_caps <- c(trow$sys.cap, trow$sys.cap[1]) # Match length

        if(length(sys_caps) != length(rates$ft)) {
          sys_caps <- sample(sys_caps, size = length(rates$ft), replace = TRUE)
        }

        # Update values in surv vector
        rates$ft <- mapply(function(fec_vec, sys_caps) {
          fec_vec <- sys_caps * fec_vec
          fec_vec
        }, rates$ft, sys_caps, SIMPLIFY = FALSE)
      }

      # For ss in 1:12, build "stage{ss}" and "s{ss}" and "surv{ss}"
      for (ss in 1:12) {
        if (index_row$life_stage == paste0("eps", ss) |
            index_row$life_stage == paste0("stageb", ss)) {

          sys_caps <- c(trow$sys.cap, trow$sys.cap[1]) # Match length

          if(length(sys_caps) != length(rates$ft)) {
            sys_caps <- sample(sys_caps, size = length(rates$ft), replace = TRUE)
          }

          # Update values in surv vector
          rates$ft <- mapply(function(fec_vec, sys_caps) {
            fec_vec[paste0("eps", ss)] <- sys_caps * fec_vec[paste0("eps", ss)]
            fec_vec
          }, rates$ft, sys_caps, SIMPLIFY = FALSE)
        }
      }





    }

  }



  # ============================================================================
  # Cumulative effects stressor is acting on survivorship vital rate
  # ============================================================================
  # apply stressors to survival for eggs, juveniles, adults, or all life stages

  if (nrow(CE_surv_type) > 0) {
    for (i in 1:nrow(CE_surv_type)) {

      # Multiply effects additively for survival
      index_row <- CE_surv_type[i, ]

      # Get all matching CE_surv records from CE_surv_type index columns
      trow <- CE_surv[CE_surv$stressor == index_row$stressor &
                        CE_surv$simulation == index_row$simulation &
                        CE_surv$parameter == index_row$parameter &
                        CE_surv$life_stage == index_row$life_stage &
                        CE_surv$HUC == index_row$HUC, ]

      if (any(is.na(trow$sys.cap))) {
        # Skip is missing data
        next
      }

      # Update Normal Stages for egg
      if (index_row$life_stage == "stagee" | index_row$life_stage == "se" | index_row$life_stage == "egg" | index_row$life_stage == "eggs") {
        sys_caps <- c(trow$sys.cap, trow$sys.cap[1]) # Match length

        if(length(sys_caps) != length(rates$st)) {
          sys_caps <- sample(sys_caps, size = length(rates$st), replace = TRUE)
        }

        # Update values in surv vector
        rates$st <- mapply(function(surv_vec, sys_caps) {
          surv_vec["sE"] <- sys_caps * surv_vec["sE"]
          surv_vec
        }, rates$st, sys_caps, SIMPLIFY = FALSE)
      }

      # Update Normal Stages for fry
      if (index_row$life_stage == "stage0" | index_row$life_stage == "s0" | index_row$life_stage == "so" | index_row$life_stage == "fry") {
        sys_caps <- c(trow$sys.cap, trow$sys.cap[1]) # Match length

        if(length(sys_caps) != length(rates$st)) {
          sys_caps <- sample(sys_caps, size = length(rates$st), replace = TRUE)
        }

        # Update values in surv vector
        rates$st <- mapply(function(surv_vec, sys_caps) {
          surv_vec["s0"] <- sys_caps * surv_vec["s0"]
          surv_vec
        }, rates$st, sys_caps, SIMPLIFY = FALSE)
      }

      # If unspecified apply to all stages
      if (index_row$life_stage == "surv" | index_row$life_stage == "all") {
        sys_caps <- c(trow$sys.cap, trow$sys.cap[1]) # Match length

        if(length(sys_caps) != length(rates$st)) {
          sys_caps <- sample(sys_caps, size = length(rates$st), replace = TRUE)
        }

        # Update values in surv vector
        rates$st <- mapply(function(surv_vec, sys_caps) {
          surv_vec <- sys_caps * surv_vec
          surv_vec
        }, rates$st, sys_caps, SIMPLIFY = FALSE)
      }

      # For ss in 1:12, build "stage{ss}" and "s{ss}" and "surv{ss}"
      for (ss in 1:12) {
        if (index_row$life_stage == paste0("stage", ss) |
            index_row$life_stage == paste0("s", ss) |
            index_row$life_stage == paste0("surv", ss)) {

          sys_caps <- c(trow$sys.cap, trow$sys.cap[1]) # Match length

          if(length(sys_caps) != length(rates$st)) {
            sys_caps <- sample(sys_caps, size = length(rates$st), replace = TRUE)
          }

          # Update values in surv vector
          rates$st <- mapply(function(surv_vec, sys_caps) {
            surv_vec[paste0("s", ss)] <- sys_caps * surv_vec[paste0("s", ss)]
            surv_vec
          }, rates$st, sys_caps, SIMPLIFY = FALSE)
        }
      }


      # If generally targeting adult survivorship and not anadromous
      if (index_row$life_stage == "adult" & !(anadromous)) {

        # Get the adult stage position
        mature_n <- as.numeric(gsub("[^0-9]", "", names(mat_stage)))
        adult_stages <- paste0("s", mature_n)

        # Prepare sys_caps
        sys_caps <- c(trow$sys.cap, trow$sys.cap[1])

        if(length(sys_caps) != length(rates$st)) {
          sys_caps <- rep(sys_caps, length.out = length(rates$st))
        }

        # Update rates$smigt: multiply the "smigX" column by sys_caps
        rates$st <- mapply(function(surv_vec, sys_caps) {
          surv_vec[adult_stages] <- surv_vec[adult_stages] * sys_caps
          surv_vec
        }, rates$st, sys_caps, SIMPLIFY = FALSE)

      }

      # However, if adult and anadromous, then it is implied that it must
      # be linked to pre-spawn survivorship... adult is too generic included
      # for backwards compatibility
      if (index_row$life_stage == "adult" & (anadromous)) {
        index_row$life_stage <- "u"
      }


      # Prespawn mortality u stages
      # Update Normal Stages for juveniles

      if (index_row$life_stage == "u" | index_row$life_stage == "prespawn"| index_row$life_stage == "spawners") {
        sys_caps <- c(trow$sys.cap, trow$sys.cap[1]) # Match length

        if(length(sys_caps) != length(rates$ut)) {
          sys_caps <- sample(sys_caps, size = length(rates$ut), replace = TRUE)
        }

        # Update values in surv vector
        rates$ut <- mapply(function(surv_vec, sys_caps) {
          surv_vec <- sys_caps * surv_vec
          surv_vec
        }, rates$ut, sys_caps, SIMPLIFY = FALSE)
      }

      # Specific spawner stage class

      # For ss in 1:12, build "u{ss}" and "prespawn{ss}"
      for (ss in 1:12) {

        u_stage   <- paste0("u", ss)
        prespawn_stage <- paste0("prespawn", ss)

        # If index_row$life_stage is one of these, do the update
        if (index_row$life_stage == u_stage | index_row$life_stage == prespawn_stage) {
          sys_caps <- c(trow$sys.cap, trow$sys.cap[1]) # Match length

          if(length(sys_caps) != length(rates$ut)) {
            sys_caps <- sample(sys_caps, size = length(rates$ut), replace = TRUE)
          }

          # Update values in surv vector
          rates$ut <- mapply(function(surv_vec, sys_caps) {
            surv_vec[u_stage] <- sys_caps * surv_vec[u_stage]
            surv_vec
          }, rates$ut, sys_caps, SIMPLIFY = FALSE)
        }
      }

      # Spawner migration smig stages

      if (index_row$life_stage == "smig" | index_row$life_stage == "spawnmig") {
        sys_caps <- c(trow$sys.cap, trow$sys.cap[1]) # Match length

        if(length(sys_caps) != length(rates$smigt)) {
          sys_caps <- sample(sys_caps, size = length(rates$smigt), replace = TRUE)
        }

        # Update values in surv vector
        rates$smigt <- mapply(function(surv_vec, sys_caps) {
          surv_vec <- sys_caps * surv_vec
          surv_vec
        }, rates$smigt, sys_caps, SIMPLIFY = FALSE)
      }


      # Stage-specific migration rates

      # For ss in 1:12, build "smig{ss}" and "spawnmig{ss}"
      for (ss in 1:12) {
        smig_stage   <- paste0("smig", ss)
        spawn_stage <- paste0("spawnmig", ss)

        # If index_row$life_stage is one of these, do the update
        if (index_row$life_stage %in% c(smig_stage, spawn_stage)) {
          # Prepare sys_caps
          sys_caps <- c(trow$sys.cap, trow$sys.cap[1])

          if(length(sys_caps) != length(rates$smigt)) {
            sys_caps <- sample(sys_caps, size = length(rates$smigt), replace = TRUE)
          }

          # Update rates$smigt: multiply the "smigX" column by sys_caps
          rates$smigt <- mapply(function(surv_vec, sys_caps) {
            surv_vec[smig_stage] <- surv_vec[smig_stage] * sys_caps
            surv_vec
          }, rates$smigt, sys_caps, SIMPLIFY = FALSE)
        }
      }


    }
  }




  # ============================================================================
  # Cumulative effects stressor is acting on carrying capacity
  # ============================================================================
  # apply stressors to carrying capacity for eggs, juveniles, adults, or all life stages

  if (nrow(CE_cap_type) > 0) {
    for (i in 1:nrow(CE_cap_type)) {

      # Multiply effects additively for survival
      index_row <- CE_cap_type[i, ]

      # Get all matching CE_cap records from CE_cap_type index columns
      trow <- CE_cap[CE_cap$stressor == index_row$stressor &
                       CE_cap$simulation == index_row$simulation &
                       CE_cap$parameter == index_row$parameter &
                       CE_cap$life_stage == index_row$life_stage &
                       CE_cap$HUC == index_row$HUC, ]

      if (any(is.na(trow$sys.cap))) {
        # Skip is missing data
        next
      }


      # Update capacity for eggs
      if (index_row$life_stage == "se" |
          index_row$life_stage == "stagee" |
          index_row$life_stage == "egg" |
          index_row$life_stage == "eggs" | index_row$life_stage == "ke") {
        sys_caps <- c(trow$sys.cap, trow$sys.cap[1]) # Match length

        if(length(sys_caps) != length(rates$Ke_list)) {
          sys_caps <- sample(sys_caps, size = length(rates$Ke_list), replace = TRUE)
        }

        # Update values in surv vector
        rates$Ke_list <- mapply(function(cap_vec, sys_caps) {
          cap_vec <- sys_caps * cap_vec
          cap_vec
        }, rates$Ke_list, sys_caps, SIMPLIFY = FALSE)
      }

      # Update capacity for fry
      if (index_row$life_stage == "s0" |
          index_row$life_stage == "so" |
          index_row$life_stage == "stage0" |
          index_row$life_stage == "fry" | index_row$life_stage == "k0") {
        sys_caps <- c(trow$sys.cap, trow$sys.cap[1]) # Match length

        if(length(sys_caps) != length(rates$K0_list)) {
          sys_caps <- sample(sys_caps, size = length(rates$K0_list), replace = TRUE)
        }

        # Update values in surv vector
        rates$K0_list <- mapply(function(cap_vec, sys_caps) {
          cap_vec <- sys_caps * cap_vec
          cap_vec
        }, rates$K0_list, sys_caps, SIMPLIFY = FALSE)
      }



      if (names(rates$K_list[[1]])[1] == "K1") {
        k_prefix <- "K"
      }
      if (names(rates$K_list[[1]])[1] == "stage_Pb_1") {
        k_prefix <- "stage_Pb_"
      }

      # For ss in 1:12, build "smig{ss}" and "spawnmig{ss}"
      for (ss in 1:12) {

        stage_stage   <- paste0("stage", ss)
        s_stage <- paste0("s", ss)
        k_stage <- paste0("k", ss)
        stage_pb_stage <- paste0("stage_pb_", ss)
        p_stage <- paste0("p", ss)

        # If index_row$life_stage is one of these, do the update
        if (index_row$life_stage %in% c(stage_stage, s_stage, k_stage, stage_pb_stage, p_stage)) {

          # Prepare sys_caps
          sys_caps <- c(trow$sys.cap, trow$sys.cap[1])

          if(length(sys_caps) != length(rates$K_list)) {
            sys_caps <- sample(sys_caps, size = length(rates$K_list), replace = TRUE)
          }

          target_stage <- paste0(k_prefix, ss)

          # Update rates$K_list: to adjust stage capacity
          rates$K_list <- mapply(function(cap_vec, sys_caps) {
            cap_vec[target_stage] <- cap_vec[target_stage] * sys_caps
            cap_vec
          }, rates$K_list, sys_caps, SIMPLIFY = FALSE)
        }
      }


      # Specific spawner stage capacities...
      # For ss in 1:12, build "{ss}"
      for (ss in 1:12) {

        check1 <- paste0("spawn", ss)
        check2 <- paste0("spawners", ss)
        check3 <- paste0("b", ss)
        check4 <- paste0("stage_b_", ss)
        check5 <- paste0("stageb", ss)
        check6 <- paste0("spawner", ss)

        # If index_row$life_stage is one of these, do the update
        if (index_row$life_stage %in% c(check1, check2, check3, check4, check5, check6)) {

          # Prepare sys_caps
          sys_caps <- c(trow$sys.cap, trow$sys.cap[1])

          if(length(sys_caps) != length(rates$K_list)) {
            sys_caps <- sample(sys_caps, size = length(rates$K_list), replace = TRUE)
          }

          target_stage <- paste0("stage_B_", ss)

          # Update rates$K_list: to adjust stage capacity
          rates$K_list <- mapply(function(cap_vec, sys_caps) {
            cap_vec[target_stage] <- cap_vec[target_stage] * sys_caps
            cap_vec
          }, rates$K_list, sys_caps, SIMPLIFY = FALSE)
        }
      }



      # If generally targeting adult CAPACITY and not anadromous
      if (index_row$life_stage == "adult" & !(anadromous)) {

        # Get the adult stage position
        mature_n <- as.numeric(gsub("[^0-9]", "", names(mat_stage)))
        adult_stages <- paste0("K", mature_n)

        # Prepare sys_caps
        sys_caps <- c(trow$sys.cap, trow$sys.cap[1])

        if(length(sys_caps) != length(rates$K_list)) {
          sys_caps <- rep(sys_caps, length.out = length(rates$K_list))
        }

        # Update rates$smigt: multiply the "smigX" column by sys_caps
        rates$K_list <- mapply(function(cap_vec, sys_caps) {
          cap_vec[adult_stages] <- cap_vec[adult_stages] * sys_caps
          cap_vec
        }, rates$K_list, sys_caps, SIMPLIFY = FALSE)

      }

      # However, if adult and anadromous, then it is implied that it must
      # be linked to spawner capacity... adult is too generic included
      # for backwards compatibility
      if (index_row$life_stage == "adult" & (anadromous)) {
        index_row$life_stage <- "spawners"
      }


      # Update capacity for ALL spawners
      if (index_row$life_stage == "spawner" |
          index_row$life_stage == "spawners" |
          index_row$life_stage == "stage_b" |
          index_row$life_stage == "spawn") {

        sys_caps <- c(trow$sys.cap, trow$sys.cap[1]) # Match length

        if(length(sys_caps) != length(rates$Kspawners_list)) {
          sys_caps <- sample(sys_caps, size = length(rates$Kspawners_list), replace = TRUE)
        }

        # Update values in Kspawners_list vector
        rates$Kspawners_list <- mapply(function(cap_vec, sys_caps) {
          cap_vec <- sys_caps * cap_vec
          cap_vec
        }, rates$Kspawners_list, sys_caps, SIMPLIFY = FALSE)

        # Also update values in the K_list vector
        rates$K_list <- mapply(function(cap_vec, sys_caps) {
          cap_vec[grepl("^stage_B_", names(cap_vec))] <- sys_caps * cap_vec[grepl("^stage_B_", names(cap_vec))]
          cap_vec
        }, rates$K_list, sys_caps, SIMPLIFY = FALSE)

      }

      if (index_row$life_stage == "all") {
        message("index_row$life_stage == 'all'")
        stop("all stage not yet implemented")
      }

    }

  } # end of capacity section



  # Return the modified life history vital rate data
  return(rates)


}
