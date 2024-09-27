#' build_k_for_proj_dd
#' @description Build build_habitat_dd_k object and bh_dd_stages from PopulationModel_Run for Projection_DD
#'
#' @keywords internal
build_k_for_proj_dd <- function(habitat_dd_k, HUC_ID, life_histories, life_cycle_params) {

  # Assume there is no DD is applied unless specified
  stage_k_override <- NULL
  bh_dd_stages <- NULL
  total_anadromous_spawners <- NULL

  # Filter for target watershed
  if (is.null(habitat_dd_k) == FALSE) {


    # Filter for target location
    hab_dd_k <- habitat_dd_k[habitat_dd_k$HUC_ID == HUC_ID, ]

    if (nrow(hab_dd_k) != 1) {
      stop("HUC_ID inhabitat_dd_k.xlsx does not match stressor magnitude data...")
    }

    # Check if the population is anadromous
    anadromous <- life_histories$anadromous

    # Assume not anadromous if null
    if(is.null(anadromous)) {
      anadromous <- FALSE
    }

    # ----------------------------------------
    # Build K vector for anadromous population
    # ----------------------------------------

    if(anadromous) {

      # --------------------------------
      # For anadrmous populations

      # Create the k vector
      # Non-spawner classes

      life_histories$stage_names
      life_histories$spanwing_years

      bh_dd_stages_set <- list()
      bh_dd_stages_set_qa <- list()

      countr <- 1

      # +1 for fry at beginning +1 spawners at end (1+1 = 2)
      stage_k_override <- rep(NA, (length(life_histories$stage_names) + 1))
      names(stage_k_override) <- c("K0", life_histories$stage_names)


      # Need to add on the K0 fry class if present
      check_this <- paste0("k_stage_0_mean")
      if (check_this %in% colnames(hab_dd_k)) {
        mean_k <- hab_dd_k[1, paste0("k_stage_0_mean")]

        # Set spawner K
        if (!is.na(mean_k)) {
          stage_k_override["K0"] <- as.numeric(mean_k[1, 1])
          bh_dd_stages_set_qa[[countr]] <- paste0("bh_stage_0")

          indx1 <- which(life_cycle_params$Name == paste0("bh_stage_0"))
          indx2 <- which(life_cycle_params$Name == paste0("hs_stage_0"))
          if(length(indx1) > 0) {
            bh_dd_stages_set[[countr]] <- paste0("bh_stage_0")
          } else if(length(indx2) > 0) {
            bh_dd_stages_set[[countr]] <- paste0("hs_stage_0")
          }

          countr <- 1 + countr
        }
      }


      # Ensure that stages are represented
      # resample habitat k values
      for (s in 1:life_histories$Nstage_Pb) {

        check_this <- paste0("k_stage_Pb_", s, "_mean")
        if (check_this %in% colnames(hab_dd_k)) {
          # Sample capacity for year and location
          mean_k_pb <- hab_dd_k[1, paste0("k_stage_Pb_", s, "_mean")]

          # Set non-spawner K
          if (!is.na(mean_k_pb)) {
            stage_k_override[paste0("stage_Pb_", s)] <- as.numeric(mean_k_pb[1, 1])
            # Set BH set
            bh_dd_stages_set_qa[[countr]] <- paste0("bh_stage_pb_", s)

            indx1 <- which(life_cycle_params$Name == paste0("bh_stage_pb_", s))
            indx2 <- which(life_cycle_params$Name == paste0("hs_stage_pb_", s))
            if(length(indx1) > 0) {
              bh_dd_stages_set[[countr]] <- paste0("bh_stage_pb_", s)
            } else if(length(indx2) > 0) {
              bh_dd_stages_set[[countr]] <- paste0("hs_stage_pb_", s)
            }

            countr <- 1 + countr
          }
        }

        # For spawner age classes
        check_this <- paste0("k_stage_B_", s, "_mean")
        if (check_this %in% colnames(hab_dd_k)) {
          mean_k_B <- hab_dd_k[1, paste0("k_stage_B_", s, "_mean")]

          # Set age-specific spawner K
          if (!is.na(mean_k_B)) {
            stage_k_override[paste0("stage_B_", s)] <- as.numeric(mean_k_B[1, 1])
            bh_dd_stages_set_qa[[countr]] <- paste0("bh_stage_b_", s)

            indx1 <- which(life_cycle_params$Name == paste0("bh_stage_b_", s))
            indx2 <- which(life_cycle_params$Name == paste0("hs_stage_b_", s))
            if(length(indx1) > 0) {
              bh_dd_stages_set[[countr]] <- paste0("bh_stage_b_", s)
            } else if(length(indx2) > 0) {
              bh_dd_stages_set[[countr]] <- paste0("hs_stage_b_", s)
            }

            countr <- 1 + countr

          }
        }

      }



      # Finally adjust for total spawners... if there is any
      # generic input for k_stage_B_mean
      check_this <- paste0("k_stage_B_mean")
      if (check_this %in% colnames(hab_dd_k)) {

        mean_k <- hab_dd_k[1, paste0("k_stage_B_mean")]

        # Set spawner K
        if (!is.na(mean_k)) {
          total_anadromous_spawners <- as.numeric(mean_k[1, 1])
          bh_dd_stages_set_qa[[countr]] <- paste0("bh_spawners")

          indx1 <- which(life_cycle_params$Name == paste0("bh_spawners"))
          indx2 <- which(life_cycle_params$Name == paste0("hs_spawners"))
          if(length(indx1) > 0) {
            bh_dd_stages_set[[countr]] <- paste0("bh_spawners")
          } else if(length(indx2) > 0) {
            bh_dd_stages_set[[countr]] <- paste0("hs_spawners")
          }

          countr <- 1 + countr
        }

      }

      bh_dd_stages_set_qa <- unlist(bh_dd_stages_set_qa)

      # easy to make error here
      # Data frame should always match the Excel table
      check1 <- gsub("bh_", "", bh_dd_stages_set_qa)
      check1 <- gsub("hs_", "", check1)


      # Then specify which parameters to use
      bh_dd_stages <- unlist(bh_dd_stages_set)

      check2 <- gsub("bh_", "", bh_dd_stages_set)
      check2 <- gsub("hs_", "", check2)

      any_diff1 <- setdiff(check1, check2)
      any_diff2 <- setdiff(check2, check1)


      if(length(any_diff1) > 0 | length(any_diff2) > 0) {
        print("Expect from life cycles table:")
        print(bh_dd_stages)
        print("Data in location K table:")
        print(bh_dd_stages_set_qa)
        stop("Error: bh_stage_... in life cycle params does not match K ages... in habitat data")
      }



      # end of anadromous populations

    } else {

      #########################################################
      #########################################################
      #########################################################
      #########################################################
      #########################################################

      # --------------------------------
      # For non-anadrmous populations
      # --------------------------------

      bh_dd_stages_set <- list()

      stage_k_override <- rep(NA, (life_histories$Nstage + 1))
      names(stage_k_override) <-
        paste0("k_stage_", 0:(life_histories$Nstage))

      # Ensure that stages are represented
      # resample habitat k values
      for (s in 0:(life_histories$Nstage)) {

        # Sample capacity for year and location
        mean_k <- hab_dd_k[1, paste0("k_stage_", s, "_mean")]
        cv_k <- hab_dd_k[1, paste0("k_stage_", s, "_cv")]

        if (!is.na(mean_k)) {
          cv_k <- ifelse(is.na(cv_k), 0, cv_k)
          this_k <-
            stats::rnorm(1, mean = as.numeric(mean_k), sd = as.numeric(mean_k * cv_k))
          stage_k_override[s + 1] <- this_k
        }

        # Set the Beverton-Holt DD mechanism (if set in life cycles file)
        if(s == 0){

          egg_fry <- life_histories$Value[life_cycle_params$Name == "dd_hs_0"]

          if(length(egg_fry) > 0) {
            if(!(is.na(egg_fry))) {
              if(egg_fry == 1) {
                bh_dd_stages_set[[1]] <- "dd_hs_0"
              }
            }
          }

        } else {
          dd_stage <- life_histories$Value[life_cycle_params$Name == paste0("bh_stage_", s)]
          if(length(dd_stage) > 0) {
            if(!(is.na(dd_stage))) {
              if(dd_stage == 1) {
                bh_dd_stages_set[[1 + s]] <- paste0("bh_stage_", s)
              }
            }
          }
        }
      }

      # Then specify which parameters to use
      bh_dd_stages <- unlist(bh_dd_stages_set)

    } # end of non-anadrmous populations







  } # end of is.null(habitat_dd_k) == FALSE




  ret_obj <- list()
  ret_obj$ret_stage_k_override <- stage_k_override
  ret_obj$ret_bh_dd_stages <- bh_dd_stages
  ret_obj$ret_total_anadromous_spawners <- total_anadromous_spawners


  return(ret_obj)


}
