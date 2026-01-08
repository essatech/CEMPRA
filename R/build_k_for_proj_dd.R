#' build_k_for_proj_dd
#' @description Build build_habitat_dd_k object and bh_dd_stages from PopulationModel_Run for Projection_DD.
#' @param life_histories Object returned from pop_model_matrix_elements()$life_histories
#' @param life_cycle_params Object returned from pop_model_dat_clean() without any further editing
#'
#' @details This function builds the habitat carrying capacity (K) vector for
#' use in density dependent projections. It extracts K values from the
#' habitat_dd_k input data frame for the target HUC_ID and formats them for
#' use in Projection_DD(). It also identifies which life stages are subject
#' to density dependence based on the life cycle parameters. The function will
#' return an empty list of there is no location-based DD.
#'
#' @returns A list with the following components:
#' * ret_stage_k_override: A named numeric vector of carrying capacity (K) values
#'  for each life stage, including fry (K0) and spawners if applicable.
#'  * ret_bh_dd_stages: A character vector of life stage names that are subject to
#'  Beverton-Holt density dependence.
#'  * ret_total_anadromous_spawners: A numeric value representing the
#'  total carrying capacity for anadromous spawners, if applicable.
#'
#'
#'
#' @export
build_k_for_proj_dd <- function(habitat_dd_k,
                                HUC_ID,
                                life_histories,
                                life_cycle_params) {

  # Assume there is no DD is applied unless specified
  stage_k_override <- NULL
  bh_dd_stages <- NULL
  total_anadromous_spawners <- NULL

  # Filter for target watershed
  if (is.null(habitat_dd_k) == FALSE) {

    # Filter for target location
    hab_dd_k <- habitat_dd_k[habitat_dd_k$HUC_ID == HUC_ID, ]

    # Fix case for _Pb_ and _B_
    fixnms <- colnames(hab_dd_k)
    fixnms <- gsub("_pb_", "_Pb_", fixnms)
    fixnms <- gsub("_PB_", "_Pb_", fixnms)
    fixnms <- gsub("_b_", "_B_", fixnms)
    fixnms <- gsub("_Stage_", "_stage_", fixnms)

    # Fix spawner column name convention
    fixnms <- gsub("k_stage_b_mean", "k_stage_B_mean", fixnms)
    fixnms <- gsub("k_spawners_mean", "k_stage_B_mean", fixnms)
    fixnms <- gsub("k_spawner_mean", "k_stage_B_mean", fixnms)

    colnames(hab_dd_k) <- fixnms


    if (nrow(hab_dd_k) != 1) {
      stop("HUC_ID inhabitat_dd_k.xlsx does not match stressor magnitude data...")
    }

    # Check if the population is anadromous
    anadromous <- life_histories$anadromous

    # Assume not anadromous if null
    if (is.null(anadromous)) {
      anadromous <- FALSE
    }

    # ----------------------------------------
    # Build K vector for anadromous population
    # ----------------------------------------

    if (anadromous) {
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

      if (check_this %in% tolower(colnames(hab_dd_k))) {

        mean_k <- hab_dd_k[1, paste0("k_stage_0_mean")]

        # Set fry K
        if (!is.na(mean_k)) {

          if (class(mean_k)[1] == "numeric" |
              class(mean_k)[1] == "integer" |
              class(mean_k)[1] == "character") {
            stage_k_override["K0"] <- as.numeric(mean_k)
          } else {
            stage_k_override["K0"] <- as.numeric(mean_k[1, 1])
          }


          bh_dd_stages_set_qa[[countr]] <- paste0("bh_stage_0")

          indx1 <- which(life_cycle_params$Name == paste0("bh_stage_0"))
          indx2 <- which(life_cycle_params$Name == paste0("hs_stage_0"))

          if (length(indx1) > 0) {
            bh_dd_stages_set[[countr]] <- paste0("bh_stage_0")
          } else if (length(indx2) > 0) {
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

            if (class(mean_k_pb)[1] == "integer" | class(mean_k_pb)[1] == "numeric") {
              k1_cap <- as.numeric(mean_k_pb)
            } else {
              k1_cap <- as.numeric(mean_k_pb[1, 1])
            }

            stage_k_override[paste0("stage_Pb_", s)] <- k1_cap

            # Set BH set
            bh_dd_stages_set_qa[[countr]] <- paste0("bh_stage_pb_", s)

            indx1 <- which(life_cycle_params$Name == paste0("bh_stage_pb_", s))
            indx2 <- which(life_cycle_params$Name == paste0("hs_stage_pb_", s))

            if (length(indx1) > 0) {
              bh_dd_stages_set[[countr]] <- paste0("bh_stage_pb_", s)
            } else if (length(indx2) > 0) {
              bh_dd_stages_set[[countr]] <- paste0("hs_stage_pb_", s)
            }

            countr <- 1 + countr
          }
        }


        # For spawner age classes
        check_this <- paste0("k_stage_B_", s, "_mean")

        if (check_this %in% colnames(hab_dd_k)) {

          mean_k_B <- hab_dd_k[1, paste0("k_stage_B_", s, "_mean")]

          if (length(mean_k_B) == 0) {
            mean_k_B <- NA
          }

          # Set age-specific spawner K
          if (!is.na(mean_k_B)) {
            if (class(stage_k_override)[1] == "numeric" |
                class(stage_k_override)[1] == "integer" |
                class(mean_k)[1] == "character") {
              stage_k_override[paste0("stage_B_", s)] <- as.numeric(mean_k_B)
            } else {
              stage_k_override[paste0("stage_B_", s)] <- as.numeric(mean_k_B[1, 1])
            }


            bh_dd_stages_set_qa[[countr]] <- paste0("bh_stage_b_", s)

            indx1 <- which(life_cycle_params$Name == paste0("bh_stage_b_", s))
            indx2 <- which(life_cycle_params$Name == paste0("hs_stage_b_", s))

            if (length(indx1) > 0) {
              bh_dd_stages_set[[countr]] <- paste0("bh_stage_b_", s)
            } else if (length(indx2) > 0) {
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

          if (class(mean_k)[1] == "numeric" |
              class(mean_k)[1] == "integer" |
              class(mean_k)[1] == "character") {
            total_anadromous_spawners <- as.numeric(mean_k)
          } else {
            total_anadromous_spawners <- as.numeric(mean_k[1, 1])
          }

          bh_dd_stages_set_qa[[countr]] <- paste0("bh_spawners")

          indx1 <- which(life_cycle_params$Name == paste0("bh_spawners"))
          indx2 <- which(life_cycle_params$Name == paste0("hs_spawners"))

          if (length(indx1) > 0) {
            bh_dd_stages_set[[countr]] <- paste0("bh_spawners")
          } else if (length(indx2) > 0) {
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


      if (length(any_diff1) > 0 | length(any_diff2) > 0) {
        message("Expect from life cycles table:")
        message(bh_dd_stages)
        message("Data in location K table:")
        message(bh_dd_stages_set_qa)
        message("Warning: bh_stage_... in life cycle params does not match K ages... in habitat data")
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

        # Set to 0 if missing
        cv_k <- ifelse(length(cv_k) == 0, 0, cv_k)
        cv_k <- ifelse(is.na(cv_k), 0, cv_k)


        mean_k <- ifelse(length(mean_k) == 0, NA, mean_k)

        if (!is.na(mean_k)) {
          cv_k <- ifelse(is.na(cv_k), 0, cv_k)
          mean_k <- mean_k[[1]]
          cv_k <- cv_k[[1]]
          this_k <-
            suppressWarnings({
              stats::rnorm(1,
                           mean = as.numeric(mean_k),
                           sd = as.numeric(mean_k * cv_k))
            })
          this_k <- ifelse(is.na(this_k), as.numeric(mean_k), this_k)
          stage_k_override[s + 1] <- this_k
        }

        # Set the Beverton-Holt DD mechanism (if set in life cycles file)
        if (s == 0) {
          egg_fry <- life_cycle_params$Value[life_cycle_params$Name == "dd_hs_0"]

          if (length(egg_fry) > 0) {
            if (!(is.na(egg_fry))) {
              if (egg_fry == 1 | egg_fry == "TRUE" | egg_fry == TRUE) {
                bh_dd_stages_set[[1]] <- "dd_hs_0"
              }
            }
          }

          egg_fry <- life_cycle_params$Value[life_cycle_params$Name == "bh_stage_0"]

          if (length(egg_fry) > 0) {
            if (!(is.na(egg_fry))) {
              if (egg_fry == 1 | egg_fry == "TRUE" | egg_fry == TRUE) {
                bh_dd_stages_set[[1]] <- "bh_stage_0"
              }
            }
          }


        } else {
          dd_stage <- life_histories$Value[life_cycle_params$Name == paste0("bh_stage_", s)]
          if (length(dd_stage) > 0) {
            if (!(is.na(dd_stage))) {
              if (dd_stage == 1) {
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
