#' Is a density-dependence flag switched on in the life cycles profile?
#'
#' A flag row (e.g. bh_stage_pb_1, hs_spawners) is ON only when the row
#' exists AND its Value is 1 / TRUE. Absent rows, blank/NA values and 0
#' all mean OFF. Shared by the anadromous and non-anadromous branches of
#' build_k_for_proj_dd() so both model modes read flags identically (the
#' anadromous branch previously activated flags on row PRESENCE alone, so
#' setting a flag to 0 or leaving it blank could not disable it).
#'
#' @param life_cycle_params Life cycles data frame (Name / Value columns).
#' @param flag_name Character. Flag row name to look up.
#' @noRd
#' @keywords internal
dd_flag_enabled <- function(life_cycle_params, flag_name) {
  val <- life_cycle_params$Value[life_cycle_params$Name == flag_name]
  if (length(val) == 0) {
    return(FALSE)
  }
  val <- val[1]
  if (is.na(val)) {
    return(FALSE)
  }
  # Case-insensitive: "TRUE", "true", "True" and 1 all enable the flag
  # (matching explain_dd_settings() so model and diagnostics can't diverge)
  val <- tolower(trimws(as.character(val)))
  val %in% c("1", "true")
}


#' Is any density-dependence flag enabled?
#'
#' Scans the life cycles profile for BH/HS flag rows of every spelling
#' (bh_stage_..., hs_stage_..., bh_stage_pb_..., bh_stage_b_...,
#' bh_spawners, hs_spawners, dd_hs_0) and reports whether at least one is
#' switched ON (via dd_flag_enabled - a row with Value 0/blank is
#' disabled). Used to decide whether a run is in habitat-capacity mode
#' (skip the s0 lambda-calibration) or truly in compensation-ratio mode
#' (calibrate s0 so the stated adult k is the equilibrium).
#'
#' @param life_cycle_params Life cycles data frame (Name / Value columns).
#' @noRd
#' @keywords internal
any_dd_flag_enabled <- function(life_cycle_params) {
  flag_rows <- grep("^(bh|hs)_stage_|^(bh|hs)_spawners$|^dd_hs_0$|^dd_hs_spawners$",
                    life_cycle_params$Name, value = TRUE)
  if (length(flag_rows) == 0) {
    return(FALSE)
  }
  any(vapply(flag_rows, function(f) {
    dd_flag_enabled(life_cycle_params, f)
  }, logical(1)))
}


#' Read a capacity cv cell from the habitat table
#'
#' Returns the coefficient of variation for a capacity column as a scalar,
#' or 0 when the column is absent, empty or NA (capacity treated as exact).
#' Handles both base data.frame cells and 1x1 tibble cells from readxl.
#'
#' @param hab_dd_k Single-row habitat capacity data frame (target HUC).
#' @param cv_col Character. cv column name to look up.
#' @noRd
#' @keywords internal
k_cv_value <- function(hab_dd_k, cv_col) {
  if (!(cv_col %in% colnames(hab_dd_k))) {
    return(0)
  }
  val <- hab_dd_k[1, cv_col]
  if (length(val) == 0) {
    return(0)
  }
  val <- suppressWarnings(as.numeric(val[[1]]))
  if (length(val) == 0 || is.na(val)) {
    return(0)
  }
  if (val < 0) {
    stop("Capacity cv values must be >= 0 (column ", cv_col, ")...")
  }
  val
}


#' build_stage_k_noise
#'
#' @description Generate per-year multiplicative noise factors for stage
#' capacity (K) values, from the cv columns of a habitat capacity file.
#'
#' @details Each capacity constraint with a cv > 0 gets an independent
#' draw for every projection year from a normal distribution with mean 1
#' and standard deviation cv, clamped at zero (draws can never produce a
#' negative capacity). Capacities with cv 0/NA keep a constant factor of 1
#' and consume no random numbers. Returns NULL when every cv is 0/NA, so
#' runs without capacity uncertainty are bit-identical to runs before this
#' feature existed.
#'
#' PopulationModel_Run() calls this once per batch replicate and passes the
#' same matrix to the CE and baseline projections of that replicate, so the
#' pair sees identical capacity trajectories (common random numbers). Direct
#' users of Projection_DD() can call it themselves and supply the result via
#' the stage_k_noise argument.
#'
#' @param stage_k_cv Numeric vector of cv values aligned with
#' stage_k_override: position 1 is the fry flow cap (K0), positions 2
#' onward are stage 1..Nstage. Use the ret_stage_k_cv element returned by
#' build_k_for_proj_dd(). NULL/NA entries are treated as 0.
#' @param spawner_k_cv Numeric. cv for the total spawner capacity
#' (k_stage_B_cv column); default 0.
#' @param Nyears Numeric. Number of projection years (rows of the matrix).
#'
#' @returns A matrix with Nyears rows and length(stage_k_cv) + 1 columns
#' (K0, stage 1..Nstage, total spawner capacity), or NULL when all cv
#' values are 0.
#'
#' @export
build_stage_k_noise <- function(stage_k_cv = NULL,
                                spawner_k_cv = 0,
                                Nyears = 100) {
  if (is.null(stage_k_cv)) {
    stage_k_cv <- numeric(0)
  }
  if (is.null(spawner_k_cv) || length(spawner_k_cv) == 0) {
    spawner_k_cv <- 0
  }
  cvs <- c(as.numeric(stage_k_cv), as.numeric(spawner_k_cv))
  cvs[is.na(cvs)] <- 0
  if (any(cvs < 0)) {
    stop("Capacity cv values must be >= 0...")
  }
  # No uncertainty set: return NULL and consume no random numbers
  if (all(cvs == 0)) {
    return(NULL)
  }
  noise <- matrix(1, nrow = Nyears, ncol = length(cvs))
  for (j in which(cvs > 0)) {
    noise[, j] <- pmax(0, stats::rnorm(Nyears, mean = 1, sd = cvs[j]))
  }
  noise
}


#' build_k_for_proj_dd
#' @description Build build_habitat_dd_k object and bh_dd_stages from PopulationModel_Run for Projection_DD.
#'
#' @param life_histories Object returned from pop_model_matrix_elements()$life_histories
#' @param life_cycle_params Object returned from pop_model_dat_clean() without any further editing
#' @param HUC_ID Target HUC_ID for location-based density dependence.
#' @param habitat_dd_k Data frame. Habitat carrying capacity (K) values for each HUC_ID.
#'
#' @details This function builds the habitat carrying capacity (K) vector for
#' use in density dependent projections. It extracts K values from the
#' habitat_dd_k input data frame for the target HUC_ID and formats them for
#' use in Projection_DD(). It also identifies which life stages are subject
#' to density dependence based on the life cycle parameters. The function will
#' return an empty list of there is no location-based DD.
#'
#' Density-dependence flag rows in the life cycles profile (bh_stage_...,
#' hs_stage_..., bh_spawners, hs_spawners, dd_hs_0) are switches: a flag is
#' active only when the row exists AND its Value is 1/TRUE. Setting a flag
#' to 0 or leaving it blank disables it - in both the anadromous and
#' non-anadromous branches. If both the bh_ and hs_ variants are enabled
#' for the same stage, the Beverton-Holt (bh_) form takes precedence.
#'
#' Blank or NA K cells mean "no constraint for that stage" - NOT that the
#' value is unknown or to-be-determined. Supplying a habitat row for a HUC
#' switches that location into habitat-capacity mode (fry survival s0 is not
#' recalibrated to force lambda = 1; see Projection_DD), even if every stage
#' is left unconstrained. A row with no usable K values at all triggers a
#' warning, since every stage being unconstrained is usually (but not always)
#' unintended.
#'
#' Optional k_..._cv columns in the habitat file hold the coefficient of
#' variation for the matching k_..._mean capacity estimate. The mean values
#' are returned unchanged in ret_stage_k_override (no draw happens here);
#' the cv values are returned in ret_stage_k_cv / ret_spawner_k_cv and are
#' consumed by PopulationModel_Run(), which draws stochastic capacity
#' factors per batch replicate and per year via build_stage_k_noise().
#' Missing cv columns and blank/NA cv cells mean cv = 0 (capacity treated
#' as exact; no random draws are made).
#'
#' @returns A list with the following components:
#' * ret_stage_k_override: A named numeric vector of carrying capacity (K) values
#'  for each life stage, including fry (K0) and spawners if applicable.
#'  * ret_bh_dd_stages: A character vector of life stage names that are subject to
#'  Beverton-Holt density dependence.
#'  * ret_total_anadromous_spawners: A numeric value representing the
#'  total carrying capacity for anadromous spawners, if applicable.
#'  * ret_stage_k_cv: A numeric vector of capacity cv values aligned with
#'  ret_stage_k_override (0 where no uncertainty was supplied), or NULL
#'  when there is no location-based DD.
#'  * ret_spawner_k_cv: A numeric cv value for the total spawner capacity
#'  (0 when no uncertainty was supplied).
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
  total_spawners <- NULL
  stage_k_cv <- NULL
  spawner_k_cv <- 0

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
    # Normalize all spawner capacity column name variants to canonical k_stage_B_mean
    # Order matters: match longer/more-specific patterns first
    fixnms <- gsub("k_stage_b_mean", "k_stage_B_mean", fixnms, ignore.case = TRUE)
    fixnms <- gsub("k_stage_spawners_mean", "k_stage_B_mean", fixnms, ignore.case = TRUE)
    fixnms <- gsub("k_stage_spawner_mean", "k_stage_B_mean", fixnms, ignore.case = TRUE)
    fixnms <- gsub("k_spawners_mean", "k_stage_B_mean", fixnms, ignore.case = TRUE)
    fixnms <- gsub("k_spawner_mean", "k_stage_B_mean", fixnms, ignore.case = TRUE)
    fixnms <- gsub("k_b_mean", "k_stage_B_mean", fixnms, ignore.case = TRUE)

    colnames(hab_dd_k) <- fixnms


    if (nrow(hab_dd_k) == 0) {
      avail <- unique(habitat_dd_k$HUC_ID)
      stop(
        "HUC_ID '", HUC_ID, "' has no row in the habitat capacities ",
        "(habitat_dd_k) file. Available HUC_ID values: ",
        paste(utils::head(avail, 20), collapse = ", "),
        ifelse(length(avail) > 20, ", ...", ""),
        ". Add a row for this location (blank K cells mean no constraint ",
        "for that life stage).",
        call. = FALSE
      )
    }
    if (nrow(hab_dd_k) > 1) {
      stop(
        "HUC_ID '", HUC_ID, "' has ", nrow(hab_dd_k), " rows in the ",
        "habitat capacities (habitat_dd_k) file - each location must have ",
        "exactly one row.",
        call. = FALSE
      )
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

      # Capacity cv values (uncertainty) aligned with stage_k_override
      stage_k_cv <- rep(0, length(stage_k_override))
      names(stage_k_cv) <- names(stage_k_override)


      # Need to add on the K0 fry class if present
      # (case-insensitive lookup: a mixed-case k_Stage_0_Mean column
      # previously passed the check but crashed on extraction)
      check_this <- paste0("k_stage_0_mean")
      col_match <- colnames(hab_dd_k)[tolower(colnames(hab_dd_k)) == check_this]

      if (length(col_match) == 1) {

        mean_k <- hab_dd_k[1, col_match]

        # Set fry K
        if (!is.na(mean_k)) {

          if (class(mean_k)[1] == "numeric" |
              class(mean_k)[1] == "integer" |
              class(mean_k)[1] == "character") {
            stage_k_override["K0"] <- as.numeric(mean_k)
          } else {
            stage_k_override["K0"] <- as.numeric(mean_k[1, 1])
          }

          stage_k_cv["K0"] <- k_cv_value(hab_dd_k, "k_stage_0_cv")

          bh_dd_stages_set_qa[[countr]] <- paste0("bh_stage_0")

          # Flag must exist AND be switched on (Value 1/TRUE); 0 or blank
          # disables. If both bh_ and hs_ are enabled, Beverton-Holt takes
          # precedence. dd_hs_0 is accepted as an alias of hs_stage_0
          # (matching the non-anadromous branch).
          if (dd_flag_enabled(life_cycle_params, "bh_stage_0")) {
            bh_dd_stages_set[[countr]] <- paste0("bh_stage_0")
          } else if (dd_flag_enabled(life_cycle_params, "hs_stage_0") ||
                     dd_flag_enabled(life_cycle_params, "dd_hs_0")) {
            bh_dd_stages_set[[countr]] <- paste0("hs_stage_0")
          }

          countr <- 1 + countr
        }
      }


      # Ensure that stages are represented
      # resample habitat k values
      # NOTE: spawner-age K columns (k_stage_B_s_mean) are only checked for
      # s up to Nstage_Pb. A spawner age of Nstage_Pb + 1 is possible in
      # principle (fish maturing out of the last pre-breeder stage), but
      # standard life cycle files pad surv_ rows up to the maximum age so
      # spawning ages are covered in practice.
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
            stage_k_cv[paste0("stage_Pb_", s)] <-
              k_cv_value(hab_dd_k, paste0("k_stage_Pb_", s, "_cv"))

            # Set BH set
            bh_dd_stages_set_qa[[countr]] <- paste0("bh_stage_pb_", s)

            # Flag must be switched on (Value 1/TRUE); 0 or blank disables
            if (dd_flag_enabled(life_cycle_params, paste0("bh_stage_pb_", s))) {
              bh_dd_stages_set[[countr]] <- paste0("bh_stage_pb_", s)
            } else if (dd_flag_enabled(life_cycle_params, paste0("hs_stage_pb_", s))) {
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
          # (check mean_k_B itself - scalar from a base data.frame vs a
          # 1x1 tibble cell from readxl - mirroring the fry/Pb blocks.
          # Previously this inspected unrelated variables and crashed with
          # "object 'mean_k' not found" when no k_stage_0_mean column
          # was present.)
          if (!is.na(mean_k_B)) {
            if (class(mean_k_B)[1] == "numeric" |
                class(mean_k_B)[1] == "integer" |
                class(mean_k_B)[1] == "character") {
              stage_k_override[paste0("stage_B_", s)] <- as.numeric(mean_k_B)
            } else {
              stage_k_override[paste0("stage_B_", s)] <- as.numeric(mean_k_B[1, 1])
            }

            stage_k_cv[paste0("stage_B_", s)] <-
              k_cv_value(hab_dd_k, paste0("k_stage_B_", s, "_cv"))

            bh_dd_stages_set_qa[[countr]] <- paste0("bh_stage_b_", s)

            # Flag must be switched on (Value 1/TRUE); 0 or blank disables
            if (dd_flag_enabled(life_cycle_params, paste0("bh_stage_b_", s))) {
              bh_dd_stages_set[[countr]] <- paste0("bh_stage_b_", s)
            } else if (dd_flag_enabled(life_cycle_params, paste0("hs_stage_b_", s))) {
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

          spawner_k_cv <- k_cv_value(hab_dd_k, "k_stage_B_cv")

          bh_dd_stages_set_qa[[countr]] <- paste0("bh_spawners")

          # Flag must be switched on (Value 1/TRUE); 0 or blank disables
          if (dd_flag_enabled(life_cycle_params, "bh_spawners")) {
            bh_dd_stages_set[[countr]] <- paste0("bh_spawners")
          } else if (dd_flag_enabled(life_cycle_params, "hs_spawners") ||
                     dd_flag_enabled(life_cycle_params, "dd_hs_spawners")) {
            # dd_hs_spawners is accepted as an alias of hs_spawners
            # (mirroring the dd_hs_0 alias of hs_stage_0)
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
        warning(
          "Density-dependence flags and habitat K values do not line up ",
          "for this location. ",
          if (length(any_diff1) > 0) {
            paste0(
              "K values supplied with no enabled flag (IGNORED - growth ",
              "unconstrained at these stages): ",
              paste(any_diff1, collapse = ", "), ". "
            )
          } else "",
          if (length(any_diff2) > 0) {
            paste0(
              "Enabled flags with no matching K value (no constraint ",
              "applied): ",
              paste(any_diff2, collapse = ", "), ". "
            )
          } else "",
          "Align the bh_/hs_ flags in the life cycles file with the ",
          "k_... columns in the habitat capacities file.",
          call. = FALSE
        )
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

      # Capacity cv values (uncertainty) aligned with stage_k_override
      stage_k_cv <- rep(0, length(stage_k_override))
      names(stage_k_cv) <- names(stage_k_override)

      # Track which stages have an enabled DD flag so K values supplied
      # without a flag can be flagged as ignored (see warning below)
      has_flag <- rep(FALSE, life_histories$Nstage + 1)

      # Ensure that stages are represented
      for (s in 0:(life_histories$Nstage)) {

        # Capacity mean and cv for the stage. The mean is used directly:
        # no random draw happens here. Stochastic capacity draws are made
        # per batch replicate and per year in PopulationModel_Run() via
        # build_stage_k_noise() (previously a single unclamped draw was
        # made here, shared by every replicate).
        mean_col <- paste0("k_stage_", s, "_mean")
        mean_k <- if (mean_col %in% colnames(hab_dd_k)) hab_dd_k[1, mean_col] else NA

        if (length(mean_k) == 0) {
          mean_k <- NA
        }

        if (!is.na(mean_k)) {
          stage_k_override[s + 1] <- as.numeric(mean_k[[1]])
          stage_k_cv[s + 1] <-
            k_cv_value(hab_dd_k, paste0("k_stage_", s, "_cv"))
        }

        # Set the Beverton-Holt DD mechanism (if set in life cycles file)
        # Flag must exist AND be switched on (Value 1/TRUE); 0 or blank
        # disables. If both bh_ and hs_ flags are enabled for one stage,
        # Beverton-Holt takes precedence (dd_hs_0 is an alias of
        # hs_stage_0).
        if (s == 0) {
          if (dd_flag_enabled(life_cycle_params, "bh_stage_0")) {
            bh_dd_stages_set[[1]] <- "bh_stage_0"
          } else if (dd_flag_enabled(life_cycle_params, "hs_stage_0")) {
            bh_dd_stages_set[[1]] <- "hs_stage_0"
          } else if (dd_flag_enabled(life_cycle_params, "dd_hs_0")) {
            bh_dd_stages_set[[1]] <- "dd_hs_0"
          }
          has_flag[1] <- length(bh_dd_stages_set) >= 1 &&
            !is.null(bh_dd_stages_set[[1]])
        } else {
          if (dd_flag_enabled(life_cycle_params, paste0("bh_stage_", s))) {
            bh_dd_stages_set[[1 + s]] <- paste0("bh_stage_", s)
            has_flag[1 + s] <- TRUE
          } else if (dd_flag_enabled(life_cycle_params, paste0("hs_stage_", s))) {
            bh_dd_stages_set[[1 + s]] <- paste0("hs_stage_", s)
            has_flag[1 + s] <- TRUE
          }
        }
      }

      # Warn when the habitat row supplies usable K values for stages
      # that have NO enabled bh_/hs_ flag: those capacities are silently
      # ignored, and because the habitat row places this location in
      # habitat-capacity mode (s0 is not re-calibrated), growth at the
      # unflagged stages is unconstrained.
      orphan_k <- which(!is.na(stage_k_override) & !has_flag) - 1
      if (length(orphan_k) > 0) {
        warning(
          "Habitat capacities are supplied for stage(s) ",
          paste(orphan_k, collapse = ", "),
          " (k_stage_X_mean columns) but no bh_/hs_ density-dependence ",
          "flag is enabled for them in the life cycles file. These K ",
          "values will be IGNORED and growth at these stages is ",
          "unconstrained. Enable the matching bh_stage_X / hs_stage_X ",
          "flags, or remove the K values.",
          call. = FALSE
        )
      }

      # ---------------------------------------------------------
      # Spawner capacity for non-anadromous populations
      # Check for k_stage_B_mean column (total spawner cap)
      # ---------------------------------------------------------
      check_spawner <- "k_stage_B_mean"
      if (check_spawner %in% colnames(hab_dd_k)) {
        mean_k <- hab_dd_k[1, check_spawner]
        if (!is.na(mean_k)) {
          if (class(mean_k)[1] == "numeric" |
              class(mean_k)[1] == "integer" |
              class(mean_k)[1] == "character") {
            total_spawners <- as.numeric(mean_k)
          } else {
            total_spawners <- as.numeric(mean_k[1, 1])
          }
          total_anadromous_spawners <- total_spawners

          spawner_k_cv <- k_cv_value(hab_dd_k, "k_stage_B_cv")

          # Check for bh_spawners or hs_spawners flag. Flag must be
          # switched on; a present-but-disabled bh_spawners row no longer
          # shadows an enabled hs_spawners row. dd_hs_spawners is an
          # accepted alias of hs_spawners.
          if (dd_flag_enabled(life_cycle_params, "bh_spawners")) {
            bh_dd_stages_set[[length(bh_dd_stages_set) + 1]] <- "bh_spawners"
          } else if (dd_flag_enabled(life_cycle_params, "hs_spawners") ||
                     dd_flag_enabled(life_cycle_params, "dd_hs_spawners")) {
            bh_dd_stages_set[[length(bh_dd_stages_set) + 1]] <- "hs_spawners"
          } else {
            warning(
              "A total spawner capacity (k_stage_B_mean / k_spawners_mean) ",
              "is supplied in the habitat file but neither bh_spawners nor ",
              "hs_spawners is enabled in the life cycles file - the ",
              "spawner capacity will be IGNORED.",
              call. = FALSE
            )
          }
        }
      }

      # Then specify which parameters to use
      bh_dd_stages <- unlist(bh_dd_stages_set)

    } # end of non-anadrmous populations







  } # end of is.null(habitat_dd_k) == FALSE


  # NA values in stage_k_override mean "no constraint for that stage" (by
  # design), and supplying any habitat row places the location in
  # habitat-capacity mode (Projection_DD will not recalibrate s0 to force
  # lambda = 1). Behaviour is unchanged for an all-NA vector, but warn:
  # a row where every K cell is blank leaves every stage unconstrained,
  # which is usually a data-entry oversight.
  if (!is.null(stage_k_override) &&
      all(is.na(stage_k_override)) &&
      is.null(total_anadromous_spawners) &&
      length(bh_dd_stages) == 0) {
    warning(
      "habitat_dd_k has a row for HUC_ID ", HUC_ID,
      " but every K value is blank/NA: all stages are unconstrained. ",
      "The location still runs in habitat-capacity mode (fry survival s0 ",
      "is not recalibrated to force lambda = 1). If you intended ",
      "compensation-ratio mode for this location, remove its row from the ",
      "habitat capacity file.",
      call. = FALSE
    )
  }


  ret_obj <- list()
  ret_obj$ret_stage_k_override <- stage_k_override
  ret_obj$ret_bh_dd_stages <- bh_dd_stages
  # Backward-compatible alias
  ret_obj$ret_total_anadromous_spawners <- total_anadromous_spawners
  ret_obj$ret_total_spawners <- total_anadromous_spawners
  # Capacity uncertainty (cv) values; consumed by build_stage_k_noise()
  ret_obj$ret_stage_k_cv <- stage_k_cv
  ret_obj$ret_spawner_k_cv <- spawner_k_cv


  return(ret_obj)


}
