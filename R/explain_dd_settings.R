#' Explain Density Dependence Settings
#'
#' @description
#' A diagnostic function that takes life cycle parameters and optional habitat
#' capacity data and generates a human-readable explanation of what density
#' dependence will be applied in the population model.
#'
#' This function helps users understand:
#' \itemize{
#'   \item Whether s0.1.det (fry survival) will be optimized or use original values
#'   \item Which compensation ratio stages have active density dependence (cr > 1)
#'   \item Which Beverton-Holt or Hockey-stick stages are enabled
#'   \item What carrying capacity (K) values will be used for each life stage
#'   \item Any potential configuration warnings
#' }
#'
#' @param life_cycles A data frame with life cycle parameters. Must have columns
#'   \code{Name} and \code{Value} (case-insensitive). This is the same format as
#'   used by \code{\link{pop_model_setup}}.
#' @param habitat_dd_k Optional data frame with habitat capacity values by location.
#'   Should have a \code{HUC_ID} column and columns like \code{k_stage_0_mean},
#'   \code{k_stage_1_mean}, etc. This is the same format as used by
#'   \code{\link{build_k_for_proj_dd}}.
#' @param HUC_ID Character. The location identifier to look up in \code{habitat_dd_k}.
#'   Required if \code{habitat_dd_k} is provided.
#' @param verbose Logical. If \code{TRUE} (default), prints the explanation to
#'   the console. Set to \code{FALSE} to suppress output and only return the list.
#'
#' @return Invisibly returns a list with the following components:
#' \describe{
#'   \item{explanation}{Character string with the full human-readable explanation}
#'   \item{summary}{List with population_type, n_stages, adult_k, dd_methods, s0_adjusted}
#'   \item{s0_details}{List with original_s0, s0_1_det, behavior ("ORIGINAL" or "OPTIMIZED"),
#'     reason, and note (explains why s0 values differ, or NULL if unchanged)}
#'   \item{compensation_ratios}{List with enabled, values, stages_with_effect}
#'   \item{bh_dd_stages}{List with enabled, all_stages, beverton_holt, hockey_stick,
#'     and note (identifies missing K columns or stable-stage-derived K used with
#'     location-specific DD, or NULL if no issues)}
#'   \item{k_values}{List with stages (named list of K values by stage, each with
#'     value and source) and note (identifies K columns without corresponding DD
#'     flags, or NULL if all K columns are used)}
#'   \item{warnings}{Character vector of any configuration warnings}
#' }
#'
#' @details
#' The function analyzes the life cycle parameters to determine:
#'
#' \strong{s0.1.det Optimization:}
#' \itemize{
#'   \item OPTIMIZED: When \code{k} is set AND no BH/HS stages are enabled.
#'     The fry survival rate is adjusted to give lambda = 1.0 at equilibrium.
#'   \item ORIGINAL: When \code{k} is NA, OR when any BH/HS stage is enabled.
#'     The original S0 value from the life cycle file is used.
#' }
#'
#' \strong{Compensation Ratio DD:}
#' Applied via the D.mx matrix during projection when \code{k} is set.
#' Only stages with cr > 1 have active density dependence.
#'
#' \strong{Beverton-Holt / Hockey-Stick DD:}
#' Applied in \code{dd.N.bh()} after matrix projection when any of these
#' parameters are set to TRUE/1: bh_stage_0, bh_stage_1, hs_stage_0, dd_hs_0,
#' bh_spawners, hs_spawners, etc.
#'
#' @seealso
#' \code{\link{pop_model_setup}} for setting up population model parameters
#' \code{\link{pop_model_matrix_elements}} for building the projection matrix
#' \code{\link{build_k_for_proj_dd}} for building K values from habitat data
#' \code{\link{Projection_DD}} for running the population projection
#'
#' @examples
#' # Create a simple resident life cycle with compensation ratios
#' life_cycles <- data.frame(
#'   Name = c("Nstage", "anadromous", "k", "S0", "SE",
#'            "surv_1", "surv_2", "surv_3", "surv_4",
#'            "cr_E", "cr_0", "cr_1", "cr_2", "cr_3", "cr_4",
#'            "mat_1", "mat_2", "mat_3", "mat_4",
#'            "year_1", "year_2", "year_3", "year_4",
#'            "events", "eps", "int", "SR",
#'            "eps_sd", "egg_rho", "M.cv", "M.rho"),
#'   Value = c(4, FALSE, 500, 0.3, 0.1,
#'             0.5, 0.6, 0.8, 0.9,
#'             1, 3.5, 2.8, 2.0, 1.5, 1,
#'             0, 0, 0, 1,
#'             1, 1, 1, 1,
#'             1, 3000, 1, 0.5,
#'             100, 0.1, 0.1, 0.1),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Explain what DD will be applied
#' result <- explain_dd_settings(life_cycles)
#'
#' # Access the structured results
#' result$summary$dd_methods
#' result$s0_details$behavior
#' result$compensation_ratios$stages_with_effect
#'
#' # With habitat capacity data
#' habitat_k <- data.frame(
#'   HUC_ID = c("1", "2", "3"),
#'   k_stage_0_mean = c(10000, 20000, 15000),
#'   k_stage_1_mean = c(5000, 10000, 7500)
#' )
#'
#' # Add BH stage to life cycle
#' life_cycles_bh <- rbind(life_cycles,
#'   data.frame(Name = "bh_stage_0", Value = "TRUE", stringsAsFactors = FALSE)
#' )
#'
#' result2 <- explain_dd_settings(
#'   life_cycles = life_cycles_bh,
#'   habitat_dd_k = habitat_k,
#'   HUC_ID = "1"
#' )
#'
#' @export
explain_dd_settings <- function(life_cycles,
                                habitat_dd_k = NULL,
                                HUC_ID = NULL,
                                verbose = TRUE) {


  # ---------------------------------------------------------------------------

# Input validation
# ---------------------------------------------------------------------------

if (!is.data.frame(life_cycles)) {
    stop("life_cycles must be a data frame with 'Name' and 'Value' columns")
  }

  # Normalize column names to lowercase for matching
  lc_names <- tolower(names(life_cycles))
  names(life_cycles) <- lc_names

  # Check for required columns
  if (!("name" %in% lc_names && "value" %in% lc_names)) {
    stop("life_cycles must have 'Name' and 'Value' columns")
  }

  # ---------------------------------------------------------------------------
  # Parse life cycle parameters
  # ---------------------------------------------------------------------------

  # Create named vector of parameters
  params <- stats::setNames(as.character(life_cycles$value), life_cycles$name)

  # Extract key parameters
  k_adult <- suppressWarnings(as.numeric(params["k"]))
  Nstage <- as.numeric(params["Nstage"])
  S0 <- as.numeric(params["S0"])
  SE <- as.numeric(params["SE"])

  # Check if anadromous
  anadromous_val <- params["anadromous"]
  anadromous <- !is.null(anadromous_val) &&
                !is.na(anadromous_val) &&
                tolower(as.character(anadromous_val)) %in% c("true", "1")

  # ---------------------------------------------------------------------------
  # Extract compensation ratios
  # ---------------------------------------------------------------------------

  cr_pattern <- "^cr_"
  cr_names <- grep(cr_pattern, names(params), value = TRUE)
  cr_values <- suppressWarnings(as.numeric(params[cr_names]))
  names(cr_values) <- cr_names

  # Which have effect (cr > 1)?
  cr_with_effect <- cr_names[!is.na(cr_values) & cr_values > 1]
  cr_enabled <- length(cr_with_effect) > 0

  # ---------------------------------------------------------------------------
  # Extract BH/HS stage flags
  # ---------------------------------------------------------------------------

  bh_hs_pattern <- "^(bh_stage|hs_stage|dd_hs|bh_spawners|hs_spawners)"
  bh_hs_names <- grep(bh_hs_pattern, names(params), value = TRUE, ignore.case = TRUE)
  bh_hs_values <- params[bh_hs_names]

  # Helper to check if a value indicates "enabled"
  is_enabled <- function(val) {
    if (is.null(val) || is.na(val)) return(FALSE)
    val <- tolower(as.character(val))
    return(val %in% c("true", "1"))
  }

  active_bh_dd <- character(0)
  for (i in seq_along(bh_hs_names)) {
    if (is_enabled(bh_hs_values[i])) {
      active_bh_dd <- c(active_bh_dd, bh_hs_names[i])
    }
  }

  # Classify by type
  beverton_holt_stages <- grep("^bh_", active_bh_dd, value = TRUE)
  hockey_stick_stages <- grep("^(hs_|dd_hs)", active_bh_dd, value = TRUE)
  bh_dd_enabled <- length(active_bh_dd) > 0

  # ---------------------------------------------------------------------------
  # Determine s0.1.det behavior
  # ---------------------------------------------------------------------------

  if (is.na(k_adult)) {
    s0_behavior <- "ORIGINAL"
    s0_reason <- "Adult capacity (k) is NA - density-independent growth assumed"
    s0_1_det <- S0
  } else if (bh_dd_enabled) {
    s0_behavior <- "ORIGINAL"
    s0_reason <- "BH/HS stages are defined - s0 optimization skipped"
    s0_1_det <- S0
  } else {
    s0_behavior <- "OPTIMIZED"
    s0_reason <- "s0.1.det will be optimized to give lambda = 1.0 at equilibrium"
    s0_1_det <- NA
  }

  # ---------------------------------------------------------------------------
  # Generate s0_note explaining why s0 values differ (or NULL if same)
  # ---------------------------------------------------------------------------

  s0_note <- NULL
  if (s0_behavior == "OPTIMIZED") {
    s0_note <- paste0(
      "The original fry survival (S0 = ", S0, ") will be replaced with an ",
      "optimized value (s0.1.det) calculated to achieve population equilibrium ",
      "(lambda = 1.0) when the population is at carrying capacity. This ensures ",
      "the population stabilizes at K under deterministic conditions with ",
      "compensation ratio density dependence."
    )
  }
  # If s0_behavior == "ORIGINAL", s0_note remains NULL since values are the same

  # ---------------------------------------------------------------------------
  # Parse habitat capacity data (if provided)
  # ---------------------------------------------------------------------------

  habitat_k_values <- list()
  hab_warnings <- character(0)

  if (!is.null(habitat_dd_k)) {
    if (!is.data.frame(habitat_dd_k)) {
      stop("habitat_dd_k must be a data frame")
    }

    if (is.null(HUC_ID)) {
      hab_warnings <- c(hab_warnings, "habitat_dd_k provided but HUC_ID is NULL")
    } else {
      # Normalize column names
      hab_names <- tolower(names(habitat_dd_k))
      names(habitat_dd_k) <- hab_names

      # Find HUC_ID column
      huc_col <- grep("^huc_id$|^huc$|^id$", hab_names, value = TRUE)[1]

      if (is.na(huc_col)) {
        hab_warnings <- c(hab_warnings, "No HUC_ID column found in habitat_dd_k")
      } else {
        # Convert to character for matching
        habitat_dd_k[[huc_col]] <- as.character(habitat_dd_k[[huc_col]])
        HUC_ID <- as.character(HUC_ID)

        row_idx <- which(habitat_dd_k[[huc_col]] == HUC_ID)

        if (length(row_idx) == 0) {
          hab_warnings <- c(hab_warnings,
                            paste("HUC_ID", HUC_ID, "not found in habitat_dd_k"))
        } else if (length(row_idx) > 1) {
          hab_warnings <- c(hab_warnings,
                            paste("Multiple rows for HUC_ID", HUC_ID, "- using first"))
          row_idx <- row_idx[1]
        }

        if (length(row_idx) == 1) {
          hab_row <- habitat_dd_k[row_idx, ]

          # Extract K values by pattern matching
          k_cols <- grep("^k_", names(hab_row), value = TRUE)

          for (col in k_cols) {
            val <- hab_row[[col]]
            # Handle comma-formatted numbers
            if (is.character(val)) {
              val <- as.numeric(gsub(",", "", val))
            }
            habitat_k_values[[col]] <- val
          }
        }
      }
    }
  }

  # ---------------------------------------------------------------------------
  # Determine final K values for each stage
  # ---------------------------------------------------------------------------

  k_summary <- list()

  # K0 (fry capacity)
  if ("k_stage_0_mean" %in% names(habitat_k_values)) {
    k_summary$K0 <- list(
      value = habitat_k_values$k_stage_0_mean,
      source = "habitat_dd_k"
    )
  } else if (!is.na(k_adult)) {
    k_summary$K0 <- list(
      value = "Derived via stable stage distribution",
      source = "stable_stage_from_adult_k"
    )
  } else {
    k_summary$K0 <- list(value = NA, source = "not_applicable")
  }

  # Stage-specific K values
  for (stage in seq_len(Nstage)) {
    col_name <- paste0("k_stage_", stage, "_mean")
    k_name <- paste0("K", stage)

    if (col_name %in% names(habitat_k_values)) {
      k_summary[[k_name]] <- list(
        value = habitat_k_values[[col_name]],
        source = "habitat_dd_k"
      )
    } else if (!is.na(k_adult)) {
      k_summary[[k_name]] <- list(
        value = "Derived via stable stage distribution",
        source = "stable_stage_from_adult_k"
      )
    } else {
      k_summary[[k_name]] <- list(value = NA, source = "not_applicable")
    }
  }

  # Anadromous-specific K values
  if (anadromous) {
    # Pre-spawner K values (Pb stages)
    pb_cols <- grep("k_stage_pb", names(habitat_k_values), value = TRUE)
    for (col in pb_cols) {
      k_summary[[col]] <- list(
        value = habitat_k_values[[col]],
        source = "habitat_dd_k"
      )
    }

    # Spawner K values (B stages)
    b_cols <- grep("k_stage_b", names(habitat_k_values), value = TRUE)
    for (col in b_cols) {
      k_summary[[col]] <- list(
        value = habitat_k_values[[col]],
        source = "habitat_dd_k"
      )
    }

    # Total spawner capacity
    spawner_cols <- grep("k_spawner|k_stage_b_mean$", names(habitat_k_values),
                         value = TRUE)
    if (length(spawner_cols) > 0) {
      k_summary$K_spawners <- list(
        value = habitat_k_values[[spawner_cols[1]]],
        source = "habitat_dd_k"
      )
    }
  }

  # ---------------------------------------------------------------------------
  # Check for mismatches between DD stages and K columns
  # ---------------------------------------------------------------------------

  # Helper function: map DD stage name to expected K column name(s)
  # Returns multiple variations to handle different naming conventions
  dd_stage_to_k_col <- function(stage_name) {
    stage_name <- tolower(stage_name)
    if (stage_name == "bh_stage_0" || stage_name == "hs_stage_0" || stage_name == "dd_hs_0") {
      return(c("k_stage_0_mean", "k_0_mean", "k_fry_mean"))
    } else if (grepl("^(bh_stage_|hs_stage_)([0-9]+)$", stage_name)) {
      num <- gsub("^(bh_stage_|hs_stage_)", "", stage_name)
      return(c(paste0("k_stage_", num, "_mean"), paste0("k_", num, "_mean")))
    } else if (grepl("^(bh_stage_|hs_stage_)pb_([0-9]+)$", stage_name)) {
      num <- gsub("^(bh_stage_|hs_stage_)pb_", "", stage_name)
      return(c(paste0("k_stage_pb_", num, "_mean"),
               paste0("k_pb_", num, "_mean"),
               paste0("k_stage_Pb_", num, "_mean")))
    } else if (grepl("^(bh_stage_|hs_stage_)b_([0-9]+)$", stage_name)) {
      num <- gsub("^(bh_stage_|hs_stage_)b_", "", stage_name)
      return(c(paste0("k_stage_b_", num, "_mean"),
               paste0("k_b_", num, "_mean"),
               paste0("k_stage_B_", num, "_mean")))
    } else if (stage_name == "bh_spawners" || stage_name == "hs_spawners") {
      return(c("k_stage_b_mean", "k_spawners_mean", "k_spawner_mean",
               "k_stage_B_mean", "k_B_mean"))
    }
    return(NULL)
  }

  # Helper function: map K column name to expected DD stage name(s)
  k_col_to_dd_stage <- function(col_name) {
    col_name <- tolower(col_name)
    if (col_name == "k_stage_0_mean") {
      return(c("bh_stage_0", "hs_stage_0", "dd_hs_0"))
    } else if (grepl("^k_stage_([0-9]+)_mean$", col_name)) {
      num <- gsub("^k_stage_|_mean$", "", col_name)
      return(c(paste0("bh_stage_", num), paste0("hs_stage_", num)))
    } else if (grepl("^k_stage_pb_([0-9]+)_mean$", col_name)) {
      num <- gsub("^k_stage_pb_|_mean$", "", col_name)
      return(c(paste0("bh_stage_pb_", num), paste0("hs_stage_pb_", num)))
    } else if (grepl("^k_stage_b_([0-9]+)_mean$", col_name)) {
      num <- gsub("^k_stage_b_|_mean$", "", col_name)
      return(c(paste0("bh_stage_b_", num), paste0("hs_stage_b_", num)))
    } else if (col_name %in% c("k_stage_b_mean", "k_spawners_mean", "k_spawner_mean")) {
      return(c("bh_spawners", "hs_spawners"))
    }
    return(NULL)
  }

  # Check 1: DD stages without matching K columns in habitat_dd_k
  bh_dd_notes <- character(0)

  if (bh_dd_enabled && !is.null(habitat_dd_k)) {
    habitat_k_cols_lower <- tolower(names(habitat_k_values))

    for (stage in active_bh_dd) {
      expected_k_cols <- dd_stage_to_k_col(stage)
      if (!is.null(expected_k_cols)) {
        # Check if any of the expected columns exist
        found <- any(tolower(expected_k_cols) %in% habitat_k_cols_lower)
        if (!found) {
          # Suggest the primary expected column name
          primary_col <- expected_k_cols[1]
          bh_dd_notes <- c(bh_dd_notes, paste0(
            "Density-dependent bottleneck specified for stage '", stage,
            "' in life cycle file, but the corresponding habitat capacity K column ",
            "is missing from the habitat capacities file. Please create a column named '",
            primary_col, "' in the habitat capacities file."
          ))
        }
      }
    }
  }

  # Check 2: K values derived from stable_stage_from_adult_k but used with BH/HS stages
  if (bh_dd_enabled) {
    for (stage in active_bh_dd) {
      stage_lower <- tolower(stage)
      expected_k_cols <- dd_stage_to_k_col(stage_lower)
      if (!is.null(expected_k_cols)) {
        # Find the corresponding K summary entry
        # Map stage to K summary key
        if (stage_lower %in% c("bh_stage_0", "hs_stage_0", "dd_hs_0")) {
          k_key <- "K0"
        } else if (grepl("^(bh_stage_|hs_stage_)([0-9]+)$", stage_lower)) {
          num <- gsub("^(bh_stage_|hs_stage_)", "", stage_lower)
          k_key <- paste0("K", num)
        } else if (grepl("^(bh_stage_|hs_stage_)pb_([0-9]+)$", stage_lower)) {
          # Pre-breeder stages - check if K is from habitat data
          k_key <- paste0("k_stage_pb_", gsub("^(bh_stage_|hs_stage_)pb_", "", stage_lower), "_mean")
        } else if (grepl("^(bh_stage_|hs_stage_)b_([0-9]+)$", stage_lower)) {
          # Breeder stages - check if K is from habitat data
          k_key <- paste0("k_stage_b_", gsub("^(bh_stage_|hs_stage_)b_", "", stage_lower), "_mean")
        } else if (stage_lower %in% c("bh_spawners", "hs_spawners")) {
          k_key <- "K_spawners"
        } else {
          k_key <- NULL
        }

        # Check if K is in k_summary with stable_stage source
        if (!is.null(k_key) && k_key %in% names(k_summary)) {
          if (k_summary[[k_key]]$source == "stable_stage_from_adult_k") {
            bh_dd_notes <- c(bh_dd_notes, paste0(
              "Stage '", stage, "' uses Beverton-Holt or Hockey-Stick density dependence, ",
              "but the carrying capacity (", k_key, ") is derived from the adult capacity (k) ",
              "via stable stage distribution rather than from location-specific habitat data. ",
              "For location-specific density dependence, provide explicit K values in the ",
              "habitat capacities file."
            ))
          }
        }
        # Also check if the expected K cols were found in habitat data
        # (for pb/b stages that use column name as key)
        if (!is.null(k_key) && grepl("^k_stage_", k_key)) {
          if (k_key %in% names(k_summary) && k_summary[[k_key]]$source == "habitat_dd_k") {
            # K was found - don't add warning
          } else if (!is.null(habitat_dd_k) && !any(tolower(expected_k_cols) %in% tolower(names(habitat_k_values)))) {
            # K not found in habitat data - this is already caught by Check 1
          }
        }
      }
    }
  }

  # Consolidate bh_dd_notes
  bh_dd_stages_note <- if (length(bh_dd_notes) > 0) {
    paste(bh_dd_notes, collapse = " ")
  } else {
    NULL
  }

  # Check 3: K columns in habitat_dd_k without corresponding DD flags in life_cycles
  k_values_notes <- character(0)

  if (!is.null(habitat_dd_k) && length(habitat_k_values) > 0) {
    # Get all K columns that could have DD stages (exclude non-stage columns)
    k_stage_cols <- grep("^k_stage_|^k_spawner", names(habitat_k_values),
                         value = TRUE, ignore.case = TRUE)

    unused_k_cols <- character(0)

    for (col in k_stage_cols) {
      expected_dd_stages <- k_col_to_dd_stage(col)
      if (!is.null(expected_dd_stages)) {
        # Check if any of the expected DD stages are active
        found <- any(tolower(expected_dd_stages) %in% tolower(active_bh_dd))
        if (!found) {
          unused_k_cols <- c(unused_k_cols, col)
        }
      }
    }

    if (length(unused_k_cols) > 0) {
      # Generate suggested DD stage names for unused columns
      suggested_dd <- character(0)
      for (col in unused_k_cols) {
        dd_stages <- k_col_to_dd_stage(col)
        if (!is.null(dd_stages)) {
          suggested_dd <- c(suggested_dd, dd_stages[1])  # Use first suggestion
        }
      }

      k_values_notes <- c(k_values_notes, paste0(
        "Habitat capacities specified for ",
        paste(paste0("'", unused_k_cols, "'"), collapse = " and "),
        ", but there are no corresponding density-dependent bottlenecks for those stages ",
        "defined in the life cycle input file. Either add stage-specific density-dependent ",
        "bottlenecks to the life cycle file (e.g., ",
        paste(paste0("'", unique(suggested_dd), "'"), collapse = ", "),
        ") or remove those columns from the habitat capacities file if they are not needed."
      ))
    }
  }

  # Consolidate k_values_notes
  k_values_note <- if (length(k_values_notes) > 0) {
    paste(k_values_notes, collapse = " ")
  } else {
    NULL
  }

  # ---------------------------------------------------------------------------
  # Generate explanation text
  # ---------------------------------------------------------------------------

  lines <- character(0)

  # Header
  lines <- c(lines,
    "============================================================",
    "       DENSITY DEPENDENCE CONFIGURATION SUMMARY",
    "============================================================",
    ""
  )

  # Population type
  pop_type <- ifelse(anadromous, "Anadromous (salmon-like)", "Resident")
  lines <- c(lines,
    paste0("POPULATION TYPE: ", pop_type),
    paste0("NUMBER OF STAGES: ", Nstage),
    paste0("ADULT CAPACITY (k): ", ifelse(is.na(k_adult), "NA (not set)", k_adult)),
    ""
  )

  # S0 adjustment section
  lines <- c(lines,
    "------------------------------------------------------------",
    "1. FRY SURVIVAL (s0.1.det) ADJUSTMENT",
    "------------------------------------------------------------",
    paste0("   Original S0 value: ", S0),
    paste0("   s0.1.det will be: ", s0_behavior),
    paste0("   Reason: ", s0_reason),
    ""
  )

  if (s0_behavior == "OPTIMIZED") {
    lines <- c(lines,
      "   Note: The actual s0.1.det value is calculated by finding the S0",
      "   that makes the dominant eigenvalue (lambda) = 1.0 when the",
      "   population is at carrying capacity.",
      ""
    )
  }

  # Compensation ratio section
  lines <- c(lines,
    "------------------------------------------------------------",
    "2. COMPENSATION RATIO DENSITY DEPENDENCE",
    "------------------------------------------------------------"
  )

  if (is.na(k_adult)) {
    lines <- c(lines,
      "   Status: DISABLED (k is NA)",
      "   Compensation ratios have no effect without adult capacity.",
      ""
    )
  } else {
    lines <- c(lines,
      "   Status: ENABLED (applied via D.mx matrix during projection)",
      "   Applied when: Each time step, survival rates are multiplied by",
      "                 density-dependent factors based on N/K ratio.",
      "",
      "   Compensation ratios by stage:"
    )

    for (i in seq_along(cr_values)) {
      cr_name <- names(cr_values)[i]
      cr_val <- cr_values[i]
      effect <- ifelse(is.na(cr_val) || cr_val == 1,
                       "(no DD effect)", "(DD effect active)")
      lines <- c(lines, paste0("     - ", cr_name, ": ", cr_val, " ", effect))
    }

    if (cr_enabled) {
      lines <- c(lines,
        "",
        paste0("   Stages with active DD: ", paste(cr_with_effect, collapse = ", "))
      )
    } else {
      lines <- c(lines,
        "",
        "   Note: All compensation ratios = 1, so no CR-based DD will occur."
      )
    }
    lines <- c(lines, "")
  }

  # BH/HS section
  lines <- c(lines,
    "------------------------------------------------------------",
    "3. BEVERTON-HOLT / HOCKEY-STICK DENSITY DEPENDENCE",
    "------------------------------------------------------------"
  )

  if (!bh_dd_enabled) {
    lines <- c(lines,
      "   Status: DISABLED (no bh_stage_X or hs_stage_X flags set to TRUE)",
      "",
      "   To enable, set any of these to TRUE/1 in life_cycles:",
      "     - bh_stage_0, bh_stage_1, ... (Beverton-Holt)",
      "     - hs_stage_0, dd_hs_0, ... (Hockey-stick)",
      "     - bh_spawners, hs_spawners (spawner constraints)",
      ""
    )
  } else {
    lines <- c(lines,
      "   Status: ENABLED (applied in dd.N.bh() AFTER matrix projection)",
      "",
      "   Active BH/HS stages:"
    )

    for (stage in active_bh_dd) {
      stage_type <- ifelse(grepl("^bh_", stage), "Beverton-Holt", "Hockey-Stick")
      lines <- c(lines, paste0("     - ", stage, " (", stage_type, ")"))
    }

    lines <- c(lines, "", "   What dd.N.bh() will do after each projection step:")

    for (stage in active_bh_dd) {
      if (stage == "dd_hs_0" || stage == "hs_stage_0") {
        lines <- c(lines, "     - Cap fry population at K0 (hard ceiling)")
      } else if (stage == "bh_stage_0") {
        lines <- c(lines, "     - Apply Beverton-Holt to egg->fry transition")
      } else if (grepl("^bh_stage_[0-9]+$", stage)) {
        stage_num <- gsub("bh_stage_", "", stage)
        lines <- c(lines,
                   paste0("     - Apply Beverton-Holt to stage ", stage_num, " transition"))
      } else if (grepl("^hs_stage_[0-9]+$", stage)) {
        stage_num <- gsub("hs_stage_", "", stage)
        lines <- c(lines,
                   paste0("     - Cap stage ", stage_num, " at K", stage_num, " (hard ceiling)"))
      } else if (grepl("bh_stage_pb", stage)) {
        lines <- c(lines, "     - Apply Beverton-Holt to pre-spawner stage")
      } else if (grepl("bh_stage_b", stage)) {
        lines <- c(lines, "     - Apply Beverton-Holt to spawner stage")
      } else if (stage == "bh_spawners") {
        lines <- c(lines, "     - Apply Beverton-Holt constraint on total spawners")
      } else if (stage == "hs_spawners") {
        lines <- c(lines, "     - Cap total spawners at K_spawners (hard ceiling)")
      }
    }
    lines <- c(lines, "")
  }

  # K values section
  lines <- c(lines,
    "------------------------------------------------------------",
    "4. CARRYING CAPACITY (K) VALUES BY STAGE",
    "------------------------------------------------------------"
  )

  for (k_name in names(k_summary)) {
    k_info <- k_summary[[k_name]]
    val_str <- if (is.numeric(k_info$value)) {
      format(k_info$value, big.mark = ",", scientific = FALSE)
    } else {
      as.character(k_info$value)
    }
    lines <- c(lines, paste0("   ", k_name, ": ", val_str))
    lines <- c(lines, paste0("      Source: ", k_info$source))
  }
  lines <- c(lines, "")

  # Collect all warnings
  all_warnings <- hab_warnings

  # Check for potential issues
  if (bh_dd_enabled && length(habitat_k_values) == 0 && is.na(k_adult)) {
    all_warnings <- c(all_warnings,
      "BH/HS stages enabled but no K values provided (habitat_dd_k or adult k)")
  }

  if (length(active_bh_dd) > 0) {
    for (stage in active_bh_dd) {
      if (grepl("bh_stage_0|hs_stage_0|dd_hs_0", stage)) {
        if (!"k_stage_0_mean" %in% names(habitat_k_values) && is.na(k_adult)) {
          all_warnings <- c(all_warnings,
            paste("Stage", stage, "enabled but no K0 value available"))
        }
      }
    }
  }

  if (length(all_warnings) > 0) {
    lines <- c(lines,
      "------------------------------------------------------------",
      "5. WARNINGS",
      "------------------------------------------------------------"
    )
    for (w in all_warnings) {
      lines <- c(lines, paste0("   ! ", w))
    }
    lines <- c(lines, "")
  }

  # Configuration notes section (if any notes exist)
  has_notes <- !is.null(s0_note) || !is.null(bh_dd_stages_note) || !is.null(k_values_note)
  if (has_notes) {
    lines <- c(lines,
      "------------------------------------------------------------",
      "6. CONFIGURATION NOTES",
      "------------------------------------------------------------"
    )

    if (!is.null(s0_note)) {
      lines <- c(lines,
        "   [s0 Optimization]",
        paste0("   ", s0_note),
        ""
      )
    }

    if (!is.null(bh_dd_stages_note)) {
      lines <- c(lines,
        "   [BH/HS Stage Configuration]",
        paste0("   ", bh_dd_stages_note),
        ""
      )
    }

    if (!is.null(k_values_note)) {
      lines <- c(lines,
        "   [Habitat Capacity Configuration]",
        paste0("   ", k_values_note),
        ""
      )
    }
  }

  # Summary section
  lines <- c(lines,
    "------------------------------------------------------------",
    "SUMMARY",
    "------------------------------------------------------------"
  )

  dd_methods <- character(0)
  if (!is.na(k_adult) && cr_enabled) {
    dd_methods <- c(dd_methods, "Compensation Ratios (via D.mx)")
  }
  if (length(beverton_holt_stages) > 0) {
    dd_methods <- c(dd_methods,
                    paste0("Beverton-Holt (", length(beverton_holt_stages), " stages)"))
  }
  if (length(hockey_stick_stages) > 0) {
    dd_methods <- c(dd_methods,
                    paste0("Hockey-Stick (", length(hockey_stick_stages), " stages)"))
  }

  if (length(dd_methods) == 0) {
    lines <- c(lines, "   Density dependence: NONE (density-independent growth)")
  } else {
    lines <- c(lines,
               paste0("   Active DD methods: ", paste(dd_methods, collapse = ", ")))
  }

  lines <- c(lines,
    paste0("   s0.1.det: ", s0_behavior),
    "",
    "============================================================"
  )

  explanation <- paste(lines, collapse = "\n")

  # ---------------------------------------------------------------------------
  # Print if verbose
  # ---------------------------------------------------------------------------

  if (verbose) {
    cat(explanation, "\n")
  }

  # ---------------------------------------------------------------------------
  # Return structured result
  # ---------------------------------------------------------------------------

  result <- list(
    explanation = explanation,

    summary = list(
      population_type = ifelse(anadromous, "anadromous", "resident"),
      n_stages = Nstage,
      adult_k = k_adult,
      dd_methods = dd_methods,
      s0_adjusted = (s0_behavior == "OPTIMIZED")
    ),

    s0_details = list(
      original_s0 = S0,
      s0_1_det = s0_1_det,
      behavior = s0_behavior,
      reason = s0_reason,
      note = s0_note
    ),

    compensation_ratios = list(
      enabled = cr_enabled && !is.na(k_adult),
      values = cr_values,
      stages_with_effect = cr_with_effect
    ),

    bh_dd_stages = list(
      enabled = bh_dd_enabled,
      all_stages = active_bh_dd,
      beverton_holt = beverton_holt_stages,
      hockey_stick = hockey_stick_stages,
      note = bh_dd_stages_note
    ),

    k_values = list(
      stages = k_summary,
      note = k_values_note
    ),

    warnings = all_warnings
  )

  invisible(result)
}
