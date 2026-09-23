#' pop_model_hab_dens_clean
#' @details Cleans input parameters from csv. Capacity column names are
#' lowercased and normalized (a missing `_mean` suffix is appended; spawner
#' column name variants are renamed to the canonical `k_stage_B_mean` /
#' `k_stage_B_cv`). Optional `k_..._cv` columns hold the coefficient of
#' variation for the matching `k_..._mean` capacity estimate and are used
#' for stochastic capacity draws in the population model (see
#' \code{\link{build_k_for_proj_dd}}). A `_cv` column with no matching
#' `_mean` column is dropped with a warning. Blank/NA cv values are treated
#' as 0 (capacity used as an exact value).
#' @param hab_dens A data frame. Habitat density data frame with capacity columns.
#' @param keep_cv Logical, default TRUE. If FALSE, all `k_..._cv`
#' (capacity uncertainty) columns are dropped and every capacity value is
#' treated as exact. The CEMPRA Shiny app runs with `keep_cv = FALSE`;
#' use the R package directly to include capacity uncertainty.
#'
#' @export
pop_model_hab_dens_clean <- function(hab_dens = NA, keep_cv = TRUE) {
  # Find capacity columns
  cap_col_filter <- which(grepl("^k_", colnames(hab_dens), ignore.case = TRUE))

  if (length(cap_col_filter) == 0) {
    stop(
      "No capacity (k_...) columns found in the habitat capacity file. ",
      "Column names must start with k_ (e.g., k_stage_1_mean)..."
    )
  }

  colnames(hab_dens)[cap_col_filter] <- tolower(colnames(hab_dens)[cap_col_filter])

  # Spawner capacity column name variants (canonical: k_stage_B_mean / k_stage_B_cv).
  # Note: columns are already lowercased above, so comparisons must be lowercase
  spawner_variants <- c("k_b", "k_stage_b", "k_stage_spawner",
                        "k_stage_spawners", "k_spawner", "k_spawners")

  # Loop through names and fix obvious errors
  keepers <- c() # columns to keep
  for (i in seq_along(cap_col_filter)) {
    this_col <- colnames(hab_dens)[cap_col_filter[i]]

    # cv (capacity uncertainty) columns
    if (grepl("_cv$", this_col)) {
      if (!keep_cv) {
        # cv columns dropped on request (e.g., Shiny app)
        next
      }
      # Normalize spawner cv name variants to canonical k_stage_B_cv
      if (this_col %in% paste0(spawner_variants, "_cv")) {
        this_col <- "k_stage_B_cv"
      }
      colnames(hab_dens)[cap_col_filter[i]] <- this_col
      keepers <- c(keepers, cap_col_filter[i])
      next
    }

    # keep if mean
    if (!(grepl("_mean$", this_col))) {
      this_col <- paste0(this_col, "_mean")
    }
    # Fix spawner naming alternatives
    # Normalize all spawner column name variants to canonical k_stage_B_mean
    if (this_col %in% paste0(spawner_variants, "_mean")) {
      this_col <- "k_stage_B_mean"
    }
    colnames(hab_dens)[cap_col_filter[i]] <- this_col
    keepers <- c(keepers, cap_col_filter[i])

  }

  hab_dens <- hab_dens[, c(1, 2, keepers), drop = FALSE]

  # Drop orphan cv columns (no matching _mean column)
  cv_cols <- grep("_cv$", colnames(hab_dens), value = TRUE)
  if (length(cv_cols) > 0) {
    expected_means <- gsub("_cv$", "_mean", cv_cols)
    orphans <- cv_cols[!(expected_means %in% colnames(hab_dens))]
    if (length(orphans) > 0) {
      warning(
        "Capacity uncertainty column(s) with no matching _mean column ",
        "dropped from the habitat capacity file: ",
        paste(orphans, collapse = ", "),
        call. = FALSE
      )
      hab_dens <- hab_dens[, !(colnames(hab_dens) %in% orphans), drop = FALSE]
    }
  }

  # Make sure k_ columns are numeric
  convert_k_columns <- function(df) {
    # Find all column names that start with "k_"
    k_cols <- grep("^k_", names(df), value = TRUE)
    # Loop over the identified columns and convert them
    for (col in k_cols) {
      if (is.character(df[[col]])) {
        df[[col]] <- gsub(",", "", df[[col]])
        df[[col]] <- as.numeric(as.character(df[[col]]))
      }
    }
    return(df)
  }


  hab_dens <- convert_k_columns(hab_dens)

  return(hab_dens)

}
