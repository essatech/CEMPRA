#' pop_model_hab_dens_clean
#' @details Cleans input parameters from csv.
#'
#' @export
pop_model_hab_dens_clean <- function(hab_dens = NA) {
  # Find capacity columns
  cap_col_filter <- which(grepl("^k_", colnames(hab_dens), ignore.case = TRUE))
  colnames(hab_dens)[cap_col_filter] <- tolower(colnames(hab_dens)[cap_col_filter])

  # Loop through names and fix obvious errors
  keepers <- c() # columns to keep
  for (i in 1:length(cap_col_filter)) {
    this_col <- colnames(hab_dens)[cap_col_filter[i]]
    # check if ends with _cv
    if (grepl("_cv$", this_col)) {
      # skip cv columns
      next
    }

    # keep if mean
    if (!(grepl("_mean$", this_col))) {
      this_col <- paste0(this_col, "_mean")
    }
    # Fix spawner naming alternatives
    if (this_col == "k_B_mean") {
      this_col <- "k_stage_B_mean"
    }
    if (this_col == "k_stage_spawner_mean") {
      this_col <- "k_stage_B_mean"
    }
    if (this_col == "k_stage_spawners_mean") {
      this_col <- "k_stage_B_mean"
    }
    if (this_col == "k_spawner_mean") {
      this_col <- "k_stage_B_mean"
    }
    if (this_col == "k_spawners_mean") {
      this_col <- "k_stage_B_mean"
    }
    colnames(hab_dens)[cap_col_filter[i]] <- this_col
    keepers <- c(keepers, cap_col_filter[i])

  }

  hab_dens <- hab_dens[, c(1, 2, keepers)]

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
