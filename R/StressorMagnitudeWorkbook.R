#' Get Stressor Magnitude Data from Excel Workbook
#'
#' @description Extract the stressor magnitude data from each sheet in the Excel workbook.
#'
#' @details For more information about data format requirements, see the Data Upload section of the vignette.
#'  Optional `From_Year` and `To_Year` columns may be supplied. When present, they are
#'  used only by the population model to restrict a stressor (per HUC) to an activation
#'  window (active between `From_Year` and `To_Year`, inclusive); they are ignored by the
#'  Joe Model. Stressors with missing/blank values, or files without these columns, are
#'  applied to all years as before (backwards compatible).
#'
#'  The `Stressor_cat` column is OPTIONAL and is normally a user label only
#'  (the original value is preserved in `Stressor_cat_orig`), with two
#'  exceptions. (1) The reserved value `"Total_Mortality"` (exact spelling)
#'  aggregates all rows so tagged into a single synthetic `Total_Mortality`
#'  stressor per HUC: the Means are summed and the SDs are pooled in
#'  quadrature (`sqrt(sum(SD^2))`), dropping NA components (all-NA -> 0). A
#'  matching `Total_Mortality` curve must exist in the stressor-response
#'  workbook for the aggregate to have an effect. Near-miss spellings warn.
#'  (2) If the stressor-response workbook disambiguated duplicate stressor
#'  names as `"<Stressor>_<Stressor_cat>"`, dose rows are aligned to those
#'  full names at model run time (see
#'  \code{\link{align_dose_stressor_names}}, with a warning). Duplicate
#'  stressor names within this workbook are otherwise left untouched:
#'  legacy files legitimately list multiple component rows per stressor
#'  with `Stressor_cat` as a component label.
#'
#' @param filename A string. Relative file name to the stressor magnitude Excel workbook.
#' @param scenario_worksheet A string. Worksheet name for the scenario.
#'
#' @returns A stressor magnitude list object for the CEMPRA
#'
#' @export
StressorMagnitudeWorkbook <- function(filename = NA, scenario_worksheet = NA) {

  # Define scope variables as NULL
  HUC_ID <- Mean <- SD <- NULL


  # If work sheet is not set assume first
  scenario_worksheet <- ifelse(is.na(scenario_worksheet), 1, scenario_worksheet)

  # File to read and view stressor-response relations
  data <- readxl::read_excel(filename, sheet = scenario_worksheet)

  # Round values to two decimal places
  # data$Mean <- round(data$Mean, 2)
  data$Mean <- suppressWarnings({ as.numeric(as.character(data$Mean)) })
  data$Mean <- round(data$Mean, 5)

  # Allow ID column to by any name
  if(!("HUC_ID" %in% colnames(data))) {
    data$HUC_ID <- as.matrix(data[, 1])[, 1]
  }

  # If HUC_ID column is missing create it from HUC_10
  if(!("HUC_ID" %in% colnames(data))) {
    if("HUC_10" %in% colnames(data)) {
      data$HUC_ID <- data$HUC_10
    } else {
      stop("Missing HUC ID column")
    }
  }

  # Stressor_cat is an OPTIONAL column - synthesize it if absent
  if (!("Stressor_cat" %in% colnames(data))) {
    data$Stressor_cat <- data$Stressor
  }

  # Ensure that all values are formatted properly
  target_columns <- c(
    "HUC_ID", "NAME", "Stressor", "Stressor_cat", "Mean", "SD",
    "Distribution", "Low_Limit", "Up_Limit"
  )

  if(!(all(target_columns %in% colnames(data)))) {
    stop(paste0("Bad column names. Expect columns to be ", paste(target_columns, collapse = ", ")))
  }

  # Trim whitespace on names and categories; preserve the user's original
  # category label for display/reference
  data$Stressor <- trimws(as.character(data$Stressor))
  data$Stressor_cat <- trimws(as.character(data$Stressor_cat))
  data$Stressor_cat_orig <- data$Stressor_cat

  # Warn on near-miss spellings of the reserved Total_Mortality value
  # (the aggregation below only fires on the exact spelling)
  near_tm <- !(is.na(data$Stressor_cat)) &
    grepl("^total[ _]?mortality$", tolower(data$Stressor_cat)) &
    data$Stressor_cat != "Total_Mortality"
  if (any(near_tm)) {
    warning(
      "Stressor_cat value(s) ",
      paste(unique(data$Stressor_cat[near_tm]), collapse = ", "),
      " look like the reserved value 'Total_Mortality' but do not match ",
      "exactly, so those rows will NOT be aggregated into a Total_Mortality ",
      "stressor. Use the exact spelling 'Total_Mortality' if aggregation ",
      "is intended.",
      call. = FALSE
    )
  }

  # NOTE: duplicate stressor names with distinct Stressor_cat values are NOT
  # renamed here. Legacy magnitude workbooks legitimately list multiple
  # component rows per (HUC, stressor) with Stressor_cat as a component
  # label feeding a single stressor-response curve, which is
  # indistinguishable (from this file alone) from the disambiguation
  # pattern. The stressor-response workbook is the authority: when its Main
  # sheet renames duplicates to "<Stressor>_<Stressor_cat>", dose rows are
  # aligned to those names at model run time (see
  # align_dose_stressor_names(), called by JoeModel_Run and
  # PopulationModel_Run).

  # Ensure that there are no duplicates
  dups <- paste0(data$HUC_ID, data$Stressor)

  if (any(duplicated(dups))) {
    # print(dups[which(duplicated(dups))])
    message("Duplicated values in stressor magnitude worksheet...")
  }


  data$SD <- as.numeric(data$SD)
  data$Mean <- as.numeric(data$Mean)


  # --------------------------------------------------------------------------
  # Optional From_Year / To_Year activation-window columns
  # --------------------------------------------------------------------------
  # These are OPTIONAL columns. If present, they are used only by the
  # population model to restrict when a stressor is active (between From_Year
  # and To_Year, inclusive). They are ignored by the Joe Model. Files without
  # these columns load and run exactly as before (backwards compatible).
  # Accept common case/spacing variants and normalize to From_Year / To_Year.
  norm_window_col <- function(cn) {
    key <- tolower(gsub("[ _]", "", cn))
    ifelse(key == "fromyear", "From_Year",
           ifelse(key == "toyear", "To_Year", cn))
  }
  colnames(data) <- vapply(colnames(data), norm_window_col, character(1))

  # Coerce window columns to numeric if present (blank/NA/garbage -> NA)
  if ("From_Year" %in% colnames(data)) {
    data$From_Year <- suppressWarnings(as.numeric(as.character(data$From_Year)))
  }
  if ("To_Year" %in% colnames(data)) {
    data$To_Year <- suppressWarnings(as.numeric(as.character(data$To_Year)))
  }


  # Need to deal with total mortality column
  # Total_Mortality is a sum of other inputs
  if ("Total_Mortality" %in% base::unique(data$Stressor_cat)) {
    data_other <- data[which(data$Stressor_cat != "Total_Mortality"), ]
    data_mort <- data[which(data$Stressor_cat == "Total_Mortality"), ]

    # summarise by HUC ID. Means add; SDs pool in quadrature
    # (sqrt of summed variances - independent mortality components).
    # NA components are dropped; if every SD is NA the pooled SD is 0.
    dms1 <- dplyr::group_by(data_mort, HUC_ID)
    dms2 <- dplyr::summarise(dms1,
      tmort_mean = sum(Mean, na.rm = TRUE),
      tmort_sd = sqrt(sum(SD^2, na.rm = TRUE))
    )

    # Consolidate
    data_mort_new <- data_mort
    data_mort_new$Stressor <- "Total_Mortality"
    data_mort_new$Mean <- 0
    data_mort_new$SD <- 0
    data_mort_new$Distribution <- "normal"
    data_mort_new$Low_Limit <- 0
    data_mort_new$Up_Limit <- 1
    data_mort_new$Comments <- NA
    # Total_Mortality is a Joe Model aggregate (summed mortality). The activation
    # window is a population-model-only concept, so drop From_Year/To_Year here.
    # This also prevents differing window values from defeating the de-dup below.
    if ("From_Year" %in% colnames(data_mort_new)) data_mort_new$From_Year <- NA
    if ("To_Year" %in% colnames(data_mort_new)) data_mort_new$To_Year <- NA
    nrow(data_mort_new)
    data_mort_new <- data_mort_new[!(duplicated(data_mort_new)), ]
    nrow(data_mort_new)

    if (any(duplicated(data_mort_new$HUC_ID))) {
      return("Duplicated values for total mortality...")
    }

    # Update summaries
    data_mort_new$Mean <- dms2$tmort_mean[match(data_mort_new$HUC_ID, dms2$HUC_ID)]
    data_mort_new$SD <- dms2$tmort_sd[match(data_mort_new$HUC_ID, dms2$HUC_ID)]

    # Recombine to single data frame
    data <- rbind(data_other, data_mort_new)
  }
  # End of total mortality special handling


  return(data)
}
