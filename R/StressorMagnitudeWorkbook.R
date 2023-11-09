#' Get Stressor Magnitude Data from Excel Workbook
#'
#' @description Extract the stressor magnitude data from each sheet in the Excel workbook.
#'
#' @details For more information about data format requirements, see the Data Upload section of the vignette.
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
  data$Mean <- round(data$Mean, 2)

  # Ensure that there are no duplicates
  dups <- paste0(data$HUC_ID, data$Stressor)

  if (any(duplicated(dups))) {
    # print(dups[which(duplicated(dups))])
    print("Duplicated values in stressor magnitude worksheet...")
  }


  # If HUC_ID column is missing create it from HUC_10
  if(!("HUC_ID" %in% colnames(data))) {
    if("HUC_10" %in% colnames(data)) {
      data$HUC_ID <- data$HUC_10
    } else {
      stop("Missing HUC ID column")
    }
  }



  # Ensure that all values are formatted properly
  target_columns <- c(
    "HUC_ID", "NAME", "Stressor", "Stressor_cat", "Mean", "SD",
    "Distribution", "Low_Limit", "Up_Limit"
  )

  if(!(all(target_columns %in% colnames(data)))) {
    stop(paste0("Bad column names. Expect columns to be ", paste(target_columns, collapse = ", ")))
  }


  data$SD <- as.numeric(data$SD)
  data$Mean <- as.numeric(data$Mean)


  # Need to deal with total mortality column
  # Total_Mortality is a sum of other inputs
  if ("Total_Mortality" %in% base::unique(data$Stressor_cat)) {
    data_other <- data[which(data$Stressor_cat != "Total_Mortality"), ]
    data_mort <- data[which(data$Stressor_cat == "Total_Mortality"), ]

    # summarise by HUC ID
    dms1 <- dplyr::group_by(data_mort, HUC_ID)
    dms2 <- dplyr::summarise(dms1,
      tmort_mean = sum(Mean),
      tmort_sd = mean(SD)
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
