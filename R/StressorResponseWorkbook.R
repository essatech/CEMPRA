#' Get Stressor Response Data from Excel Workbook
#'
#' @description Extract the stressor response data from each sheet in the Excel workbook.
#'
#' @details For more information about data format requirements, see the Data Upload and Results Export tab of the associated Shiny App.
#'
#' Uses `readxl::read_excel()` to load in workbook sheets:
#' * Main sheet: meta data / cover sheet indicating:
#'     * stressor names
#'     * categories
#'     * interactions and linkages
#'     * scales
#'     * approximation function type
#'     * target life stages
#'     * contribution to survival or capacity parameters
#'     * stressor units
#'     * target model (CE or population models, or both)
#'
#' * all additional stressor-response sheets in workbook
#'     * context specific stressor-response curve data
#'     * sheet name must match stressor names in Main sheet, otherwise "Bad worksheet names" error message.
#'     * sheets must contain columns for `<stressor_name>`, `Mean System Capacity (%)`, `SD`, `low.limit`, and `up.limit`
#'
#' @param filename A string. Relative file name to the stressor response Excel workbook.
#'
#' @returns A list object of the `main_sheet`, `stressor_names` and stressor response data frames (`sr_dat`).
#'
#' @export
StressorResponseWorkbook <- function(filename = NA) {

  # File to read and view stressor-response relations
  main_sheet <- readxl::read_excel(filename, sheet = "Main")

  # Order rows in main_sheet to match stressor names alphabetically
  main_sheet <- main_sheet[order(main_sheet$Stressors), ]

  # Get Stressor names
  stressor_names <- as.character(main_sheet$Stressors)
  stressor_names <- stressor_names[!(is.na(stressor_names))]


  # Ensure worksheets are named properly
  snames <- readxl::excel_sheets(filename)
  snames <- snames[2:length(snames)]

  # Retain all names and matrix names
  snames_all <- snames


  # Check if there are any matrix interactions
  if(any(grepl("MInt_", snames))) {
    mint_names <- snames[grepl("MInt_", snames)]
    mint_obj <- interaction_matrix_load(mint_names = mint_names, filename = filename)
  } else {
    mint_names <- NULL
    mint_obj <- NULL
  }


  # Ignore any custom matrix interaction terms
  snames <- snames[!(grepl("MInt_", snames))]

  if (!(all(snames %in% stressor_names))) {
    return("Bad worksheet names")
  }

  if (!(all(stressor_names %in% snames))) {
    return("Bad worksheet names")
  }




  # Gather stressor-response relationships for other variables
  ReadSheets <- function(sheet) {
    dat <- readxl::read_xlsx(filename,
      sheet = sheet
    )
    dat <- dat[, c(1:5)] # Exclude other extraneous columns to right (if any)
    # Make sure columns in the correct order
    colnames(dat)[1] <- "value"
    colnames(dat)[2] <- "Mean System Capacity (%)"

    if (any(colnames(dat) != c("value", "Mean System Capacity (%)", "SD", "low.limit", "up.limit"))) {
      return("Column names in stressor response workbook out of order")
    }
    colnames(dat) <- c("value", "mean_system_capacity", "sd", "lwr", "upr")
    return(dat)
  }


  sr_dat <- lapply(stressor_names, ReadSheets)
  names(sr_dat) <- stressor_names



  # Make pretty names
  pretty_names <- c()
  for (i in 1:length(stressor_names)) {
    this_name <- stressor_names[i]
    this_name <- gsub("_", " ", this_name)
    pretty_names <- c(pretty_names, this_name)
  }


  # Add in any interaction matrices
  if(any(grepl("MInt_", snames_all))) {

    mint_names <- snames_all[grepl("MInt_", snames_all)]
    #stressor_names <- c(stressor_names, mint_names)

    # Get pretty names
    full_names <- lapply(mint_obj, function(x) { x$Matrix_Name })
    full_names <- unlist(full_names)
    names(full_names) <- NULL
    #pretty_names <- c(pretty_names, full_names)

  }


  # Build export object
  ret_obj <- list()
  ret_obj$main_sheet <- main_sheet
  ret_obj$stressor_names <- stressor_names
  ret_obj$pretty_names <- pretty_names
  ret_obj$sr_dat <- sr_dat
  ret_obj$MInt <- mint_obj

  return(ret_obj)
}
