#' Get Socio Economic Data from Excel Workbook
#'
#' @description Extract the socio economic data from each sheet
#' in the Excel workbook.
#'
#' @details For more information about data format requirements, see manual.
#'
#' @param filename A string. Relative file name to the stressor response Excel workbook.
#'
#' @returns A list object of each worksheet organized as separate data frames.
#'
#' @examples
#' \dontrun{
#' library(CEMPRA)
#' filename <- system.file("extdata", "socio_economic/socio_economic_input.xlsx", package = "CEMPRA")
#' socioeconomic_inputs <- SocioEconomicWorkbook(filename = filename)
#' }
#'
#' @export
SocioEconomicWorkbook <- function(filename = NA) {

  # List object to build
  ret_obj <- list()

  ret_obj$error_state <- "Ready to go!"


  ret_obj$import_pass <- FALSE

  # filename <- system.file("extdata/socio_economic", "socio_economic_input.xlsx", package = "CEMPRA")

  # Check that the file exists
  if (!file.exists(filename)) {
    ret_obj$error_state <- "File does not exist"
    return(ret_obj)
  }

  # Check to make sure we have all worksheets.
  # Extra worksheets are ok.
  sheet_names <- readxl::excel_sheets(filename)

  # Check to make sure we have all worksheets.
  must_have <- c(
    "Management Actions",
    "Location Implementation",
    "Location Size Attributes",
    "Stressor Reduction"
  )

  # Check if missing critical worksheet
  missing <- setdiff(must_have, sheet_names)
  if (length(missing) > 0) {
    ret_obj$error_state <-
      paste0("Missing worksheets: ", paste(missing, collapse = ", "))
    return(ret_obj)
  }


  #--------------------------------------
  # Management Actions - worksheet
  #--------------------------------------

  # Import
  dat <- readxl::read_excel(filename,
                            sheet = "Management Actions",
                            skip = 1)

  # Column check
  must_have <- c(
    "Management Action Name",
    "Measurement Unit",
    "Mean Cost per Unit",
    "SD of Cost per Unit",
    "Lower Limit of Cost per Unit",
    "Upper Limit of Cost per Unit",
    "Bulk Discount Units Threshold (Level 1)",
    "Bulk Discount Price Multiplier (Level 1)",
    "Bulk Discount Units Threshold (Level 2)",
    "Bulk Discount Price Multiplier (Level 2)"
  )

  check_this <- colnames(dat)[1:length(must_have)]

  # Check that all items are equal between must_have and check_this
  if (!all(must_have %in% check_this)) {
    ret_obj$error_state <- "Management Actions worksheet does
    not have the correct columns"
    return(ret_obj)
  }

  # Subset to target columns
  dat <- dat[, must_have]

  # Drop rows with NA
  dat <- dat[stats::complete.cases(dat$`Management Action Name`), ]


  # Fix mean cost
  dat$`Mean Cost per Unit` <- as.numeric(dat$`Mean Cost per Unit`)

  dat$`SD of Cost per Unit` <- as.numeric(dat$`SD of Cost per Unit`)
  dat$`SD of Cost per Unit` <-
    ifelse(is.na(dat$`Mean Cost per Unit`), 0, dat$`SD of Cost per Unit`)
  dat$`Lower Limit of Cost per Unit` <-
    as.numeric(dat$`Lower Limit of Cost per Unit`)
  dat$`Lower Limit of Cost per Unit` <-
    ifelse(is.na(dat$`Lower Limit of Cost per Unit`),
           0,
           dat$`Lower Limit of Cost per Unit`)
  dat$`Upper Limit of Cost per Unit` <-
    as.numeric(dat$`Upper Limit of Cost per Unit`)
  dat$`Bulk Discount Units Threshold (Level 1)` <-
    as.numeric(dat$`Bulk Discount Units Threshold (Level 1)`)
  dat$`Bulk Discount Price Multiplier (Level 1)` <-
    as.numeric(dat$`Bulk Discount Price Multiplier (Level 1)`)
  dat$`Bulk Discount Units Threshold (Level 2)` <-
    as.numeric(dat$`Bulk Discount Units Threshold (Level 2)`)
  dat$`Bulk Discount Price Multiplier (Level 2)` <-
    as.numeric(dat$`Bulk Discount Price Multiplier (Level 2)`)

  if (any(duplicated(dat$`Management Action Name`))) {
    ret_obj$error_state <-
      "Management Action Name in Management Actions must be unique"
    return(ret_obj)
  }


  # Key attributes
  management_actions <- dat$`Management Action Name`

  # Assign data frame to list object
  ret_obj$`Management Actions` <- dat



  #--------------------------------------
  # Location Implementation - worksheet
  #--------------------------------------

  # Import
  dat <- readxl::read_excel(filename,
                            sheet = "Location Implementation",
                            skip = 1)

  # Column check
  must_have <- c(
    "Location ID",
    "Location Name",
    "Management Action Name",
    "Measurement Unit",
    "Mean Number of Units Restored",
    "SD Number of Units",
    "Lower Limit for Number of Units",
    "Upper Limit for Number of Units",
    "Location Cost Multiplier",
    "Location Effect Multiplier"
  )

  check_this <- colnames(dat)[1:length(must_have)]

  # Check that all items are equal between must_have and check_this
  if (!all(must_have %in% check_this)) {
    ret_obj$error_state <- "Location Implementation worksheet does
    not have the correct columns"
    return(ret_obj)
  }

  # subset to target
  dat <- dat[, must_have]


  # Drop rows with NA for management action
  dat <- dat[stats::complete.cases(dat$`Management Action Name`), ]
  # Drop rows with NA for Location ID
  dat <- dat[stats::complete.cases(dat$`Location ID`), ]



  # Ensure target columns are numeric
  dat$`Mean Number of Units Restored` <-
    as.numeric(dat$`Mean Number of Units Restored`)
  dat$`SD Number of Units` <- as.numeric(dat$`SD Number of Units`)
  dat$`SD Number of Units` <-
    ifelse(is.na(dat$`Mean Number of Units Restored`),
           0,
           dat$`SD Number of Units`)
  dat$`Lower Limit for Number of Units` <-
    as.numeric(dat$`Lower Limit for Number of Units`)
  dat$`Lower Limit for Number of Units` <-
    ifelse(is.na(dat$`Lower Limit for Number of Units`),
           0,
           dat$`Lower Limit for Number of Units`)
  dat$`Upper Limit for Number of Units` <-
    as.numeric(dat$`Upper Limit for Number of Units`)
  dat$`Location Cost Multiplier` <-
    as.numeric(dat$`Location Cost Multiplier`)

  dat$`Location Cost Multiplier` <-
    ifelse(is.na(dat$`Location Cost Multiplier`),
           1,
           dat$`Location Cost Multiplier`)

  dat$`Location Effect Multiplier` <-
    as.numeric(dat$`Location Effect Multiplier`)
  dat$`Location Effect Multiplier` <-
    ifelse(is.na(dat$`Location Effect Multiplier`),
           1,
           dat$`Location Effect Multiplier`)


  # Remove locations with no action
  dat <- dat[dat$`Mean Number of Units Restored` > 0, ]

  # No location actions
  if (nrow(dat) == 0) {
    ret_obj$error_state <-
      "Location Implementation empty. No proposed actions at locations..."
    return(ret_obj)
  }


  check_unique <-
    paste0(dat$`Location ID`, "__", dat$`Management Action Name`)

  if (any(duplicated(check_unique))) {
    ret_obj$error_state <-
      "Location Implementation worksheet must have unique combinations of Location ID and Management Action Name..."
    return(ret_obj)
  }


  # Check management actions are represented in previous worksheet
  check_unique <- paste0(dat$`Management Action Name`)
  any_diff <- setdiff(check_unique, management_actions)

  if (length(any_diff) > 0) {
    ret_obj$error_state <-
      paste0(
        "All management actions in Location Implementation worksheet mmust be described in Management Actions..."
      )
    return(ret_obj)
  }


  # Assign data frame to list object
  ret_obj$`Location Implementation` <- dat




  #--------------------------------------
  # Location Size Attributes - worksheet
  #--------------------------------------

  # Import
  dat <- readxl::read_excel(filename,
                            sheet = "Location Size Attributes",
                            skip = 2)

  # Column check

  must_have <- c("Location ID",
                 "Location Name")

  check_this <- colnames(dat)[1:length(must_have)]

  # Check that all items are equal between must_have and check_this
  if (!all(must_have %in% check_this)) {
    ret_obj$error_state <- "Location Size Attributes worksheet does
    not have the correct columns"
    return(ret_obj)
  }

  # Drop colnames with ...

  dat <- dat[, !grepl("\\.\\.\\.", colnames(dat))]

  size_columns <- setdiff(colnames(dat), must_have)

  if (length(size_columns) == 0) {
    ret_obj$error_state <-
      "Location Size Attributes needs baseline total areas"
    return(ret_obj)
  }

  # Key attributes
  size_columns

  if (any(duplicated(dat$`Location ID`))) {
    ret_obj$error_state <-
      "Location Size Attributes must have unique locations"
    return(ret_obj)
  }


  # Assign data frame to list object
  ret_obj$`Location Size Attributes` <- dat



  #--------------------------------------
  # Stressor Reduction - worksheet
  #--------------------------------------

  # Import
  dat <- readxl::read_excel(filename,
                            sheet = "Stressor Reduction",
                            skip = 1)

  # Column check
  must_have <- c(
    "Stress Reduction Curve ID",
    "Management Intervention Name",
    "Measurement Unit for Management Action",
    "Location Baseline Size Scaling Column",
    "Restoration Effort per Unit Area",
    "Affected Stressor"
  )

  check_this <- colnames(dat)[1:length(must_have)]

  # Check that all items are equal between must_have and check_this
  if (!all(must_have %in% check_this)) {
    ret_obj$error_state <- "Stressor Reduction worksheet does
    not have the correct columns"
    return(ret_obj)
  }

  # Check that management actions are represented in cost worksheet
  sr_mas <- unique(dat$`Management Intervention Name`)

  mdiff <- setdiff(sr_mas, management_actions)

  if (length(mdiff) > 0) {
    ret_obj$error_state <-
      paste0(
        "All management actions in Stressor Reduction worksheet must be described in Management Actions..."
      )
    return(ret_obj)
  }


  # Check that management actions are represented in cost worksheet
  sr_mas <- unique(dat$`Location Baseline Size Scaling Column`)
  mdiff <- setdiff(sr_mas, size_columns)

  if (length(mdiff) > 0) {
    ret_obj$error_state <-
      paste0(
        "All Location Baseline Size Scaling Columns in Stressor Reduction worksheet must be included in the Location Size Attributes worksheet..."
      )
    return(ret_obj)
  }

  # Filter to only include target columns
  dat <- dat[, must_have]


  # Attribute
  stressor_reduction_functions <-
    unique(dat$`Stress Reduction Curve ID`)

  # Attribute
  linked_stressors <- unique(dat$`Affected Stressor`)


  # Assign data frame to list object
  ret_obj$`Stressor Reduction` <- dat



  #---------------------------------------------
  # Loop through SR curve IDs and perform checks
  #---------------------------------------------

  sr_curvs <- list()

  for (i in 1:length(stressor_reduction_functions)) {
    this_func <- stressor_reduction_functions[i]

    if (!(this_func %in% sheet_names)) {
      ret_obj$error_state <- paste0(this_func, " worksheet is missing...")
      return(ret_obj)
    }


    # Import
    dat <- readxl::read_excel(filename,
                              sheet = this_func,
                              skip = 0)

    # Column check
    must_have <- c("SD",
                   "low.limit",
                   "up.limit")

    check_this <- colnames(dat)[1:5]

    # Check that all items are equal between must_have and check_this
    if (!all(must_have %in% check_this)) {
      ret_obj$error_state <-
        paste0(this_func, " worksheet does not have the correct columns")
      return(ret_obj)
    }

    # Ensure that column 1 is the restoration effort defined
    # in the SR worksheet

    colnames(dat)[1] <- "restoration_effort"
    colnames(dat)[2] <- "effect"

    dat$restoration_effort <- as.numeric(dat$restoration_effort)
    dat$effect <- as.numeric(dat$effect)
    dat$SD <- as.numeric(dat$SD)
    dat$low.limit <- as.numeric(dat$low.limit)
    dat$up.limit <- as.numeric(dat$up.limit)
    dat$SD <- ifelse(is.na(dat$SD), 0, dat$SD)

    sr_curvs[[this_func]] <- dat

  }

  # Assign nested list
  ret_obj$sr_curvs <- sr_curvs


  #--------------------------------------------------
  # Finalize return object
  #--------------------------------------------------


  ret_obj$error_state <- "Ready to go!"

  attributes <- list(
    management_actions = management_actions,
    size_columns = size_columns,
    stressor_reduction_functions = stressor_reduction_functions,
    linked_stressors = linked_stressors
  )

  ret_obj$import_pass <- TRUE


  ret_obj$attributes <- attributes

  return(ret_obj)


}
