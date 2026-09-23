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
#'     * sheet name must match (final) stressor names in Main sheet, otherwise a "Bad worksheet names" error is raised listing the mismatches.
#'     * sheets must contain columns for `<stressor_name>`, `Mean System Capacity (%)`, `SD`, `low.limit`, and `up.limit`
#'
#' Each stressor name may appear only once in the Main sheet: one stressor is
#' always linked to exactly one stressor-response relationship. To link one
#' stressor-response relationship to multiple life stages in the population
#' model, use a comma-separated list in the Life_stages column (e.g.,
#' "stage_1, stage_2"); to affect multiple parameters (e.g., survival and
#' capacity), create separate stressors, each with its own worksheet.
#'
#' The Stressor_cat column is OPTIONAL and is normally a user label only
#' (it is overwritten with the stressor name on load; the original value is
#' preserved in `Stressor_cat_orig`). The one exception: if the same stressor
#' name appears on several rows and every copy has a distinct, non-blank
#' Stressor_cat, the rows are automatically renamed
#' `"<Stressor>_<Stressor_cat>"` (with a warning). The curve worksheets and
#' the stressor magnitude workbook must then use those full names. Note
#' Excel limits worksheet names to 31 characters. The category value
#' "Total_Mortality" is reserved (see \code{\link{StressorMagnitudeWorkbook}})
#' and is never used to rename.
#'
#' @param filename A string. Relative file name to the stressor response Excel workbook.
#'
#' @returns A list object of the `main_sheet`, `stressor_names` and stressor response data frames (`sr_dat`).
#'
#' @export
StressorResponseWorkbook <- function(filename = NA) {

  # File to read and view stressor-response relations
  main_sheet <- readxl::read_excel(filename, sheet = "Main")

  # Stressor_cat is an OPTIONAL column - synthesize it if absent
  if (!("Stressor_cat" %in% colnames(main_sheet))) {
    main_sheet$Stressor_cat <- main_sheet$Stressors
  }

  # Trim whitespace on names and categories
  main_sheet$Stressors <- trimws(as.character(main_sheet$Stressors))
  main_sheet$Stressor_cat <- trimws(as.character(main_sheet$Stressor_cat))

  # Preserve the user's original category label for display/reference
  # (Stressor_cat itself is overwritten with the stressor name below)
  main_sheet$Stressor_cat_orig <- main_sheet$Stressor_cat

  # --------------------------------------------------------------------------
  # Duplicate stressor names: disambiguate via Stressor_cat (rare)
  # --------------------------------------------------------------------------
  # If the same stressor name appears on several rows and every copy carries a
  # distinct, non-blank Stressor_cat, the rows are renamed to
  # "<Stressor>_<Stressor_cat>" (e.g., "Pools_Winter Habitat",
  # "Pools_Summer Rear"). The curve worksheets must use these final
  # (concatenated) names, and the stressor magnitude workbook applies the
  # same rule so the two files stay aligned. "Total_Mortality" is reserved
  # for the magnitude-side aggregation and is never used to rename.
  # Duplicates that cannot be resolved this way stop with an error below.
  nm <- main_sheet$Stressors
  dup_all <- unique(nm[duplicated(nm)])
  dup_all <- dup_all[!(is.na(dup_all))]
  if (length(dup_all) > 0) {
    for (dn in dup_all) {
      idx <- which(nm == dn)
      cats <- main_sheet$Stressor_cat[idx]
      usable <- !(is.na(cats)) & cats != "" & cats != dn &
        cats != "Total_Mortality"
      if (all(usable) && !(any(duplicated(cats)))) {
        main_sheet$Stressors[idx] <- paste(dn, cats, sep = "_")
        warning(
          "Duplicate stressor name '", dn, "' resolved using Stressor_cat: ",
          "renamed to ", paste0("'", paste(dn, cats, sep = "_"), "'",
                                collapse = ", "),
          ". The stressor-response worksheets and the stressor magnitude ",
          "workbook must use these full names.",
          call. = FALSE
        )
      }
    }
  }

  # Order rows in main_sheet to match stressor names alphabetically
  main_sheet <- main_sheet[order(main_sheet$Stressors), ]

  # Make the Stressor_cat column in the main sheet equal to the (final)
  # Stressors column. The user's original label is kept in Stressor_cat_orig.
  main_sheet$Stressor_cat <- main_sheet$Stressors


  # Make sure interactions column inputs are valid
  # main_sheet$Interaction



  # Get Stressor names
  stressor_names <- as.character(main_sheet$Stressors)
  stressor_names <- stressor_names[!(is.na(stressor_names))]

  # One stressor = one stressor-response relationship: duplicate stressor
  # names remaining after the Stressor_cat disambiguation above are an
  # authoring error and would corrupt downstream per-stressor lookups.
  dup_names <- unique(stressor_names[duplicated(stressor_names)])
  if (length(dup_names) > 0) {
    stop(
      "Duplicate stressor name(s) in the Main sheet of the stressor-response ",
      "workbook: ", paste(dup_names, collapse = ", "), ". Each stressor may ",
      "only appear once. To link one stressor-response relationship to ",
      "multiple life stages, use a comma-separated list in the Life_stages ",
      "column (e.g., 'stage_1, stage_2'). To affect multiple parameters ",
      "(e.g., survival and capacity), create separate stressors, each with ",
      "its own worksheet (e.g., Fines_surv, Fines_cap). Alternatively, give ",
      "each duplicate row a distinct, non-blank Stressor_cat and the rows ",
      "will be renamed '<Stressor>_<Stressor_cat>' automatically (the curve ",
      "worksheets must then use those full names).",
      call. = FALSE
    )
  }


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

  if (!(all(snames %in% stressor_names)) ||
      !(all(stressor_names %in% snames))) {
    missing_sheets <- setdiff(stressor_names, snames)
    extra_sheets <- setdiff(snames, stressor_names)
    stop(
      "Bad worksheet names: every stressor in the Main sheet needs a ",
      "worksheet with exactly the same name.",
      if (length(missing_sheets) > 0) {
        paste0(" Stressors with no matching worksheet: ",
               paste(missing_sheets, collapse = ", "), ".")
      } else "",
      if (length(extra_sheets) > 0) {
        paste0(" Worksheets with no matching stressor: ",
               paste(extra_sheets, collapse = ", "), ".")
      } else "",
      " Note: stressors renamed via Stressor_cat disambiguation must have ",
      "worksheets named with the full '<Stressor>_<Stressor_cat>' name ",
      "(Excel limits sheet names to 31 characters).",
      call. = FALSE
    )
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
