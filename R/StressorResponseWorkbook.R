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

  # The Main cover sheet must be the FIRST worksheet: downstream code
  # (and the Shiny app) assume sheet 1 is the cover sheet, so an
  # alphabetized or re-ordered workbook previously produced confusing
  # failures far from the cause
  all_sheets <- readxl::excel_sheets(filename)
  if (all_sheets[1] != "Main") {
    stop(
      "The FIRST worksheet of the stressor response workbook must be ",
      "named 'Main' (found '", all_sheets[1], "'). Move the Main cover ",
      "sheet to the first position.",
      call. = FALSE
    )
  }

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
    if (ncol(dat) < 5) {
      stop(
        "Stressor-response worksheet '", sheet, "' has only ", ncol(dat),
        " column(s). Expected five columns: '<stressor name>' (dose values), ",
        "'Mean System Capacity (%)', 'SD', 'low.limit', 'up.limit'.",
        call. = FALSE
      )
    }
    dat <- dat[, c(1:5)] # Exclude other extraneous columns to right (if any)

    # Validate the column layout up front. Columns 1 (dose axis, usually
    # the stressor's own name) and 2 (the response - legacy workbooks use
    # alternate labels like 'Scaled Response Values 0 to 100') keep their
    # historical force-rename, but a SHIFTED sheet - where a limit/SD
    # header lands in column 2, or columns 3-5 are out of order - is now
    # a clear error instead of silently producing garbage.
    found <- trimws(colnames(dat))
    expected_345 <- c("SD", "low.limit", "up.limit")
    shifted <- found[2] %in% c(expected_345, "value")
    mismatch_345 <- which(found[3:5] != expected_345)
    if (shifted || length(mismatch_345) > 0) {
      stop(
        "Stressor-response worksheet '", sheet, "' has unexpected column ",
        "name(s)/order: ",
        if (shifted) paste0("column 2 is '", found[2], "'; ") else "",
        if (length(mismatch_345) > 0) {
          paste0("column ", mismatch_345 + 2, " is '",
                 found[3:5][mismatch_345], "' (expected '",
                 expected_345[mismatch_345], "')", collapse = "; ")
        } else "",
        ". Columns must be: '<stressor name>' (dose values), ",
        "'Mean System Capacity (%)' (the response), 'SD', 'low.limit', ",
        "'up.limit'.",
        call. = FALSE
      )
    }
    colnames(dat) <- c("value", "mean_system_capacity", "sd", "lwr", "upr")

    # Coerce to numeric early: one stray text cell (e.g. "TBD") otherwise
    # turns a whole column into text and surfaces far away as
    # "non-numeric argument to binary operator". Text cells become NA.
    for (cc in colnames(dat)) {
      dat[[cc]] <- suppressWarnings(as.numeric(as.character(dat[[cc]])))
    }
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
    # NOTE: matrix names are deliberately NOT appended to stressor_names /
    # pretty_names - they are exposed separately via ret_obj$MInt so that
    # regular-stressor consumers (dose alignment, curve loops) never see
    # them. The Shiny app lists them from names(sr_wb_dat$MInt).

    # Validate each matrix's axis stressor names against the Main sheet -
    # a typo or trailing space here previously surfaced only at run time
    # (as a crash or a silent skip)
    for (mn in names(mint_obj)) {
      axes <- c(mint_obj[[mn]]$Columns, mint_obj[[mn]]$Rows)
      missing_axes <- setdiff(axes, main_sheet$Stressors)
      if (length(missing_axes) > 0) {
        warning(
          "Interaction matrix sheet '", mn, "': axis stressor name(s) not ",
          "found in the Main sheet: ",
          paste(missing_axes, collapse = ", "),
          ". The matrix will be skipped at run time unless the names match.",
          call. = FALSE
        )
      }
    }

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
