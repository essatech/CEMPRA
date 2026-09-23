#' Align Dose (Stressor Magnitude) Names with Stressor-Response Names
#'
#' @description Renames stressor magnitude rows to the full
#' `"<Stressor>_<Stressor_cat>"` form when - and only when - the
#' stressor-response workbook uses those concatenated names.
#'
#' @details `StressorResponseWorkbook()` resolves duplicate stressor names by
#' renaming rows to `"<Stressor>_<Stressor_cat>"` when each copy carries a
#' distinct, non-blank `Stressor_cat`. The stressor magnitude workbook keeps
#' its short names on load (legacy workbooks legitimately repeat a stressor
#' name across component rows that feed a single curve), so this helper
#' aligns the two at model run time. A dose row is renamed only when its
#' short name does NOT match any stressor-response name AND the concatenated
#' name DOES. Rows whose short name already matches (including legacy
#' component-row workbooks) are left untouched, as are rows tagged with the
#' reserved category `"Total_Mortality"`. A warning is issued whenever
#' `Stressor_cat` is used to resolve a name, as this is expected to be rare.
#'
#' Called automatically by \code{\link{JoeModel_Run}} and
#' \code{\link{PopulationModel_Run}}.
#'
#' @param dose Data frame. Stressor magnitude data (from
#' \code{\link{StressorMagnitudeWorkbook}}).
#' @param sr_names Character vector of (final) stressor names from the
#' stressor-response workbook main sheet.
#'
#' @returns The dose data frame with aligned stressor names.
#'
#' @export
align_dose_stressor_names <- function(dose = NULL, sr_names = NULL) {

  if (is.null(dose) || !(is.data.frame(dose))) {
    return(dose)
  }
  if (is.null(sr_names) || length(sr_names) == 0) {
    return(dose)
  }
  if (!("Stressor_cat" %in% colnames(dose))) {
    return(dose)
  }

  nm_v <- trimws(as.character(dose$Stressor))
  cat_v <- trimws(as.character(dose$Stressor_cat))
  cand <- paste(nm_v, cat_v, sep = "_")

  fix <- !(is.na(nm_v)) & !(nm_v %in% sr_names) &
    (cand %in% sr_names) &
    !(is.na(cat_v)) & cat_v != "" &
    cat_v != "Total_Mortality"

  if (any(fix)) {
    dose$Stressor[fix] <- cand[fix]
    warning(
      "Stressor_cat was used to resolve stressor name(s) between the ",
      "stressor magnitude and stressor-response workbooks: ",
      paste(unique(cand[fix]), collapse = ", "),
      ". This happens when the stressor-response Main sheet disambiguates ",
      "duplicate stressor names as '<Stressor>_<Stressor_cat>'.",
      call. = FALSE
    )
  }

  return(dose)
}
