#' Interaction Matrix System Capacity
#'
#' @description Use run a 2D linear interpolation with interaction matrix to
#' calculate system capacity.
#'
#' @details Runs through an individual MInt_ worksheet object supplied from
#' sr_wb_dat$MInt and calculates system capacity. interaction_matrix_sys_cap()
#' should be run with apply on sr_wb_dat$MInt. Returns sc.dose.df with updated
#' system capacity and filtered for variables.
#'
#'
#' @param MInt One item from object returned from StressorResponseWorkbook()
#' @param sc.dose.df Data frame with columns HUC, Stressor, simulation, dose
#' and sys.cap.
#' @param adult_sys_cap Should the Joe Model be run only with variables
#' identified for adult system capacity.
#' @param keep_main_effects (optional) character vector of main-effect
#' stressor names that must stay in the returned sc.dose.df even when this
#' matrix has Main_Effect = "Included" (used by JoeModel_Run when the caller
#' explicitly selected those stressors). Defaults to none.
#'
#' @returns Updated sc.dose.df data frame with system capacity values
#'
#'
#' @export
#'
interaction_matrix_sys_cap <- function(MInt, sc.dose.df, adult_sys_cap,
                                       keep_main_effects = character(0)) {

  # interaction_matrix_sys_cap() should be run with apply on sr_wb_dat$MInt

  m_name <- as.character(MInt$Matrix_Name)

  # Axis stressor names - trimmed so a stray space in the worksheet cell
  # cannot cause a mismatch against the dose data
  col_var <- trimws(as.character(MInt$Columns))
  row_var <- trimws(as.character(MInt$Rows))

  # Check if adult filter should be used (case/whitespace-insensitive;
  # a missing Life_stages defaults to adult at load)
  if (adult_sys_cap) {
    life_stage <- tolower(trimws(as.character(MInt$Life_stages)))
    if (!is.na(life_stage) && nchar(life_stage) > 0 && life_stage != "adult") {
      return(sc.dose.df)
    }
  }

  # Both axis stressors must be present in the dose data - a partial match
  # previously crashed downstream with "undefined columns selected"
  have_axis <- c(col_var, row_var) %in% unique(as.character(sc.dose.df$Stressor))
  if (!all(have_axis)) {
    message(
      "Interaction matrix '", m_name, "' skipped: axis stressor(s) not ",
      "available in the dose data: ",
      paste(c(col_var, row_var)[!have_axis], collapse = ", ")
    )
    return(sc.dose.df)
  }

  # Axis dose values from the mean matrix
  mmat <- MInt$mat_msc
  col_vals <- colnames(mmat)
  col_vals <- col_vals[2:length(col_vals)]
  col_vals <- suppressWarnings(as.numeric(as.character(col_vals)))

  row_vals <- unlist(mmat[, 1])
  row_vals <- suppressWarnings(as.numeric(as.character(row_vals)))
  names(row_vals) <- NULL

  # Drop non-numeric axis entries (e.g., stray note columns picked up by
  # the loader) and sort both axes ascending - pracma::interp2 requires
  # non-decreasing axis vectors and previously crashed on matrices
  # authored with descending dose values
  keep_c <- which(!is.na(col_vals))
  keep_r <- which(!is.na(row_vals))
  if (length(keep_c) < length(col_vals) || length(keep_r) < length(row_vals)) {
    message(
      "Interaction matrix '", m_name, "': dropping ",
      (length(col_vals) - length(keep_c)) + (length(row_vals) - length(keep_r)),
      " non-numeric axis entr(ies)."
    )
  }
  if (length(keep_c) < 2 || length(keep_r) < 2) {
    message("Interaction matrix '", m_name,
            "' skipped: fewer than two numeric dose values on an axis.")
    return(sc.dose.df)
  }
  ord_c <- keep_c[order(col_vals[keep_c])]
  ord_r <- keep_r[order(row_vals[keep_r])]
  col_vals <- col_vals[ord_c]
  row_vals <- row_vals[ord_r]

  # Get dose values across HUCs and simulations
  dose_mat <- sc.dose.df[sc.dose.df$Stressor %in% c(col_var, row_var), ]

  # Duplicate rows for the same HUC/simulation/stressor would make dcast()
  # aggregate doses to row COUNTS (garbage values) - keep the first row
  dup_rows <- duplicated(dose_mat[, c("HUC", "simulation", "Stressor")])
  if (any(dup_rows)) {
    message(
      "Interaction matrix '", m_name, "': duplicate dose rows detected ",
      "for the same HUC/simulation/stressor - using the first row of each."
    )
    dose_mat <- dose_mat[!dup_rows, ]
  }

  # Prep format
  dose_mat_wide <- reshape2::dcast(dose_mat, HUC + simulation ~ Stressor,
                                   value.var = "dose")

  # col values (x)
  xp <- dose_mat_wide[, col_var]
  # row values (y)
  yp <- dose_mat_wide[, row_var]

  # Fix min and max bounds
  xp <- ifelse(xp > max(col_vals), max(col_vals), xp)
  xp <- ifelse(xp < min(col_vals), min(col_vals), xp)
  yp <- ifelse(yp > max(row_vals), max(row_vals), yp)
  yp <- ifelse(yp < min(row_vals), min(row_vals), yp)

  # NA doses cannot be interpolated (interp2 errors out) - those
  # HUC/simulation combinations get NA system capacity instead
  ok <- !is.na(xp) & !is.na(yp)
  if (any(!ok)) {
    message(
      "Interaction matrix '", m_name, "': NA dose values for ", sum(!ok),
      " HUC-simulation combination(s) - matrix system capacity is NA there."
    )
  }

  # Run two-dimensional Data Interpolation over one block (mean, SD,
  # low limit or up limit), aligned to the cleaned/sorted axes
  interp_surface <- function(block) {
    Z <- as.matrix(block[, 2:ncol(block)])
    colnames(Z) <- NULL
    rownames(Z) <- NULL
    Z <- Z[ord_r, ord_c, drop = FALSE]
    mode(Z) <- "numeric"
    out <- rep(NA_real_, length(xp))
    if (any(ok)) {
      out[ok] <- pracma::interp2(
        x = col_vals, y = row_vals, Z = Z,
        xp = xp[ok], yp = yp[ok], method = "linear"
      )
    }
    out
  }

  preds <- interp_surface(MInt$mat_msc)
  preds_sd <- interp_surface(MInt$mat_sd)
  preds_ll <- interp_surface(MInt$mat_ll)
  preds_ul <- interp_surface(MInt$mat_ul)

  # calculate system.capacity vector (x.dose is a vector)
  sys.capacity <- tbeta_rnd(
    mn_est = preds / 100,
    sd_est = preds_sd / 100,
    low.limit = preds_ll / 100,
    up.limit = preds_ul / 100
  )

  sys.capacity <- round(sys.capacity, 4)


  # Update the corresponding data objects
  mat_data <- data.frame(
    HUC = dose_mat_wide$HUC,
    Stressor = m_name,
    simulation = dose_mat_wide$simulation,
    dose = NA,
    sys.cap = sys.capacity
  )

  # Determine if main effects should be included or excluded
  main_effect <- tolower(trimws(MInt$Main_Effect))
  if (is.na(main_effect)) main_effect <- "included"

  if (main_effect == "excluded") {
    # Keep main effects alongside the interaction matrix result
    sc.dose.df.ret <- sc.dose.df
    sc.dose.df.ret <- rbind(sc.dose.df.ret, mat_data)
  } else {
    # Default: main effects are included in the matrix, so drop standalone
    # main effects to avoid double-counting - except any the caller
    # explicitly asked to keep (see JoeModel_Run's `stressors` argument)
    drop_vars <- setdiff(c(col_var, row_var), keep_main_effects)
    sc.dose.df.ret <- sc.dose.df[!(sc.dose.df$Stressor %in% drop_vars), ]
    sc.dose.df.ret <- rbind(sc.dose.df.ret, mat_data)
  }

  return(sc.dose.df.ret)


}
