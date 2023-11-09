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
#'
#'
#' @export
#'
interaction_matrix_sys_cap <- function(MInt, sc.dose.df, adult_sys_cap) {

  # interaction_matrix_sys_cap() should be run with apply on sr_wb_dat$MInt

  col_var <- MInt$Columns
  row_var <- MInt$Rows

  # Check if adult filter should be used
  if(adult_sys_cap) {
    if(MInt$Life_stages != "adult") {
      return(sc.dose.df)
    }
  }

  mmat <- MInt$mat_msc
  col_vals <- colnames(mmat)
  col_vals <- col_vals[2:length(col_vals)]
  col_vals <- as.numeric(as.character(col_vals))

  row_vals <- unlist(mmat[, 1])
  row_vals <- as.numeric(as.character(row_vals))
  names(row_vals) <- NULL

  # Extract and clean inner matrix
  Z <- mmat[, 2:ncol(mmat)]
  Z <- as.matrix(Z)
  colnames(Z) <- NULL
  rownames(Z) <- NULL


  # Get dose values across HUCs and simulations
  dose_mat <- sc.dose.df[sc.dose.df$Stressor %in% c(col_var, row_var), ]

  # If main effect variables are unavailable or
  # already used in another matrix return original (skip)
  if(nrow(dose_mat) == 0) {
    print("Main effects not available...")
    return(sc.dose.df)
  }

  # Prep format
  dose_mat_wide <- reshape2::dcast(dose_mat, HUC + simulation ~ Stressor, value.var = "dose")

  # col values (x)
  xp <- dose_mat_wide[, col_var]
  # row values (y)
  yp <- dose_mat_wide[, row_var]

  # Fix min and max bounds
  xp <- ifelse(xp > max(col_vals), max(col_vals), xp)
  xp <- ifelse(xp < min(col_vals), min(col_vals), xp)
  yp <- ifelse(yp > max(row_vals), max(row_vals), yp)
  yp <- ifelse(yp < min(row_vals), min(row_vals), yp)

  # Run two-dimensional Data Interpolation
  preds <- pracma::interp2(x = col_vals, y = row_vals, Z = Z, xp = xp, yp = yp, method = "linear")

  # Get the SD (if any) and resample
  mmat_sd <- MInt$mat_sd
  Z_sd <- mmat_sd[, 2:ncol(mmat_sd)]
  Z_sd <- as.matrix(Z_sd)
  colnames(Z_sd) <- NULL
  rownames(Z_sd) <- NULL

  preds_sd <- pracma::interp2(x = col_vals, y = row_vals, Z = Z_sd, xp = xp, yp = yp, method = "linear")

  # Get the lower limit
  mmat <- MInt$mat_ll
  Z <- mmat[, 2:ncol(mmat)]
  Z <- as.matrix(Z)
  colnames(Z) <- NULL
  rownames(Z) <- NULL
  preds_ll <- pracma::interp2(x = col_vals, y = row_vals, Z = Z, xp = xp, yp = yp, method = "linear")

  # Get the upper limit
  mmat <- MInt$mat_ul
  Z <- mmat[, 2:ncol(mmat)]
  Z <- as.matrix(Z)
  colnames(Z) <- NULL
  rownames(Z) <- NULL
  preds_ul <- pracma::interp2(x = col_vals, y = row_vals, Z = Z, xp = xp, yp = yp, method = "linear")

  # calculate system.capacity vector (x.dose is a vector)
  sys.capacity <- tbeta_rnd(
    mn_est = preds/100,
    sd_est = preds_sd/100,
    low.limit = preds_ll/100,
    up.limit = preds_ul/100
  )

  sys.capacity <- round(sys.capacity, 4)


  # Update the corresponding data objects
  mat_data <- data.frame(
    HUC = dose_mat_wide$HUC,
    Stressor = MInt$Matrix_Name,
    simulation = dose_mat_wide$simulation,
    dose = NA,
    sys.cap = sys.capacity
  )

  # Determine if main effects should be included or excluded
  if(MInt$Main_Effect == "Included") {
    # Drop main effects
    sc.dose.df.ret <- sc.dose.df[!(sc.dose.df$Stressor %in% c(col_var, row_var)), ]
    sc.dose.df.ret <- rbind(sc.dose.df.ret, mat_data)
  }

  if(MInt$Main_Effect == "Excluded") {
    # Keep main effects
    sc.dose.df.ret <- sc.dose.df
    sc.dose.df.ret <- rbind(sc.dose.df.ret, mat_data)
  }

  return(sc.dose.df.ret)


}
