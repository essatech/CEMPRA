#' Run SocioEconomic Cost Calculations
#'
#' @description Provides draft cost calculations for socioeconomic inputs.
#'
#' @details Internal function run within the Joe Model to perform socioeconomic calculations.
#'
#' @param socioeconomic_inputs list object of socioeconomic inputs returned from SocioEconomicWorkbook().
#' @param deterministic Boolean. Defaults to FALSE. If TRUE model will run without stochasticity.
#'
#' @returns A list object with cost and area summary calculations included
#'
#' @examples
#' \dontrun{
#' library(CEMPRA)
#' filename <- system.file("extdata", "socio_economic/socio_economic_input.xlsx", package = "CEMPRA")
#' socioeconomic_inputs <- SocioEconomicWorkbook(filename = filename)
#' socioeconomic_inputs <- SocioEconomicRun(socioeconomic_inputs = socioeconomic_inputs)
#'
#' }
#'
#' @export
SocioEconomicRun <- function(socioeconomic_inputs = NA, deterministic = FALSE) {

  #--------------------------------------------------
  # Merge all columns into a new implementation
  # table to simplify calculations
  #--------------------------------------------------

  # These calculations only need to be done once
  # so easy to do here...

  imp <- socioeconomic_inputs$`Location Implementation`

  colnames(imp) <- c(
    "id",
    "loc_name",
    "action",
    "measurement",
    "n_units",
    "sd_units",
    "low_units",
    "up_units",
    "cost_multiplier",
    "effect_multiplier"
  )

  # Merge on a modified cost dataframe
  cost <- socioeconomic_inputs$`Management Actions`
  colnames(cost) <- c(
    "action",
    "measurement",
    "cost_units",
    "sd_unit_cost",
    "low_unit_cost",
    "up_unit_cost",
    "bulk_thresh_1",
    "bulk_multi_1",
    "bulk_thresh_2",
    "bulk_multi_2"
  )
  cost$measurement <- NULL

  # merge to implmentation object
  imp_2 <-
    merge(imp,
          cost,
          by.x = "action",
          by.y = "action",
          all.x = TRUE)


  # Turn off SD values if running in deterministic mode
  if(deterministic) {
    imp_2$sd_units <- 0
    imp_2$sd_unit_cost <- 0
  }

  # --------------------------------
  # Mean cost calculations
  # --------------------------------

  # determine mean cost - should bulk discounts be applied

  imp_2$bulk_thresh_1 <-
    ifelse(is.na(imp_2$bulk_thresh_1),-9999, imp_2$bulk_thresh_1)
  imp_2$bulk_thresh_2 <-
    ifelse(is.na(imp_2$bulk_thresh_2),-9999, imp_2$bulk_thresh_2)

  imp_2$bulk_multi_1 <-
    ifelse(is.na(imp_2$bulk_multi_1), 1, imp_2$bulk_multi_1)
  imp_2$bulk_multi_2 <-
    ifelse(is.na(imp_2$bulk_multi_2), 1, imp_2$bulk_multi_2)

  imp_2$cost_units <- ifelse(
    imp_2$n_units >= imp_2$bulk_thresh_1,
    imp_2$bulk_multi_1 * imp_2$cost_units,
    imp_2$cost_units
  )

  imp_2$cost_units <- ifelse(
    imp_2$n_units >= imp_2$bulk_thresh_2,
    imp_2$bulk_multi_2 * imp_2$cost_units,
    imp_2$cost_units
  )

  # Apply the location-specific cost-multiplier
  imp_2$cost_multiplier <- ifelse(
    is.na(imp_2$cost_multiplier),
    1,
    imp_2$cost_multiplier
  )

  imp_2$cost_units <- ifelse(
    imp_2$cost_multiplier != 1,
    imp_2$cost_multiplier * imp_2$cost_units,
    imp_2$cost_units
  )

  # ---------------------------------------------------
  # Perform action cost calculations for this batch iteration
  # ---------------------------------------------------

  means_vec <- imp_2$cost_units
  sds_vec <- imp_2$sd_unit_cost
  low_vec <- imp_2$low_unit_cost
  up_vec <- imp_2$up_unit_cost

  SampleRange <- function(n, mean, sd, low_lim, up_lim) {
    samp <- stats::rnorm(n = n, mean = mean, sd = sd)
    samp <- ifelse(samp < low_lim, low_lim, samp)
    samp <- ifelse(samp > up_lim, up_lim, samp)
    return(samp)
  }

  # apply the stats::rnorm over a vector of means (mmeans) and sd values (msds)

  cost_samples <-
    mapply(
      SampleRange,
      n = 1,
      mean = means_vec,
      sd = sds_vec,
      low_lim = low_vec,
      up_lim = up_vec
    )

  imp_2$unit_cost_samples <- round(cost_samples, 2)

  # Create a vector of unit developments
  # how many

  means_vec <- imp_2$n_units
  sds_vec <- imp_2$sd_units
  low_vec <- imp_2$low_units
  up_vec <- imp_2$up_units


  unit_samples <-
    mapply(
      SampleRange,
      n = 1,
      mean = means_vec,
      sd = sds_vec,
      low_lim = low_vec,
      up_lim = up_vec
    )

  imp_2$unit_n_samples <- unit_samples

  if(deterministic) {
    # do not sample units in deterministic mode
    imp_2$unit_n_samples <- imp_2$n_units
  }


  # Multiply for program cost
  total_costs <- imp_2$unit_cost_samples * imp_2$unit_n_samples
  total_costs <- round(total_costs, 2)

  imp_2$total_cost <- round(total_costs, 2)


  # -------------------------------------------
  # Export attribute - cost_summary
  cost_summary <- imp_2
  # -------------------------------------------


  # Merge on a modified stressor reduction dataframe
  sr <- socioeconomic_inputs$`Stressor Reduction`
  colnames(sr) <- c(
    "sr_curve_id",
    "action",
    "measurement",
    "size_column",
    "scaled_units",
    "linked_stressor"
  )
  sr$measurement <- NULL

  # merge to implmentation object
  imp_3 <-
    merge(imp_2,
          sr,
          by = c("action", "action"),
          all.x = TRUE)

  # Merge on a modified location size reduction dataframe
  size <- socioeconomic_inputs$`Location Size Attributes`
  colnames(size)[1] <- "id"
  colnames(size)[2] <- "loc_name"
  size$loc_name <- NULL

  # Convert to long format
  vary_cross <- colnames(size)[2:ncol(size)]
  mlist <- list()

  for (ii in 1:ncol(size)) {
    if (colnames(size)[ii] == 'id') {
      next
    }
    df <- as.data.frame(size[, vary_cross[ii - 1]][, 1])
    size_vals <- as.numeric(df[, 1])
    size_var <- colnames(size)[ii]
    id <- size$id
    mlist[[ii]] <-
      data.frame(id, size_var = size_var, size_vals = size_vals)
  }

  mlist <- do.call("rbind", mlist)
  mlist$tmp_merge <- paste0(mlist$id, "__", mlist$size_var)
  mlist$id <- NULL

  imp_3$tmp_merge <- paste0(imp_3$id, "__", imp_3$size_column)


  # merge to implmentation object
  imp_4 <-
    merge(
      imp_3,
      mlist,
      by.x = "tmp_merge",
      by.y = "tmp_merge",
      all.x = TRUE,
      all.y =  FALSE
    )
  imp_4$tmp_merge <- NULL


  # Convert restoration action to implementation densities

  imp_4$rest_density <- imp_4$unit_n_samples / imp_4$size_vals


  # -------------------------------------------
  # Modify local restoration density by
  # location effect multiplier (effect_multiplier)
  # -------------------------------------------

  imp_4$effect_multiplier <- ifelse(is.na(imp_4$effect_multiplier), 1, imp_4$effect_multiplier)
  imp_4$rest_density <- imp_4$rest_density * imp_4$effect_multiplier


  # -------------------------------------------
  # Export attribute - restoration densities
  rest_density <- imp_4
  # -------------------------------------------




  # -------------------------------------------
  # Reference stressor reduction functions
  # look up values for level of restoration action
  # -------------------------------------------

  sr_functions <- unique(imp_4$sr_curve_id)

  # Loop through each stressor reduction function
  # to calculate the change in stressor levels at
  # each location

  imp_4$stressor_reductions <- NA

  for (i in 1:length(sr_functions)) {

    sr_id <- sr_functions[i]
    sr_curve <- sr[sr$sr_curve_id == sr_id, ]
    sr_dat <- socioeconomic_inputs$sr_curvs[[sr_id]]

    if(deterministic) {
      # Trun off SD if running with no stochastcitiy
      sr_dat$SD <- 0
    }


    # Get the actual levels of restoration action
    # Action in terms of densities

    m_action <- imp_4$rest_density
    m_action <- ifelse(imp_4$sr_curve_id == sr_id, m_action, NA)

    x_vals <- as.numeric(as.data.frame(sr_dat[, 1])[, 1])
    y_vals <- as.numeric(as.data.frame(sr_dat[, 2])[, 1])

    m_mean <- stats::approx(
      x = x_vals,
      y = y_vals,
      xout = m_action,
      rule = 2
    )$y

    y_vals <- sr_dat$SD

    m_sd <- stats::approx(
      x = x_vals,
      y = y_vals,
      xout = m_action,
      rule = 2
    )$y

    y_vals <- sr_dat$low.limit

    m_low <- stats::approx(
      x = x_vals,
      y = y_vals,
      xout = m_action,
      rule = 2
    )$y

    y_vals <- sr_dat$up.limit

    m_high <- stats::approx(
      x = x_vals,
      y = y_vals,
      xout = m_action,
      rule = 2
    )$y

    # Sample how stressor will change
    # apply the stats::rnorm over a vector of means (mmeans) and sd values (msds)

    sr_sample <- suppressWarnings({
      mapply(
        SampleRange,
        n = 1,
        mean = m_mean,
        sd = m_sd,
        low_lim = m_low,
        up_lim = m_high
      )
    })

    if(deterministic) {
      sr_sample <- m_mean
    }

    # Assign stressor reductions to parent dataframe
    imp_4$stressor_reductions <-
      ifelse(imp_4$sr_curve_id == sr_id,
             sr_sample,
             imp_4$stressor_reductions)

  }


  imp_4$stressor_reductions <-
    ifelse(is.na(imp_4$stressor_reductions),
           0,
           imp_4$stressor_reductions)


  # -------------------------------------------
  # Export attribute - change is stressor levels
  stressor_reductions <- imp_4
  # -------------------------------------------

  # assign new tables to outputs
  socioeconomic_inputs$cost_summary <- cost_summary
  socioeconomic_inputs$stressor_reductions <- stressor_reductions


  return(socioeconomic_inputs)


}
