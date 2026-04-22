#' Population Model LTRE
#'
#' @description Run a LTRE for the population model
#'
#' @details In prep .... step_size = 0.05
#'
#' @param step_size numeric. step size for ltre experiment parameter adjustment.
#' @param dose dataframe. Stressor magnitude dataset
#'  imported from StressorMagnitudeWorkbook().
#' @param sr_wb_dat list object. Stressor response workbook
#' imported from StressorResponseWorkbook().
#' @param life_cycle_params dataframe. Life cycle parameters.
#' @param HUC_ID character. HUC_ID for the location unit. Can only be one HUC_ID (not an array).
#' @param n_reps numeric. Number of replicates.
#'  Carlo simulations for the Population Model.
#' @param stressors (optional) character vector of stressor
#'  names to include in the Population Model. Leave the default
#'  value as NA if you wish to include all stressors
#'  applicable to the population model.
#' @param habitat_dd_k (optional) dataframe of location and stage-specific habitat capacity k values for the target species. If used this dataframe will override the capacity estimates
#'
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' library(CEMPRA)
#' # in prep
#' }
#'
#' @export
pop_model_ltre <- function(step_size = 0.05,
                           dose = NA,
                           sr_wb_dat = NA,
                           life_cycle_params = NA,
                           HUC_ID = NA,
                           n_reps = 100,
                           stressors = NA,
                           habitat_dd_k = NULL) {


  n_stage <- life_cycle_params$Value[life_cycle_params$Name == "Nstage"]
  n_stage <- as.numeric(n_stage)
  lcm_clean <- pop_model_dat_clean(dat = life_cycle_params, nstage_fill = n_stage)


  MC_sims <- 1
  n_years <- n_reps


  # Function to calculate lambda for a list of matrices
  calculate_lambdas <- function(matrix_list) {
    # Apply lambda function to each matrix in the list
    lapply(matrix_list, function(A) {
      tryCatch({
        popbio::lambda(A)  # Calculate lambda
      }, error = function(e) {
        NA  # Return NA if there's an error
      })
    })
  }


  # Perform a status_quo run
  status_quo <- PopulationModel_Run(
    dose = dose,
    sr_wb_dat = sr_wb_dat,
    life_cycle_params = lcm_clean,
    HUC_ID = HUC_ID,
    n_years = n_years,
    MC_sims = MC_sims,
    output_type = "qa_matrices",
    # Make sure matricies are returned
    habitat_dd_k = habitat_dd_k
  )

  # get the global mean lambda
  ce_lambda_global <- unlist(calculate_lambdas(status_quo$ce))
  baseline_lambda_global <- unlist(calculate_lambdas(status_quo$baseline))

  mean_ce_lambda_global <- exp(mean(log(ce_lambda_global), na.rm = TRUE))
  mean_baseline_lambda_global <- exp(mean(log(baseline_lambda_global), na.rm = TRUE))



  # Adjustable parameters
  adj_params <- c(
    "SE",
    "S0",
    "SR",
    paste0("surv_", 1:n_stage),
    paste0("u_", 1:n_stage),
    paste0("smig_", 1:n_stage),
    paste0("mat_", 1:n_stage),
    paste0("eps_", 1:n_stage),
    "eps_sd",
    "egg_rho",
    "M.cv",
    "M.rho"
  )

  # Loop though parameters and increase and decrease parameter by step size.
  export_list_all <- list()
  export_list_mean <- list()

  for (p in 1:length(adj_params)) {
    this_param <- adj_params[p]

    message(paste0(
      "Working on ",
      this_param,
      ", p = ",
      p,
      " of ",
      length(adj_params),
      "..."
    ))

    # Get value
    p_value <- lcm_clean$Value[lcm_clean$Name == this_param]

    # Skip if non-existent, NA or 0,
    if (length(p_value) == 0) {
      next
    }
    if (is.na(p_value)) {
      next
    }
    if (p_value == 0) {
      next
    }

    # Increase and decrease by step size
    p_value_plus <- p_value + (step_size * p_value)
    p_value_minus <- p_value - (step_size * p_value)

    # Make sure never less than zero
    p_value_plus <- max(p_value_plus, 0)
    p_value_minus <- max(p_value_minus, 0)

    # Make sure never greater than 1.0 unless eps_ parameter
    if (!(grepl("eps_|M.cv", this_param))) {
      p_value_plus <- min(p_value_plus, 1.0)
      p_value_minus <- min(p_value_minus, 1.0)
    }

    # Skip if value is the same
    if (p_value_plus == p_value_minus) {
      next
    }


    # ---------------------------------------------
    # Adjust value increase
    # ---------------------------------------------
    lcm_sim <- lcm_clean
    lcm_sim$Value[lcm_sim$Name == this_param] <- p_value_plus

    p_increase <- PopulationModel_Run(
      dose = dose,
      sr_wb_dat = sr_wb_dat,
      life_cycle_params = lcm_sim,
      HUC_ID = HUC_ID,
      n_years = n_years,
      MC_sims = MC_sims,
      output_type = "qa_matrices",
      # Make sure matricies are returned
      habitat_dd_k = habitat_dd_k
    )

    ce_lambda_increase <- unlist(calculate_lambdas(p_increase$ce))
    baseline_lambda_increase <- unlist(calculate_lambdas(p_increase$baseline))


    # ---------------------------------------------
    # Adjust value decrease
    # ---------------------------------------------
    lcm_sim <- lcm_clean
    lcm_sim$Value[lcm_sim$Name == this_param] <- p_value_minus

    p_decrease <- PopulationModel_Run(
      dose = dose,
      sr_wb_dat = sr_wb_dat,
      life_cycle_params = lcm_sim,
      HUC_ID = HUC_ID,
      n_years = n_years,
      MC_sims = MC_sims,
      output_type = "qa_matrices",
      # Make sure matricies are returned
      habitat_dd_k = habitat_dd_k
    )

    ce_lambda_decrease <- unlist(calculate_lambdas(p_decrease$ce))
    baseline_lambda_decrease <- unlist(calculate_lambdas(p_decrease$baseline))


    # Build export data frames with stochastic
    df1 <- data.frame(
      this_param = this_param,
      value_default = p_value,
      value_adj = p_value_plus,
      direction = "increase",
      ce = "ce",
      lambda = ce_lambda_increase
    )
    df2 <- data.frame(
      this_param = this_param,
      value_default = p_value,
      value_adj = p_value_plus,
      direction = "increase",
      ce = "baseline",
      lambda = baseline_lambda_increase
    )
    df3 <- data.frame(
      this_param = this_param,
      value_default = p_value,
      value_adj = p_value_minus,
      direction = "decrease",
      ce = "ce",
      lambda = ce_lambda_decrease
    )
    df4 <- data.frame(
      this_param = this_param,
      value_default = p_value,
      value_adj = p_value_minus,
      direction = "decrease",
      ce = "baseline",
      lambda = baseline_lambda_decrease
    )
    # Merge dfs togehter
    df <- rbind(df1, df2, df3, df4)

    export_list_all[[p]] <- df

    # Build static geometruc mean data frame
    df1 <- data.frame(
      this_param = this_param,
      value_default = p_value,
      value_adj = p_value_plus,
      direction = "increase",
      ce = "ce",
      lambda = exp(mean(log(
        ce_lambda_increase
      ), na.rm = TRUE))
    )
    df2 <- data.frame(
      this_param = this_param,
      value_default = p_value,
      value_adj = p_value_plus,
      direction = "increase",
      ce = "baseline",
      lambda = exp(mean(
        log(baseline_lambda_increase), na.rm = TRUE
      ))
    )
    df3 <- data.frame(
      this_param = this_param,
      value_default = p_value,
      value_adj = p_value_minus,
      direction = "decrease",
      ce = "ce",
      lambda = exp(mean(log(
        ce_lambda_decrease
      ), na.rm = TRUE))
    )
    df4 <- data.frame(
      this_param = this_param,
      value_default = p_value,
      value_adj = p_value_minus,
      direction = "decrease",
      ce = "baseline",
      lambda = exp(mean(
        log(baseline_lambda_decrease), na.rm = TRUE
      ))
    )
    # Merge dfs togehter
    df <- rbind(df1, df2, df3, df4)

    export_list_mean[[p]] <- df


  }

  all_dat_mean <- do.call("rbind", export_list_mean)
  all_dat_all <- do.call("rbind", export_list_all)

  # =======================================
  # Create output table of summaries
  # =======================================

  # split out summary table
  all_dat_mean_ce <- all_dat_mean[all_dat_mean$ce == "ce", ]
  all_dat_mean_baseline <- all_dat_mean[all_dat_mean$ce == "baseline", ]

  # summarize percent change
  all_dat_mean_ce$lambda_change <- (all_dat_mean_ce$lambda / mean_ce_lambda_global) - 1
  all_dat_mean_baseline$lambda_change <- (all_dat_mean_baseline$lambda / mean_baseline_lambda_global) - 1

  # fix order
  all_dat_mean_baseline <- all_dat_mean_baseline[rev(order(all_dat_mean_baseline$lambda_change)), ]
  all_dat_mean_ce <- all_dat_mean_ce[rev(order(all_dat_mean_ce$lambda_change)), ]

  # =======================================
  # Return outputs to user
  # =======================================

  ret_obj <- list(
    ltre_summary_baseline = all_dat_mean_baseline,
    ltre_summary_ce = all_dat_mean_ce,
    mean_ce_lambda = mean_ce_lambda_global,
    mean_baseline_lambda = mean_baseline_lambda_global,
    all_dat_stochastic = all_dat_all
  )

  return(ret_obj)





}
