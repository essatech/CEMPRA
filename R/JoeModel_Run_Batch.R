#' Batch Joe Model Run
#'
#' Batch version of JoeModel_Run across scenarios input file.
#'
#' @details Runs the Joe Model for cumulative system capacity
#' across multiple scenarios. See vignette for custom input file format.
#'
#' @param scenarios dataframe. Custom scenarios dataframe with columns `Scenario` (scenario ID),
#' `Stressor` (stressor name), `Metric` (either Mean or SD) and `Multiplier` (a numeric multiplier).
#' @param dose dataframe. Stressor magnitude file exported from StressorMagnitudeWorkbook().
#' @param sr_wb_dat list object. Stressor response workbook returned from StressorResponseWorkbook().
#' @param MC_sims numeric. set number of Monte Carlo simulations for the Joe Model. set number
#' of Monte Carlo simulations for the Joe model.
#'
#' @returns A dataframe of system capacity results for each HUC and scenario.
#'
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' library(CEMPRA)
#' library(dplyr)
#' library(ggplot2)
#'
#' # Import stressor-magnitude workbook
#' filename_rm <- system.file("extdata", "stressor_magnitude_unc_ARTR.xlsx", package = "CEMPRA")
#' stressor_magnitude <- StressorMagnitudeWorkbook(filename = filename_rm,
#' scenario_worksheet = "natural_unc")
#'
#' # Import stressor-response workbook
#' filename_sr <- system.file("extdata", "stressor_response_fixed_ARTR.xlsx", package = "CEMPRA")
#' stressor_response <- StressorResponseWorkbook(filename = filename_sr)
#'
#' # Import the custom scenarios workbook
#' filename_batch <- system.file("extdata", "scenario_batch_joe.xlsx", package = "CEMPRA")
#' scenarios <- readxl::read_excel(filename_batch)
#'
#'
#' # Run the Joe Model with batch scenarios
#' exp_dat <- JoeModel_Run_Batch(scenarios = scenarios, dose = stressor_magnitude,
#'  sr_wb_dat = stressor_response, MC_sims = 3)
#'
#' #-----------------------------------------------------
#' # Plot result for single HUC - filter by name or ID
#' hub_sub <- exp_dat[exp_dat$HUC == 1701010204, ]
#' plot_title <- paste0("Watershed Name - ", 1701010204)
#'
#' # Add mean and error bars (SD)
#' hub_sb <- hub_sub %>%
#'   group_by(Scenario) %>%
#'   summarise(
#'     mean = mean(CE, na.rm = TRUE),
#'     sd = sd(CE, na.rm = TRUE)
#'   )
#'
#' hub_sb$lower <- hub_sb$mean - hub_sb$sd
#' hub_sb$upper <- hub_sb$mean + hub_sb$sd
#'
#' ggplot(hub_sb, aes(x = mean, y = Scenario, xmin = lower, xmax = upper)) +
#'   geom_rect(aes(xmin = 0, xmax = 0.2, ymin = -Inf, ymax = Inf),
#'     fill = "pink", alpha = 0.02
#'   ) +
#'   geom_rect(aes(xmin = 0.2, xmax = 0.5, ymin = -Inf, ymax = Inf),
#'     fill = "orange", alpha = 0.02
#'   ) +
#'   geom_rect(aes(xmin = 0.5, xmax = 0.75, ymin = -Inf, ymax = Inf),
#'     fill = "yellow", alpha = 0.02
#'   ) +
#'   geom_rect(aes(xmin = 0.75, xmax = 1, ymin = -Inf, ymax = Inf),
#'     fill = "green", alpha = 0.02
#'   ) +
#'   geom_point() +
#'   geom_errorbarh(height = .2) +
#'   ggtitle(plot_title) +
#'   xlab("System Capacity (0 - 1)") +
#'   ylab("Recovery Scenario") +
#'   theme_bw()
#'
#' #-----------------------------------------------------
#' # Plot result for single scenario
#' sen_sub <- exp_dat[exp_dat$Scenario == "scenario_1", ]
#' plot_title <- paste0("Scenario 1 ...")
#'
#' # Add mean and error bars (SD)
#' sen_sb <- sen_sub %>%
#'   group_by(HUC) %>%
#'   summarise(
#'     mean = mean(CE, na.rm = TRUE),
#'     sd = sd(CE, na.rm = TRUE)
#'   )
#'
#' sen_sb$lower <- sen_sb$mean - sen_sb$sd
#' sen_sb$upper <- sen_sb$mean + sen_sb$sd
#'
#' sen_sb$HUC <- as.character(sen_sb$HUC)
#'
#' ggplot(sen_sb, aes(x = mean, y = HUC, xmin = lower, xmax = upper)) +
#'   geom_rect(aes(xmin = 0, xmax = 0.2, ymin = -Inf, ymax = Inf),
#'     fill = "pink", alpha = 0.02
#'   ) +
#'   geom_rect(aes(xmin = 0.2, xmax = 0.5, ymin = -Inf, ymax = Inf),
#'     fill = "orange", alpha = 0.02
#'   ) +
#'   geom_rect(aes(xmin = 0.5, xmax = 0.75, ymin = -Inf, ymax = Inf),
#'     fill = "yellow", alpha = 0.02
#'   ) +
#'   geom_rect(aes(xmin = 0.75, xmax = 1, ymin = -Inf, ymax = Inf),
#'     fill = "green", alpha = 0.02
#'   ) +
#'   geom_point() +
#'   geom_errorbarh(height = .2) +
#'   ggtitle(plot_title) +
#'   xlab("System Capacity (0 - 1)") +
#'   ylab("Recovery Scenario") +
#'   theme_bw()
#' }
#'
#' @export
JoeModel_Run_Batch <-
  function(scenarios = NA,
           dose = NA,
           sr_wb_dat = NA,
           MC_sims = 10) {
    # Create object to save results
    all_results <- list()
    counter <- 1

    # Save the raw original sm
    raw_sm <- dose

    # Loop through scenarios
    sims <- unique(scenarios$Scenario)

    if (length(sims) < 1) {
      stop("The scenario column must be populated")
    }

    for (s in 1:length(sims)) {

      this_sim <- sims[s]
      paste0("running scenario... ", this_sim)

      # Adjust stressor magnitude values
      adj_sm <- raw_sm

      u_stressors <- unique(adj_sm$Stressor)

      # Look at all adjustments to be made
      adjustments <- scenarios[scenarios$Scenario == this_sim, ]

      # Implement all adjustments
      for (a in 1:nrow(adjustments)) {

        this_adj <- adjustments[a, ]

        if (!(this_adj$Metric %in% c("Mean", "SD", "Low_Limit", "Up_Limit"))) {
          stop("Metric column must be either Mean, SD, Low_Limit, or Up_Limit.")
        }

        multi <- as.numeric(as.character(this_adj$Multiplier))

        if (is.na(multi)) {
          message(this_adj)
          stop("Multiplier must be a number...")
        }

        # Make sure stressor is in list
        strss <- as.character(this_adj$Stressor)

        if (!(strss %in% u_stressors)) {
          warning(paste0(
            "No instances of ",
            strss,
            " in stressor magnitude datase..."
          ))
        }

        # Make adjustments with multiplier
        if (this_adj$Metric == "Mean") {
          org_vals <- adj_sm$Mean[which(adj_sm$Stressor == strss)]
          adj_sm$Mean[which(adj_sm$Stressor == strss)] <-
            org_vals * multi
        }

        # Make adjustments with multiplier
        if (this_adj$Metric == "SD") {
          org_vals <- adj_sm$SD[which(adj_sm$Stressor == strss)]
          adj_sm$SD[which(adj_sm$Stressor == strss)] <-
            org_vals * multi
        }

        # Make adjustments with multiplier
        if (this_adj$Metric == "Low_Limit") {
          org_vals <- adj_sm$Low_Limit[which(adj_sm$Stressor == strss)]
          adj_sm$Low_Limit[which(adj_sm$Stressor == strss)] <-
            org_vals * multi
        }
        # Make adjustments with multiplier
        if (this_adj$Metric == "Up_Limit") {
          org_vals <- adj_sm$Up_Limit[which(adj_sm$Stressor == strss)]
          adj_sm$Up_Limit[which(adj_sm$Stressor == strss)] <-
            org_vals * multi
        }
      }

      # End of adjustments - run scenario in joe model
      # run with adjusted values
      jmr <-
        JoeModel_Run(
          dose = adj_sm,
          sr_wb_dat = sr_wb_dat,
          MC_sims = MC_sims
        )

      # Just take the core sys capacity results
      results <- jmr$ce.df

      # Add name on for reference
      results$Scenario <- this_sim

      # Create object to save results
      all_results[[counter]] <- results
      counter <- 1 + counter
    }

    exp_dat <- do.call("rbind", all_results)

    return(exp_dat)
  }
