# Batch Joe Model Run

Batch version of JoeModel_Run across scenarios input file.

## Usage

``` r
JoeModel_Run_Batch(scenarios = NA, dose = NA, sr_wb_dat = NA, MC_sims = 10)
```

## Arguments

- scenarios:

  dataframe. Custom scenarios dataframe with columns `Scenario`
  (scenario ID), `Stressor` (stressor name), `Metric` (either Mean or
  SD) and `Multiplier` (a numeric multiplier).

- dose:

  dataframe. Stressor magnitude file exported from
  StressorMagnitudeWorkbook().

- sr_wb_dat:

  list object. Stressor response workbook returned from
  StressorResponseWorkbook().

- MC_sims:

  numeric. set number of Monte Carlo simulations for the Joe Model. set
  number of Monte Carlo simulations for the Joe model.

## Value

A dataframe of system capacity results for each HUC and scenario.

## Details

Runs the Joe Model for cumulative system capacity across multiple
scenarios. See vignette for custom input file format.

## Examples

``` r
if (FALSE) { # \dontrun{
library(CEMPRA)
library(dplyr)
library(ggplot2)

# Import stressor-magnitude workbook
filename_rm <- system.file("extdata", "stressor_magnitude_unc_ARTR.xlsx", package = "CEMPRA")
stressor_magnitude <- StressorMagnitudeWorkbook(filename = filename_rm,
scenario_worksheet = "natural_unc")

# Import stressor-response workbook
filename_sr <- system.file("extdata", "stressor_response_fixed_ARTR.xlsx", package = "CEMPRA")
stressor_response <- StressorResponseWorkbook(filename = filename_sr)

# Import the custom scenarios workbook
filename_batch <- system.file("extdata", "scenario_batch_joe.xlsx", package = "CEMPRA")
scenarios <- readxl::read_excel(filename_batch)


# Run the Joe Model with batch scenarios
exp_dat <- JoeModel_Run_Batch(scenarios = scenarios, dose = stressor_magnitude,
 sr_wb_dat = stressor_response, MC_sims = 3)

#-----------------------------------------------------
# Plot result for single HUC - filter by name or ID
hub_sub <- exp_dat[exp_dat$HUC == 1701010204, ]
plot_title <- paste0("Watershed Name - ", 1701010204)

# Add mean and error bars (SD)
hub_sb <- hub_sub %>%
  group_by(Scenario) %>%
  summarise(
    mean = mean(CE, na.rm = TRUE),
    sd = sd(CE, na.rm = TRUE)
  )

hub_sb$lower <- hub_sb$mean - hub_sb$sd
hub_sb$upper <- hub_sb$mean + hub_sb$sd

ggplot(hub_sb, aes(x = mean, y = Scenario, xmin = lower, xmax = upper)) +
  geom_rect(aes(xmin = 0, xmax = 0.2, ymin = -Inf, ymax = Inf),
    fill = "pink", alpha = 0.02
  ) +
  geom_rect(aes(xmin = 0.2, xmax = 0.5, ymin = -Inf, ymax = Inf),
    fill = "orange", alpha = 0.02
  ) +
  geom_rect(aes(xmin = 0.5, xmax = 0.75, ymin = -Inf, ymax = Inf),
    fill = "yellow", alpha = 0.02
  ) +
  geom_rect(aes(xmin = 0.75, xmax = 1, ymin = -Inf, ymax = Inf),
    fill = "green", alpha = 0.02
  ) +
  geom_point() +
  geom_errorbarh(height = .2) +
  ggtitle(plot_title) +
  xlab("System Capacity (0 - 1)") +
  ylab("Recovery Scenario") +
  theme_bw()

#-----------------------------------------------------
# Plot result for single scenario
sen_sub <- exp_dat[exp_dat$Scenario == "scenario_1", ]
plot_title <- paste0("Scenario 1 ...")

# Add mean and error bars (SD)
sen_sb <- sen_sub %>%
  group_by(HUC) %>%
  summarise(
    mean = mean(CE, na.rm = TRUE),
    sd = sd(CE, na.rm = TRUE)
  )

sen_sb$lower <- sen_sb$mean - sen_sb$sd
sen_sb$upper <- sen_sb$mean + sen_sb$sd

sen_sb$HUC <- as.character(sen_sb$HUC)

ggplot(sen_sb, aes(x = mean, y = HUC, xmin = lower, xmax = upper)) +
  geom_rect(aes(xmin = 0, xmax = 0.2, ymin = -Inf, ymax = Inf),
    fill = "pink", alpha = 0.02
  ) +
  geom_rect(aes(xmin = 0.2, xmax = 0.5, ymin = -Inf, ymax = Inf),
    fill = "orange", alpha = 0.02
  ) +
  geom_rect(aes(xmin = 0.5, xmax = 0.75, ymin = -Inf, ymax = Inf),
    fill = "yellow", alpha = 0.02
  ) +
  geom_rect(aes(xmin = 0.75, xmax = 1, ymin = -Inf, ymax = Inf),
    fill = "green", alpha = 0.02
  ) +
  geom_point() +
  geom_errorbarh(height = .2) +
  ggtitle(plot_title) +
  xlab("System Capacity (0 - 1)") +
  ylab("Recovery Scenario") +
  theme_bw()
} # }
```
