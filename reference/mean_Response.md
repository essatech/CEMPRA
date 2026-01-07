# Mean Response Function

Generate the mean response function for each stressor.

## Usage

``` r
mean_Response(n.stressors = NA, str.list = NA, main = NA)
```

## Arguments

- n.stressors:

  Numeric. The number of stressors.

- str.list:

  List object. A list object of dataframes of stressor response
  relationships.

- main:

  Dataframe. A dataframe of the Main cover sheet for the stressor
  response relationships.

## Details

Creates functions to interpolate the mean system capacity, SD, lower
limit and upper limit.

## Examples

``` r
if (FALSE) { # \dontrun{

library(CEMPRA)

# Import of stressor response and magnitude workbook
filename_rm <- system.file("extdata", "stressor_magnitude_unc_ARTR.xlsx", package = "CEMPRA")
filename_sr <- system.file("extdata", "stressor_response_fixed_ARTR.xlsx", package = "CEMPRA")
dose <- StressorMagnitudeWorkbook(filename = filename_rm, scenario_worksheet = "natural_unc")
sr_wb_dat <- StressorResponseWorkbook(filename = filename_sr)


mean.resp.list <- mean_Response(
  n.stressors = nrow(sr_wb_dat$main_sheet),
  str.list = sr_wb_dat$sr_dat,
  main = sr_wb_dat$main_sheet
)

# Get the mean response function for the first stressor
my_stressor <- sr_wb_dat$main_sheet$Stressors[[1]]
my_stressor

# Look at the stressor-response curve
sr_data <- sr_wb_dat$sr_dat[names(sr_wb_dat$sr_dat) == my_stressor][[1]]

plot(sr_data$value,
     sr_data$mean_system_capacity,
     type = 'b',
     xlab = "Raw Stressor Value",
     ylab = "Mean System Capacity (%)")

# The lookup the value from mean_Response()
custom_function <- mean.resp.list[[1]][[1]]

# Input several raw stressor values and the function will
# return the mean system capacity
custom_function(20)
custom_function(60)
custom_function(80)

} # }
```
