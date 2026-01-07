# Joe Model Run

Runs the Joe Model.

## Usage

``` r
JoeModel_Run(
  dose = NA,
  sr_wb_dat = NA,
  MC_sims = 100,
  stressors = NA,
  adult_sys_cap = FALSE,
  socioeconomic_inputs = NULL
)
```

## Arguments

- dose:

  dataframe. Stressor magnitude file exported from
  StressorMagnitudeWorkbook().

- sr_wb_dat:

  list object. Stressor response workbook returned from
  StressorResponseWorkbook().

- MC_sims:

  numeric. set number of Monte Carlo simulations for the Joe Model.

- stressors:

  (optional) character vector of stressor names to include in the Joe
  Model. Leave the default value as NA if you wish to include all
  stressors applicable to the adult life stage or provide a character
  vector of stressors if you only want to run the model on a subset of
  the stressors.

- adult_sys_cap:

  Should the Joe Model be run only with variables identified for `adult`
  system capacity.

- socioeconomic_inputs:

  (optional) list object. Socioeconomic inputs returned from
  SocioEconomicWorkbook().

## Value

A list object with the following components:

- ce.df:

  dataframe. Cumulative effects data frame with columns for HUC,
  simulation, and CE (cumulative effect).

- sc.dose.df:

  dataframe. Stressor-specific system capacity and dose data frame with
  columns for HUC, Stressor, simulation, dose, sys.cap, int.type, and
  link.

- socioeconomic_inputs:

  (optional) list object. Socioeconomic inputs returned from
  SocioEconomicWorkbook() if socioeconomic_inputs were provided to the
  function.

set number of Monte Carlo simulations for the Joe model.

## Details

Runs the Joe Model for cumulative system capacity across stressors and
watersheds. Note that only stressors with the applicable to the 'adult'
Life_stages from the 'main_sheet' of the Stressor Response workbook are
included in the Joe Model. socioeconomic_inputs can be optionally
supplied to run the Joe Model with socioeconomic inputs reductions.

## Examples

``` r
if (FALSE) { # \dontrun{
library(CEMPRA)

# Load in the sample data from the reference Excel workbook
filename_rm <- system.file("extdata", "stressor_magnitude_unc_ARTR.xlsx", package = "CEMPRA")
filename_sr <- system.file("extdata", "stressor_response_fixed_ARTR.xlsx", package = "CEMPRA")
# Stressor Magnitue and Doese Response Workbooks
dose <- StressorMagnitudeWorkbook(filename = filename_rm)
sr_wb_dat <- StressorResponseWorkbook(filename = filename_sr)

# Run the Joe Model
jmr <- JoeModel_Run(dose = dose,
             sr_wb_dat = sr_wb_dat,
             MC_sims = 100)

# The Joe model holds cumulative effects data frame
# and sc.dose.df for individual stressors
names(jmr)

# Evaluate the cumulative system capacity
summary(jmr$ce.df$CE)
} # }
```
