# Population Model Run

Runs the population model with stressors and stressor-response
relationships

## Usage

``` r
PopulationModel_Run(
  dose = NA,
  sr_wb_dat = NA,
  life_cycle_params = NA,
  HUC_ID = NA,
  n_years = 100,
  MC_sims = 10,
  stressors = NA,
  output_type = "full",
  habitat_dd_k = NULL
)
```

## Arguments

- dose:

  dataframe. Stressor magnitude dataset imported from
  StressorMagnitudeWorkbook().

- sr_wb_dat:

  list object. Stressor response workbook imported from
  StressorResponseWorkbook().

- life_cycle_params:

  dataframe. Life cycle parameters.

- HUC_ID:

  character. HUC_ID for the location unit. Can only be one HUC_ID (not
  an array).

- n_years:

  numeric. Number of years to run the population.

- MC_sims:

  numeric. set number of Monte Carlo simulations for the Population
  Model.

- stressors:

  (optional) character vector of stressor names to include in the
  Population Model. Leave the default value as NA if you wish to include
  all stressors applicable to the population model.

- output_type:

  (optional) character. Set to "full" for all data of "adults" for only
  adult data.

- habitat_dd_k:

  (optional) dataframe of location and stage-specific habitat capacity k
  values for the target species. If used this dataframe will override
  the capacity estimates

## Value

List object of population model outputs. These include the population
vectors for each Monte Carlo simulation and year. List objects names
include "ce" for the runs with stressors and "baseline" for the runs
without stressors.

## Details

Runs the population model within the Joe Model stress-response
framework. The population model is run independently for each single
sub-watershed unit (HUC_ID). Inputs for `PopulationModel_Run` include
the stressor-response and stressor magnitude workbooks as well as the
life cycle parameters object.

## Examples

``` r
if (FALSE) { # \dontrun{
library(CEMPRA)
} # }
```
