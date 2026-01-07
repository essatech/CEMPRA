# Population Model LTRE

Run a LTRE for the population model

## Usage

``` r
pop_model_ltre(
  step_size = 0.05,
  dose = NA,
  sr_wb_dat = NA,
  life_cycle_params = NA,
  HUC_ID = NA,
  n_reps = 100,
  stressors = NA,
  habitat_dd_k = NULL
)
```

## Arguments

- step_size:

  numeric. step size for ltre experiment parameter adjustment.

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

- n_reps:

  numeric. Number of replicates. Carlo simulations for the Population
  Model.

- stressors:

  (optional) character vector of stressor names to include in the
  Population Model. Leave the default value as NA if you wish to include
  all stressors applicable to the population model.

- habitat_dd_k:

  (optional) dataframe of location and stage-specific habitat capacity k
  values for the target species. If used this dataframe will override
  the capacity estimates

## Details

In prep .... step_size = 0.05

## Examples

``` r
if (FALSE) { # \dontrun{
library(CEMPRA)
# in prep
} # }
```
