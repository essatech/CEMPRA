# build_k_for_proj_dd

Build build_habitat_dd_k object and bh_dd_stages from
PopulationModel_Run for Projection_DD.

## Usage

``` r
build_k_for_proj_dd(habitat_dd_k, HUC_ID, life_histories, life_cycle_params)
```

## Arguments

- life_histories:

  Object returned from pop_model_matrix_elements()\$life_histories

- life_cycle_params:

  Object returned from pop_model_dat_clean() without any further editing

## Value

A list with the following components:

- ret_stage_k_override: A named numeric vector of carrying capacity (K)
  values for each life stage, including fry (K0) and spawners if
  applicable.

- ret_bh_dd_stages: A character vector of life stage names that are
  subject to Beverton-Holt density dependence.

- ret_total_anadromous_spawners: A numeric value representing the total
  carrying capacity for anadromous spawners, if applicable.

## Details

This function builds the habitat carrying capacity (K) vector for use in
density dependent projections. It extracts K values from the
habitat_dd_k input data frame for the target HUC_ID and formats them for
use in Projection_DD(). It also identifies which life stages are subject
to density dependence based on the life cycle parameters. The function
will return an empty list of there is no location-based DD.
