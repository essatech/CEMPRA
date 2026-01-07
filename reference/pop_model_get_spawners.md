# Population Model Get Spawners

Get spawners from
[`PopulationModel_Run()`](https://essatech.github.io/CEMPRA/reference/PopulationModel_Run.md)
output.

## Usage

``` r
pop_model_get_spawners(
  data = NULL,
  life_stage = "spawners",
  life_cycle_params = NULL
)
```

## Arguments

- data:

  list output from
  [`PopulationModel_Run()`](https://essatech.github.io/CEMPRA/reference/PopulationModel_Run.md).

- life_stage:

  Target lifestage. Character string. Can be either `spawners` or.

- life_cycle_params:

  dataframe. Life cycle parameters.

## Value

dataframe of spawner abundance by location, replicate, and year.

## Details

Transform verbose output from PopulationModel_Run() into clean dataframe
of abundance data for target life stage. This should be run after
[`PopulationModel_Run()`](https://essatech.github.io/CEMPRA/reference/PopulationModel_Run.md)
or `Projection_DD`.

## Examples

``` r
if (FALSE) { # \dontrun{
library(CEMPRA)
} # }
```
