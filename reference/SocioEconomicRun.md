# Run SocioEconomic Cost Calculations

Provides draft cost calculations for socioeconomic inputs.

## Usage

``` r
SocioEconomicRun(socioeconomic_inputs = NA, deterministic = FALSE)
```

## Arguments

- socioeconomic_inputs:

  list object of socioeconomic inputs returned from
  SocioEconomicWorkbook().

- deterministic:

  Boolean. Defaults to FALSE. If TRUE model will run without
  stochasticity.

## Value

A list object with cost and area summary calculations included

## Details

Internal function run within the Joe Model to perform socioeconomic
calculations.

## Examples

``` r
if (FALSE) { # \dontrun{
library(CEMPRA)
filename <- system.file("extdata", "socio_economic/socio_economic_input.xlsx", package = "CEMPRA")
socioeconomic_inputs <- SocioEconomicWorkbook(filename = filename)
socioeconomic_inputs <- SocioEconomicRun(socioeconomic_inputs = socioeconomic_inputs)

} # }
```
