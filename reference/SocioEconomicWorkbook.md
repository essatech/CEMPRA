# Get Socio Economic Data from Excel Workbook

Extract the socio economic data from each sheet in the Excel workbook.

## Usage

``` r
SocioEconomicWorkbook(filename = NA)
```

## Arguments

- filename:

  A string. Relative file name to the stressor response Excel workbook.

## Value

A list object of each worksheet organized as separate data frames.

## Details

For more information about data format requirements, see manual.

## Examples

``` r
if (FALSE) { # \dontrun{
library(CEMPRA)
filename <- system.file("extdata", "socio_economic/socio_economic_input.xlsx", package = "CEMPRA")
socioeconomic_inputs <- SocioEconomicWorkbook(filename = filename)
} # }
```
