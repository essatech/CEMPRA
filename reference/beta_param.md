# Estimate Beta Parameter

Estimates beta parameter given mean (rates) and SD for the the
"untruncated" beta distribution.

## Usage

``` r
beta_param(mean, sd)
```

## Arguments

- mean:

  Numeric. The mean.

- sd:

  Numeric. The standard deviation.

## Value

A list with alpha and beta parameters along with messages about mean and
cv

## Details

Updated using analytical solution from Kyle Wilson.

## Examples

``` r
if (FALSE) { # \dontrun{
library(CEMPRA)
CEMPRA::beta_param(0.8, 0.2)$beta
} # }
```
