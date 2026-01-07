# System Capacity

Calculates the system capacity for each stressor.

## Usage

``` r
SystemCapacity(
  f.dose.df,
  f.main.df,
  f.stressor.df,
  f.mean.resp.list,
  n.sims = 100,
  socioeconomic_inputs = NULL
)
```

## Arguments

- f.dose.df:

  Stressor magnitude data frame returned from
  [`StressorMagnitudeWorkbook()`](https://essatech.github.io/CEMPRA/reference/StressorMagnitudeWorkbook.md)
  filtered for target stressor.

- f.main.df:

  `main_sheet` list object returned from
  `StressorResponseWorkbook()$main_sheet` filtered for target stressor.

- f.stressor.df:

  Dose response data frame returned from
  `StressorResponseWorkbook()$sr_dat` for target stressor.

- f.mean.resp.list:

  Response function list for target variable returned from
  [`mean_Response()`](https://essatech.github.io/CEMPRA/reference/mean_Response.md).

- n.sims:

  Number of simulations to generate. Defaults t0 100.

- socioeconomic_inputs:

  (Optional) list object of SE inputs.

## Details

`SystemCapacity()` calculates the system capacity for each stressor.
This function takes data frame arguments for stressor magnitudes and
dose responses plus a list of stressor-response approximation functions
(`f.` for local list or data frame). Note: Some stressors have multiple
doses (Additive interaction).
