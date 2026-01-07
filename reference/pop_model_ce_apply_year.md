# Population Model CE Application to Vital Rates Year-Specific

Utility function to apply cumulative effects to vital rates.

## Usage

``` r
pop_model_ce_apply_year(
  rates = NULL,
  CE_df = NULL,
  anadromous = FALSE,
  mat_stage = NULL
)
```

## Arguments

- rates:

  Life history vital rates list object.

- CE_df:

  Cumulative effects data frame.

- anadromous:

  Anadromous TRUE/FALSE.

- mat_stage:

  Named numeric vector of mature life stages.

## Value

a modified dat vital rates object after applying CE effects.
