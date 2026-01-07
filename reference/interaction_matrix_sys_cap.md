# Interaction Matrix System Capacity

Use run a 2D linear interpolation with interaction matrix to calculate
system capacity.

## Usage

``` r
interaction_matrix_sys_cap(MInt, sc.dose.df, adult_sys_cap)
```

## Arguments

- MInt:

  One item from object returned from StressorResponseWorkbook()

- sc.dose.df:

  Data frame with columns HUC, Stressor, simulation, dose and sys.cap.

- adult_sys_cap:

  Should the Joe Model be run only with variables identified for adult
  system capacity.

## Value

Updated sc.dose.df data frame with system capacity values

## Details

Runs through an individual MInt\_ worksheet object supplied from
sr_wb_dat\$MInt and calculates system capacity.
interaction_matrix_sys_cap() should be run with apply on
sr_wb_dat\$MInt. Returns sc.dose.df with updated system capacity and
filtered for variables.
