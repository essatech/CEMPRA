# tbeta Random

Returns N random estimates for truncated beta distribution given vectors
for mean and SD for the untruncated distribution of system capacity.

## Usage

``` r
tbeta_rnd(mn_est, sd_est, low.limit = 0, up.limit = 1)
```

## Arguments

- mn_est:

  Vector of mean values.

- sd_est:

  Vector of standard deviations.

- low.limit:

  Numeric. Defaults to 0.

- up.limit:

  Numeric. Defaults to 1 .

## Details

Function returns returns one random value for each mean and sd value.
Will not run with 0 or 1 values.
