# 1. Joe Model Overview

## Overview

The following tutorial provides an overview of running the Joe Model
from the CEMPRA R package. Users following this tutorial should have R,
RStudio and the R package `devtools` installed on their machine.

## Installation

At this time the CEMPRA R package is hosted on GitHub and can be
installed using devtools. In the near future the CEMPRA package will be
available on R-CRAN for standard installation and loading.

``` r
library(devtools)
# Uncomment and run the following line to install the CEMPRA package
# install_github("essatech/CEMPRA")
library(CEMPRA)
```

## Import Data

The Joe Model requires two key datasets, a stressor-response workbook
and a stressor magnitude workbook. We will import these datasets using
the `StressorResponseWorkbook` and `StressorMagnitudeWorkbook`
functions. Change `filename_sr` and `filename_rm` to the location of
your stressor-response and stressor-magnitude workbooks on your own
computer. We are loading canned data from the CEMPRA package for this
tutorial.

### Stressor Response Workbook

The stressor-response Excel workbook must follow a standardized format.
The first worksheet should be titled `Main`.

#### Main Worksheet

    #>      Stressors Stressor_cat Interaction Linked Stress_Scale   Function
    #> 1     Aug_flow     Aug_flow     Minimum      A       linear continuous
    #> 2 Barrier_dams Barrier_dams          NA     NA       linear       step
    #> 3         BKTR         BKTR          NA     NA       linear continuous
    #> 4     Feb_flow     Feb_flow     Minimum      A       linear continuous
    #> 5    Foot_flow    Foot_flow          NA     NA       linear continuous
    #>   Life_stages Parameters Units     Model
    #> 1       adult       <NA>    NA Joe Model
    #> 2       adult       <NA>    NA Joe Model
    #> 3       adult       <NA>    NA Joe Model
    #> 4       adult       <NA>    NA Joe Model
    #> 5       adult       <NA>    NA Joe Model

Take a look at the `Main` worksheet.

1.  **Stressors** Name of the stressor…
2.  **Stressor_cat** Name of the stressor category (only relevant to
    population model)…
3.  **Interaction** Can be either blank or NA with the default of
    multiplicative, ‘Additive’ or ‘Minimum’…
4.  **Linked** For variables with minimum interaction…
5.  **Stress_Scale** Can be either `'linear'` or `'logarithmic'`…
6.  **Function** Can be either `'continuious'` or `'step'`…
7.  **Life_stages** Can be either `'fry_parr'`, `'alevin'`,
    `'sub_adult'` or `'adult'`,…
8.  **Parameters** Can be blank or NA (default), which implys the
    relationship is for system capacity or linked to a vital rate
    `'survival'`, `'capacity'`…

#### Stressor-Response Worksheets

After the stressor response Excel workbook then must have individual
worksheets for each of the stressors after the `Main` sheet. Please
ensure that the spelling is identical between the `Stressors` column in
the `Main` sheet and the work sheet name for each stressor.

Each stressor-response curve is provided on a separate worksheet. The
example below shows the stressor-response curve of water temperature (C)
and the Adult system capacity.

| Temperature | Mean System Capacity (%) |  SD | low.limit | up.limit |
|------------:|-------------------------:|----:|----------:|---------:|
|          12 |                       60 |  15 |        10 |       80 |
|          14 |                       80 |  10 |        30 |       95 |
|          16 |                      100 |  10 |        40 |      100 |
|          18 |                       95 |  10 |        40 |      100 |
|          20 |                       30 |   5 |         0 |       50 |
|          22 |                       15 |   3 |         0 |       30 |
|          24 |                        0 |   0 |         0 |        0 |
|          26 |                        0 |   0 |         0 |        0 |
|          28 |                        0 |   0 |         0 |        0 |

Table 2: Example ‘Temperature’ dose-response curve data for temperature
(x-axis) and system capacity (y-axis)

1.  **Temperature** The first column should be the name of the stressor…
2.  **Mean System Capacity (%)** The next column is the mean system
    capacity (from 0 - 100)..
3.  **SD** The third column is the standard deviation for the mean
    system capacity. This is useful to represent uncertainty in the
    relationship. For example, a temperature of 12C will not always
    equate to a system capacity of exactly 90%…
4.  **low.limit** Lower limits for the cumulative system capacity…
5.  **up.limit** Upper limits for the cumulative system capacity…

![Example Stressor-Response
Curve](a01-joe-model_files/figure-html/dr_sample-1.png)

Example Stressor-Response Curve

The figure above shows a stressor response curve. The raw values of the
stressor are plotted on the x-axis and the mean system capacity is shown
on the y-axis. For each stressor and each simulation values for the mean
system capacity will be drawn from a normal distribution with a mean and
standard deviation (error bars) bound by the lower and upper limits
(grey shading).

``` r
  # Load default file from Joe Model Package
  filename_sr <- system.file("extdata", "stressor_response_fixed_ARTR.xlsx", package = "CEMPRA")
  # Use the StressorResponseWorkbook() function from the CEMPRA package to format the data and check for errors and issues
  sr_wb_dat <- StressorResponseWorkbook(filename = filename_sr)
  names(sr_wb_dat)
#> [1] "main_sheet"     "stressor_names" "pretty_names"   "sr_dat"
```

### Stressor Magnitude Workbook

Review the stressor magnitude levels for a given location.

|     HUC_ID | NAME  | Stressor     | Stressor_cat | Mean |  SD | Distribution | Low_Limit | Up_Limit | Comments |
|-----------:|:------|:-------------|:-------------|-----:|----:|:-------------|----------:|---------:|:---------|
| 1702010107 | Name… | Aug_flow     | Aug_flow     |  100 |   0 | normal       |       100 |        0 | NA       |
| 1702010107 | Name… | Barrier_dams | Barrier_dams |    0 |   0 | normal       |         0 |        5 | NA       |
| 1702010107 | Name… | BKTR         | BKTR         |    0 |   0 | normal       |         0 |      100 | NA       |
| 1702010107 | Name… | Feb_flow     | Feb_flow     |  100 |   0 | normal       |       100 |        0 | NA       |

Table 3: Location data from stressor-magnitude.xlsx workbook

Column names should be as follows:

1.  **HUC_ID** Unique (numeric) identifier for a location of interest.
    Formally from Hydrologial Unit Code (HUC) representing unique sub
    basins, but can be anything…
2.  **NAME** Name (character) of a given location. Note that this column
    is used for display purposes only…
3.  **Stressor** Name of the stressor. Note that this must match the
    spelling used in the stressor response workbook…
4.  **Stressor_cat** Name of the stressor category. Note that this mutch
    match the spelling used in the stressor response workbook…
5.  **Mean** Mean value for the specific location…
6.  **SD** Standard deviation for the specific stressor for that
    location. The value of each stressor and combination will be
    resampled for each simulation…
7.  **Distribution** Re-sampling distribution either `'lognormal'` or
    `'normal'`…
8.  **Low_Limit** Lower limits for the location-specific stressor…
9.  **Up_Limit** Upper limits for the location-specific stressor….

``` r
  # Load default file from Joe Model Package
  filename_rm <- system.file("extdata", "stressor_magnitude_unc_ARTR.xlsx", package = "CEMPRA")
  # Use the StressorMagnitudeWorkbook() function from the CEMPRA package to format the data and check for errors and issues
  smw <- StressorMagnitudeWorkbook(filename = filename_rm, scenario_worksheet = "natural_unc")
  names(smw)
#>  [1] "HUC_ID"       "NAME"         "Stressor"     "Stressor_cat" "Mean"        
#>  [6] "SD"           "Distribution" "Low_Limit"    "Up_Limit"     "Comments"
```

## Cumulative System Capacity

Run the cumulative system capacity for a single watershed

``` r

# Stressor Response Workbook object = sr_wb_dat

# Run the mean_Response() function to create response curves.
# Note that this intermediate object is need throughout
mean.resp.list <- mean_Response(
  n.stressors = nrow(sr_wb_dat$main_sheet),
  str.list = sr_wb_dat$sr_dat,
  main = sr_wb_dat$main_sheet
)

# Set up objects for system capacity function - for "Temperature_adult"
f.main.df <- sr_wb_dat$main_sheet[which(sr_wb_dat$main_sheet$Stressors == "Temperature_adult"), ]
f.stressor.df <- sr_wb_dat$sr_dat[["Temperature_adult"]]
f.mean.resp.list <- mean.resp.list[[which(sr_wb_dat$main_sheet$Stressors == "Temperature_adult")]]

# Stressor Magnitude: Make up fake data for a sample watershed (e.g., HUC_ID: 1701010208)
smw_sample <- data.frame(HUC_ID = 0, NAME = "Location Name",
                      Stressor = "Temperature_adult", Stressor_cat = "Temperature",
                      Mean = 14, SD = 3, Distribution = "normal",
                      Low_Limit = 4, Up_Limit = 20)

# ?SystemCapacity # Preview help function

test_sc <- SystemCapacity(f.dose.df = smw_sample,
                          f.main.df = f.main.df,
                          f.stressor.df = f.stressor.df,
                          f.mean.resp.list = f.mean.resp.list,
                          n.sims = 100)
names(test_sc)
#> [1] "sys.cap"  "dose"     "dose.mat"

# Look at system capacity for one variable and one stressor
sys_cap <- round(test_sc$sys.cap * 100, 3)
```

``` r
hist(sys_cap, xlab = "Mean System Capacity (%)")
```

![Mean System Capacity for temperature for a hypothetical
watershed](a01-joe-model_files/figure-html/tmsc_plot-1.png)

Mean System Capacity for temperature for a hypothetical watershed

## Joe Model

What about all variables and all stressors - run the full Joe Model…

``` r
# Import of stressor response and magnitude workbook
filename_rm <- system.file("extdata", "stressor_magnitude_unc_ARTR.xlsx", package = "CEMPRA")
filename_sr <- system.file("extdata", "stressor_response_fixed_ARTR.xlsx", package = "CEMPRA")

dose <- StressorMagnitudeWorkbook(filename = filename_rm, scenario_worksheet = "natural_unc")
sr_wb_dat <- StressorResponseWorkbook(filename = filename_sr)

# ----------------------------------------------------------
# Run the Basic Joe Model
jmr <- JoeModel_Run(dose = dose, # Stressor Magnitude Object
                    sr_wb_dat = sr_wb_dat, # Stressor Response Object
                    MC_sims = 10 # Number of Monte Carlo simulations
                    )
names(jmr)
#> [1] "ce.df"      "sc.dose.df"
# ce.df = Mean system capacity for each HUC_ID for each simulation
# sc.dose.df = Mean system capacity for each variable for each HUC_ID for each simulation
msc <- jmr$ce.df$CE * 100
```

``` r
hist(msc, xlab = "Mean System Capacity (%)", main = NULL)
```

![Mean System Capacity for all stressors and all watersheds across all
simulations](a01-joe-model_files/figure-html/msc_plot-1.png)

Mean System Capacity for all stressors and all watersheds across all
simulations
