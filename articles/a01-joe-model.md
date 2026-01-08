# 1. Joe Model Overview

## Overview

This tutorial provides an introduction to the **Joe Model**, a core
component of the CEMPRA (Cumulative Effects Model for Prioritizing
Recovery Actions) framework. By the end of this tutorial, you will
understand how to:

- Import stressor-response and stressor-magnitude data
- Calculate system capacity for individual stressors
- Run the full Joe Model to assess cumulative effects across multiple
  stressors

### What is the Joe Model?

The Joe Model is a **cumulative effects assessment tool** that
quantifies how multiple environmental stressors combine to affect the
capacity of a system to support a target species or ecosystem function.

The model works by:

1.  **Defining stressor-response relationships**: How does each
    environmental stressor (e.g., temperature, sediment, flow
    alteration) affect system capacity?
2.  **Measuring stressor magnitudes**: What are the current levels of
    each stressor at locations of interest?
3.  **Combining effects**: How do multiple stressors interact to produce
    a cumulative effect on the system?

The output is a **system capacity score** (0-100%) representing the
relative ability of each location to support the target species, given
current stressor conditions.

### Prerequisites

Users following this tutorial should have:

- R and RStudio installed
- The `devtools` package installed
- Basic familiarity with R data structures (data frames, lists)

## Installation

The CEMPRA R package is hosted on GitHub and can be installed using
`devtools`.

``` r
library(devtools)
# Uncomment and run the following line to install the CEMPRA package
# install_github("essatech/CEMPRA")
library(CEMPRA)
```

## Understanding the Input Data

The Joe Model requires two Excel workbooks as input:

| Workbook                        | Purpose                                                                      |
|---------------------------------|------------------------------------------------------------------------------|
| **Stressor-Response Workbook**  | Defines how each stressor affects system capacity (the dose-response curves) |
| **Stressor-Magnitude Workbook** | Contains measured or estimated stressor levels at each location              |

These two workbooks work together: the stressor-response workbook tells
us *how bad* a given stressor level is, while the stressor-magnitude
workbook tells us *what the stressor levels actually are* at each
location.

## Stressor-Response Workbook

The stressor-response workbook defines the relationship between
environmental conditions and system capacity. Think of it as answering
the question: *“If temperature is 18°C, what percentage of optimal
habitat capacity remains?”*

### Workbook Structure

The stressor-response Excel workbook must follow a standardized format
with:

1.  A **Main** worksheet listing all stressors and their properties
2.  **Individual worksheets** for each stressor containing the
    dose-response curve data

### The Main Worksheet

The `Main` worksheet serves as a master index of all stressors in your
analysis. Here’s an example of what it looks like:

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

Each row represents one stressor, with the following columns:

| Column           | Description                                                                                         |
|------------------|-----------------------------------------------------------------------------------------------------|
| **Stressors**    | Unique name for the stressor (must match worksheet name exactly)                                    |
| **Stressor_cat** | Category grouping (used in population model for linking to vital rates)                             |
| **Interaction**  | How this stressor combines with others: `NA`/blank (multiplicative), `Additive`, or `Minimum`       |
| **Linked**       | For minimum interactions, which stressor this one is linked to                                      |
| **Stress_Scale** | Scale of the x-axis: `linear` or `logarithmic`                                                      |
| **Function**     | Curve type: `continuous` (smooth interpolation) or `step` (discrete thresholds)                     |
| **Life_stages**  | Which life stage this stressor affects (e.g., `fry_parr`, `adult`)                                  |
| **Parameters**   | What the stressor affects: blank/`NA` for system capacity, or `survival`/`capacity` for vital rates |

### Stressor-Response Curve Worksheets

After the Main worksheet, each stressor must have its own worksheet
containing the dose-response data. **The worksheet name must exactly
match the stressor name in the Main sheet.**

Each dose-response curve defines how system capacity changes across a
range of stressor values:

| Temperature (°C) | Mean System Capacity (%) |  SD | Lower Limit | Upper Limit |
|-----------------:|-------------------------:|----:|------------:|------------:|
|               12 |                       60 |  15 |          10 |          80 |
|               14 |                       80 |  10 |          30 |          95 |
|               16 |                      100 |  10 |          40 |         100 |
|               18 |                       95 |  10 |          40 |         100 |
|               20 |                       30 |   5 |           0 |          50 |
|               22 |                       15 |   3 |           0 |          30 |
|               24 |                        0 |   0 |           0 |           0 |
|               26 |                        0 |   0 |           0 |           0 |
|               28 |                        0 |   0 |           0 |           0 |

Example dose-response curve for temperature and adult system capacity

The columns are:

| Column                   | Description                                                                 |
|--------------------------|-----------------------------------------------------------------------------|
| **Stressor values**      | The first column contains raw stressor values (e.g., temperature in °C)     |
| **Mean System Capacity** | Expected system capacity (0-100%) at each stressor level                    |
| **SD**                   | Standard deviation representing uncertainty in the relationship             |
| **low.limit / up.limit** | Absolute bounds for the system capacity (used to constrain random sampling) |

#### Visualizing the Dose-Response Curve

The figure below illustrates how to interpret a stressor-response curve.
The optimal temperature for this species is around 16°C (100% capacity).
As temperature increases beyond the optimum, capacity declines rapidly,
reaching 0% at temperatures above 24°C.

![Example stressor-response curve showing how temperature affects system
capacity. Error bars show standard deviation; grey shading shows the
allowable range.](a01-joe-model_files/figure-html/dr_sample-1.png)

Example stressor-response curve showing how temperature affects system
capacity. Error bars show standard deviation; grey shading shows the
allowable range.

**Key points about the uncertainty:**

- The **error bars** (standard deviation) represent natural variability
  or uncertainty in the relationship
- The **grey shading** shows the absolute bounds - system capacity
  values will never fall outside this range
- During Monte Carlo simulations, values are sampled from this
  distribution to propagate uncertainty through the analysis

### Loading the Stressor-Response Workbook

Use the
[`StressorResponseWorkbook()`](https://essatech.github.io/CEMPRA/reference/StressorResponseWorkbook.md)
function to import and validate your data:

``` r
# Load the example stressor-response workbook included with CEMPRA
filename_sr <- system.file("extdata", "stressor_response_fixed_ARTR.xlsx", package = "CEMPRA")

# Import and validate the workbook
sr_wb_dat <- StressorResponseWorkbook(filename = filename_sr)

# The function returns a list with two components:
names(sr_wb_dat)
#> [1] "main_sheet"     "stressor_names" "pretty_names"   "sr_dat"
```

The returned object contains:

- `main_sheet`: The Main worksheet data as a data frame
- `sr_dat`: A list of data frames, one for each stressor’s dose-response
  curve

## Stressor-Magnitude Workbook

While the stressor-response workbook defines *how* stressors affect the
system, the stressor-magnitude workbook tells us *what the actual
stressor levels are* at each location.

This workbook contains location-specific measurements or estimates of
each stressor, along with uncertainty bounds.

### Workbook Structure

|     HUC_ID | NAME             | Stressor     | Stressor_cat | Mean |  SD | Distribution | Low_Limit | Up_Limit | Comments |
|-----------:|:-----------------|:-------------|:-------------|-----:|----:|:-------------|----------:|---------:|:---------|
| 1702010107 | Example Location | Aug_flow     | Aug_flow     |  100 |   0 | normal       |       100 |        0 | NA       |
| 1702010107 | Example Location | Barrier_dams | Barrier_dams |    0 |   0 | normal       |         0 |        5 | NA       |
| 1702010107 | Example Location | BKTR         | BKTR         |    0 |   0 | normal       |         0 |      100 | NA       |
| 1702010107 | Example Location | Feb_flow     | Feb_flow     |  100 |   0 | normal       |       100 |        0 | NA       |

Example stressor-magnitude data showing stressor levels at one location

Each row represents one stressor at one location:

| Column                   | Description                                                                                              |
|--------------------------|----------------------------------------------------------------------------------------------------------|
| **HUC_ID**               | Unique numeric identifier for the location (originally Hydrological Unit Code, but can be any ID system) |
| **NAME**                 | Human-readable location name (for display purposes)                                                      |
| **Stressor**             | Stressor name (must exactly match the stressor-response workbook)                                        |
| **Stressor_cat**         | Stressor category (must match the stressor-response workbook)                                            |
| **Mean**                 | Mean/expected value of the stressor at this location                                                     |
| **SD**                   | Standard deviation representing measurement uncertainty or natural variability                           |
| **Distribution**         | Sampling distribution: `normal` or `lognormal`                                                           |
| **Low_Limit / Up_Limit** | Bounds for the stressor values (prevents unrealistic samples)                                            |

### Loading the Stressor-Magnitude Workbook

Use the
[`StressorMagnitudeWorkbook()`](https://essatech.github.io/CEMPRA/reference/StressorMagnitudeWorkbook.md)
function to import your location data:

``` r
# Load the example stressor-magnitude workbook
filename_rm <- system.file("extdata", "stressor_magnitude_unc_ARTR.xlsx", package = "CEMPRA")

# Import the workbook - note that you specify which scenario worksheet to use
smw <- StressorMagnitudeWorkbook(filename = filename_rm, scenario_worksheet = "natural_unc")

# Preview the column names
names(smw)
#>  [1] "HUC_ID"       "NAME"         "Stressor"     "Stressor_cat" "Mean"        
#>  [6] "SD"           "Distribution" "Low_Limit"    "Up_Limit"     "Comments"
```

**Tip:** The stressor-magnitude workbook can contain multiple scenario
worksheets (e.g., “current”, “restored”, “future_climate”). This allows
you to compare system capacity under different management scenarios
without creating multiple files.

## Calculating System Capacity

Now that we understand the input data, let’s calculate system capacity.
We’ll start with a single stressor to understand the mechanics, then run
the full model with all stressors.

### Step 1: Generate Mean Response Curves

Before calculating system capacity, we need to process the
stressor-response data into interpolated curves. The
[`mean_Response()`](https://essatech.github.io/CEMPRA/reference/mean_Response.md)
function creates smooth curves from the discrete data points in your
worksheets:

``` r
# Create interpolated response curves for all stressors
mean.resp.list <- mean_Response(
  n.stressors = nrow(sr_wb_dat$main_sheet),
  str.list = sr_wb_dat$sr_dat,
  main = sr_wb_dat$main_sheet
)
```

This creates a list of response functions that can convert any stressor
value to a system capacity score.

### Step 2: Calculate Capacity for a Single Stressor

Let’s calculate system capacity for temperature at a hypothetical
location. This helps illustrate what the model is doing before we run
the full analysis.

``` r
# Extract data for the "Temperature_adult" stressor
f.main.df <- sr_wb_dat$main_sheet[which(sr_wb_dat$main_sheet$Stressors == "Temperature_adult"), ]
f.stressor.df <- sr_wb_dat$sr_dat[["Temperature_adult"]]
f.mean.resp.list <- mean.resp.list[[which(sr_wb_dat$main_sheet$Stressors == "Temperature_adult")]]

# Create sample data for a hypothetical location
# Mean temperature = 14°C, SD = 3°C
smw_sample <- data.frame(
  HUC_ID = 0,
  NAME = "Example Location",
  Stressor = "Temperature_adult",
  Stressor_cat = "Temperature",
  Mean = 14,
  SD = 3,
  Distribution = "normal",
  Low_Limit = 4,
  Up_Limit = 20
)

# Calculate system capacity with 100 Monte Carlo simulations
test_sc <- SystemCapacity(
  f.dose.df = smw_sample,
  f.main.df = f.main.df,
  f.stressor.df = f.stressor.df,
  f.mean.resp.list = f.mean.resp.list,
  n.sims = 100
)

# Convert to percentage
sys_cap <- round(test_sc$sys.cap * 100, 3)
```

### Understanding the Monte Carlo Results

The model runs multiple simulations (100 in this case) to capture
uncertainty. In each simulation:

1.  A temperature value is randomly drawn from the specified
    distribution (mean=14°C, SD=3°C)
2.  That temperature is converted to a system capacity using the
    dose-response curve
3.  Additional uncertainty from the dose-response relationship is
    incorporated

The result is a distribution of possible system capacity values:

``` r
hist(sys_cap,
     xlab = "System Capacity (%)",
     main = "System Capacity Distribution - Temperature Only",
     col = "steelblue",
     border = "white")
abline(v = mean(sys_cap), col = "red", lwd = 2, lty = 2)
legend("topright", legend = paste("Mean =", round(mean(sys_cap), 1), "%"),
       col = "red", lty = 2, lwd = 2, bty = "n")
```

![Distribution of system capacity values for temperature at the example
location. The spread reflects uncertainty in both the temperature
measurement and the dose-response
relationship.](a01-joe-model_files/figure-html/tmsc_plot-1.png)

Distribution of system capacity values for temperature at the example
location. The spread reflects uncertainty in both the temperature
measurement and the dose-response relationship.

At a mean temperature of 14°C, the system capacity is relatively high
(around 80%), which matches our dose-response curve where 14°C
corresponds to good habitat conditions.

## Running the Full Joe Model

Now let’s run the complete Joe Model, which:

1.  Calculates system capacity for **every stressor** at **every
    location**
2.  Combines individual stressor effects into a **cumulative system
    capacity** score
3.  Repeats this process across multiple Monte Carlo simulations to
    quantify uncertainty

### Loading the Data

``` r
# Load both workbooks
filename_rm <- system.file("extdata", "stressor_magnitude_unc_ARTR.xlsx", package = "CEMPRA")
filename_sr <- system.file("extdata", "stressor_response_fixed_ARTR.xlsx", package = "CEMPRA")

dose <- StressorMagnitudeWorkbook(filename = filename_rm, scenario_worksheet = "natural_unc")
sr_wb_dat <- StressorResponseWorkbook(filename = filename_sr)

# Check how many locations and stressors we have
cat("Number of locations:", length(unique(dose$HUC_ID)), "\n")
#> Number of locations: 103
cat("Number of stressors:", nrow(sr_wb_dat$main_sheet), "\n")
#> Number of stressors: 18
```

### Running the Model

The
[`JoeModel_Run()`](https://essatech.github.io/CEMPRA/reference/JoeModel_Run.md)
function handles all the calculations:

``` r
# Run the Joe Model
jmr <- JoeModel_Run(
  dose = dose,           # Stressor magnitude data
  sr_wb_dat = sr_wb_dat, # Stressor-response relationships
  MC_sims = 10           # Number of Monte Carlo simulations (use more for final analysis)
)

# Examine the output structure
names(jmr)
#> [1] "ce.df"      "sc.dose.df"
```

### Understanding the Output

The Joe Model returns a list with two key components:

| Component    | Description                                                                         |
|--------------|-------------------------------------------------------------------------------------|
| `ce.df`      | **Cumulative Effects** - Overall system capacity for each location and simulation   |
| `sc.dose.df` | **Stressor-specific Capacity** - System capacity broken down by individual stressor |

Let’s visualize the cumulative system capacity across all locations and
simulations:

``` r
# Extract cumulative system capacity (as percentage)
msc <- jmr$ce.df$CE * 100

hist(msc,
     xlab = "Cumulative System Capacity (%)",
     main = "Joe Model Results - All Locations",
     col = "steelblue",
     border = "white",
     breaks = 20)
abline(v = mean(msc), col = "red", lwd = 2, lty = 2)
legend("topleft",
       legend = c(paste("Mean =", round(mean(msc), 1), "%"),
                  paste("Min =", round(min(msc), 1), "%"),
                  paste("Max =", round(max(msc), 1), "%")),
       bty = "n")
```

![Distribution of cumulative system capacity across all locations and
Monte Carlo simulations. Lower values indicate locations where multiple
stressors are limiting habitat
quality.](a01-joe-model_files/figure-html/msc_plot-1.png)

Distribution of cumulative system capacity across all locations and
Monte Carlo simulations. Lower values indicate locations where multiple
stressors are limiting habitat quality.

### Interpreting the Results

The histogram shows the distribution of cumulative system capacity
values. Key insights:

- **High values (\>70%)**: Locations with good habitat conditions where
  few stressors are limiting
- **Medium values (40-70%)**: Locations where some stressors are
  degrading habitat quality
- **Low values (\<40%)**: Heavily impacted locations where multiple
  stressors are limiting capacity

The spread in values reflects both:

1.  **Spatial variation**: Different locations have different stressor
    levels
2.  **Uncertainty**: Monte Carlo sampling propagates uncertainty from
    both stressor measurements and dose-response relationships

## Next Steps

This tutorial covered the basics of running the Joe Model for a single
scenario. To learn more about:

- **Running multiple scenarios**: See [Tutorial 2: Joe Model Batch
  Run](https://essatech.github.io/CEMPRA/articles/a02-joe-model-batch-run.md)
- **Linking to population dynamics**: See [Tutorial 3: Population Model
  Overview](https://essatech.github.io/CEMPRA/articles/a03-population-model.md)
- **Sensitivity analysis**: See [Tutorial 6: Population Model
  Evaluation](https://essatech.github.io/CEMPRA/articles/a06-population-model-sensitivity.md)

For detailed guidance on preparing your own data, see the [CEMPRA
Documentation](https://mattjbayly.github.io/CEMPRA_documentation/).
