# Get Stressor Response Data from Excel Workbook

Extract the stressor response data from each sheet in the Excel
workbook.

## Usage

``` r
StressorResponseWorkbook(filename = NA)
```

## Arguments

- filename:

  A string. Relative file name to the stressor response Excel workbook.

## Value

A list object of the `main_sheet`, `stressor_names` and stressor
response data frames (`sr_dat`).

## Details

For more information about data format requirements, see the Data Upload
and Results Export tab of the associated Shiny App.

Uses
[`readxl::read_excel()`](https://readxl.tidyverse.org/reference/read_excel.html)
to load in workbook sheets:

- Main sheet: meta data / cover sheet indicating:

  - stressor names

  - categories

  - interactions and linkages

  - scales

  - approximation function type

  - target life stages

  - contribution to survival or capacity parameters

  - stressor units

  - target model (CE or population models, or both)

- all additional stressor-response sheets in workbook

  - context specific stressor-response curve data

  - sheet name must match stressor names in Main sheet, otherwise "Bad
    worksheet names" error message.

  - sheets must contain columns for `<stressor_name>`,
    `Mean System Capacity (%)`, `SD`, `low.limit`, and `up.limit`
