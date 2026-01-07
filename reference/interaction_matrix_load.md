# Load Interaction Matrices

Load interaction Excel Matrices

## Usage

``` r
interaction_matrix_load(mint_names = NA, filename = NA)
```

## Arguments

- mint_names:

  Character vector of interaction matrix names.

- filename:

  Character string of Excel file.

## Value

a list object of interaction matrix objects.

## Details

Load 2-factor interaction matrices from Excel workbook (if they exist).
This function is run from within StressorResponseWorkbook().
