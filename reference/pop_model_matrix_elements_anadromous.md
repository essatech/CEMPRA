# Population Model Matrix Elements for Anadromous Fish

Build population model matrix elements for anadromous fish.

## Usage

``` r
pop_model_matrix_elements_anadromous(pop_mod_setup = NA)
```

## Arguments

- pop_mod_setup:

  List. Object returned from population_model_setup_anadromous(). Use
  pop_model_matrix_elements() for objects returned from
  population_model_setup().

## Value

a list object of symbolic objects.

## Details

This is an intermediate setup function to run the population model.
However, some of outputs are useful on their own, especially for eigen
analyses. pop_model_matrix_elements() is run after pop_model_setup() or
pop_model_setup_anadromous(), but pop_model_matrix_elements() must be
run before Projection_DD(). Key outputs to explore include a density
independent projection matrix, life histories, and a symbolic
representation of the matrix math for density dependent and density
independent components. population_model_setup() should be run with
pop_model_matrix_elements() for non-anadromous species.
population_model_setup_anadromous() should be run with
pop_model_matrix_elements_anadromous() for anadromous species.

## Examples

``` r
if (FALSE) { # \dontrun{

# Load the CEMPRA package.
library(CEMPRA)

filename_lc <- system.file("extdata", "life_cycles.csv", package = "CEMPRA")
life_cycles <- read.csv(filename_lc)

# Setup objects for population model
pop_mod_setup <- pop_model_setup(life_cycles = life_cycles)

# Build matrix elements for population model
pop_mod_mat <- pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)
names(pop_mod_mat)

# A density independent projection matrix
pop_mod_mat$projection_matrix



} # }
```
