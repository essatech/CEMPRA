# Population Model Setup

Generate symbolic objects for the population model and then generate
parameter matrices.

## Usage

``` r
pop_model_setup(life_cycles = NA)
```

## Arguments

- life_cycles:

  Data frame. Raw life cycles.csv data frame.

## Value

a list object of symbolic objects.

## Details

This is an intermediate setup function to run the population, but some
of its outputs are useful on their own, especially for eigen analyses
from the projection matrix. pop_model_setup() is run before
pop_model_matrix_elements() and Projection_DD().

Transition Matrix:

- Calculates stage-specific survivals and transition rates, survival
  probabilities, transition probabilities.

- Initializes the survival/transition rates subject to a stage-specific
  density-dependence.

- Checks for excessive compensation ratios.

Dataset for life-histories:

- transition probabilities

- egg-survival, age-0 survival, and then stage-specific survival
  probabilities

- spawning events per year

- eggs-per-spawner

- number of spawning intervals

- adult carrying capacity

- sex ratio
