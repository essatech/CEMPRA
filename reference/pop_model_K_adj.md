# K_adj

Define K_adj value.

## Usage

``` r
pop_model_K_adj(replicates = 100, dat, mx, dx, Nyears = 250, H.mx = NULL)
```

## Arguments

- replicates:

  Number of replicates

- dat:

  Life history data

- mx:

  A projection matrix expression

- dx:

  A matrix of density-dependence effect

- Nyears:

  Years to run simulation

- H.mx:

  A harm projection matrix

## Value

A K_adj object to make adjustments

## Details

Initialize populations and find carrying capacity for each life stage.
Carrying capacity is defined at the adult stage and estimated for all
other life-stages using the stable-stage distribution (i.e., if the
carrying capacity of the adult stage was set to 100 the carrying
capacity for a stage below (stage x) is the amount of stage x necessary
to result in 100 adults at equilibrium conditions).
