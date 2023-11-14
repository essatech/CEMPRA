# CEMPRA <img src="man/figures/CEMPRA_small.png" align="right" style="max-width: 120px;"/>

<!-- badges: start -->
<!-- badges: end -->

The Cumulative Effects Model for Prioritizing Recovery Actions (CEMPRA) is a cumulative effects modelling framework. The CEMPRA tool uses a series of standardized stressor-response functions to link environmental attributes to the system capacity and productivity of a target species/system. This framework design is as generalizable, simple, and versatile as possible so that users can apply the model to various geographic regions, contexts, systems, and species. As the name suggests, the CEMPRA tool helps prioritize recovery actions for data-limited species and species-at-risk, with the flexibility to  accommodate both data-rich and data-poor study systems. The CEMPRA tool is accessible as an open-source R package (https://github.com/essatech/CEMPRA) and R Shiny interactive web application (https://github.com/essatech/CEMPRAShiny).


## Project Components

-   GitHub Repository for R-Package (<https://github.com/essatech/CEMPRA>)
-   GitHub Repository for R-Shiny Application (<https://github.com/essatech/CEMPRAShiny>)
-   LIVE (online R-Shiny Application) (<https://essa.shinyapps.io/CEMPRAShiny/>)
-   R-Package Tutorials (<https://essatech.github.io/CEMPRA/index.html>)
-   Guidance Document: (<https://mattjbayly.github.io/CEMPRA_documentation/>)


**[Shiny App Tutorial Video](https://youtu.be/Ln9EYi_NVPo)** [access the tool](https://essa.shinyapps.io/CEMPRAShiny)

[![Tutorial Video](./man/figures/shiny_app_cover.png)](https://youtu.be/Ln9EYi_NVPo)


## Package Contributors:
This is a broad collaboration between Fisheries and Oceans Canada (DFO), B.C. Ministry of Environment and Climate Change Strategy (ECCS), Alberta Environment and Parks (AEP), and Simon Fraser University (SFU). 

Contributors include:
-   [Matthew Bayly](https://github.com/mattjbayly): MJBA, Core application development.
-   Alexandra Tekatch: ESSA, Core application development.
-   [Jordan Rosenfeld](http://www.aferu.ca/rosenfeld-lab): Project design and coordination; ECCS Aquatic Ecologist
-   [Lauren Jarvis](https://github.com/andrewpaul68): Collaborator; AEP Research Scientist
-   [Andrew Paul](https://github.com/andrewpaul68): Collaborator; AEP Research Scientist
-   [Eva Enders](https://profils-profiles.science.gc.ca/en/profile/eva-enders): Project Lead; DFO Research Scientist
-   [Kyle Wilson](https://github.com/klwilson23): Population model development.
-   [Isuru Dharmasena](https://www.linkedin.com/in/isuru-dharmasena-90269895/?originalSubdomain=ca): Core Shiny app development
(https://github.com/julianheavyside) from [ESSA Technologies Ltd](https://essa.com/): R package and Shiny app development support.
-   Alejandra Urcelay
-   Pedro Gonzalez
-   Marc Porter
-   Julian Heavyside


## Installation

The easiest way to install the `CEMPRA` package is from within the [RStudio IDE](https://www.rstudio.com/products/rstudio/download/) using `remotes::install_github()`. At this time the package has not been published to CRAN so the default `install.packages()` will not work. Instead use remotes (or devtools) to install the packge directly from GitHub:
``` r
# You may need to install remotes
library(remotes)
remotes::install_github("essatech/CEMPRA")
```

## Usage
There are several vignettes available that provide detailed guidance for `CEMPRA` usage and common workflows. See articles availabe in the guidance document [CEMPRA project page](https://mattjbayly.github.io/CEMPRA_documentation/):

- [Setup and installation](https://mattjbayly.github.io/CEMPRA_documentation/04_initial_setup.html)
- [Data inputs](https://mattjbayly.github.io/CEMPRA_documentation/05_data_inputs.html)

## Code of Conduct

Please note that the `CEMPRA` package is released with a [Contributor Code of Conduct](https://pkgs.rstudio.com/rmarkdown/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.

## Contributing

If you want to contribute to the project you can use the `git` "fork and pull request" workflow.

Rebuilding R-package locally

Helpful commands to run to reploy package locally

```r
# Build and test package
library(testthat)
library(devtools)
rm(list = ls())      # Clear memory
devtools::load_all() # Load functions
devtools::document() # Update documentation
devtools::test()     # Run tests
devtools::check()    # Operating system test
```

Re-install R-package locally

Helpful commands to install package from source

```r
remove.packages("CEMPRA") # Remove local installation
install.packages(getwd(), repos = NULL, type = "source")
# library(CEMPRA)
# ?SystemCapacity
```
