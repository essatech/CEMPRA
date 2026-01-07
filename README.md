# CEMPRA <img src="man/figures/CEMPRA_small.png" align="right" style="max-width: 120px;"/>

<!-- badges: start -->
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R-CMD-check](https://github.com/essatech/CEMPRA/workflows/R-CMD-check/badge.svg)](https://github.com/essatech/CEMPRA/actions)
<!-- badges: end -->

The Cumulative Effects Model for Prioritizing Recovery Actions (CEMPRA) is a cumulative effects modelling framework. The CEMPRA tool uses a series of standardized stressor-response functions to link environmental attributes to the system capacity and productivity of a target species or system.

The framework is designed to be generalizable, simple, and versatile so that users can apply the model to various geographic regions, contexts, systems, and species. As the name suggests, the CEMPRA tool helps prioritize recovery actions for data-limited species and species-at-risk, with the flexibility to accommodate both data-rich and data-poor study systems.

The CEMPRA tool is accessible as an open-source R package and R Shiny interactive web application.


## Key Features

- **Stressor-Response Framework**: Link environmental stressors to system capacity using customizable dose-response relationships
- **Joe Model**: Calculate cumulative system capacity across multiple stressors and locations
- **Stage-Structured Population Model**: Project population dynamics with life-stage-specific vital rates
- **Density-Dependence Options**: Support for Beverton-Holt and Hockey-Stick density-dependent constraints at any life stage
- **Anadromous & Resident Life Histories**: Model both anadromous (salmon) and non-anadromous (resident) populations
- **Batch Processing**: Run scenarios across multiple locations and stressor combinations
- **Sensitivity Analysis**: Evaluate parameter uncertainty and identify key drivers


## Project Components

- GitHub Repository for R-Package: <https://github.com/essatech/CEMPRA>
- GitHub Repository for R-Shiny Application: <https://github.com/essatech/CEMPRAShiny>
- LIVE Online R-Shiny Application: <https://essa.shinyapps.io/CEMPRAShiny/>
- R-Package Tutorials: <https://essatech.github.io/CEMPRA/index.html>
- Guidance Document (Realtime Updates): <https://mattjbayly.github.io/CEMPRA_documentation/>
- Guidance Document (2024 pdf): <https://www.essa.com/wp-content/uploads/2024/05/Guideline_Documentation_Report-2024.03.14.pdf>


## Shiny App

**[Shiny App Tutorial Video](https://youtu.be/Ln9EYi_NVPo)** | [Access the tool](https://essa.shinyapps.io/CEMPRAShiny)

[![Tutorial Video](./man/figures/shiny_app_cover.png)](https://youtu.be/Ln9EYi_NVPo)


## Installation

The easiest way to install the `CEMPRA` package is from within the [RStudio IDE](https://www.rstudio.com/products/rstudio/download/) using `remotes::install_github()`. At this time the package has not been published to CRAN so the default `install.packages()` will not work. Instead use remotes (or devtools) to install the package directly from GitHub:

``` r
# You may need to install remotes
library(remotes)
remotes::install_github("essatech/CEMPRA")
```


## Tutorials

Detailed tutorials are available as package vignettes:

| Tutorial | Description |
|----------|-------------|
| [1. Joe Model Overview](https://essatech.github.io/CEMPRA/articles/a01-joe-model.html) | Introduction to the Joe Model for cumulative effects assessment |
| [2. Joe Model Batch Run](https://essatech.github.io/CEMPRA/articles/a02-joe-model-batch-run.html) | Running the Joe Model across multiple locations |
| [3. Population Model Overview](https://essatech.github.io/CEMPRA/articles/a03-population-model.html) | Introduction to stage-structured population modeling |
| [4. Population Model Batch Run](https://essatech.github.io/CEMPRA/articles/a04-population-model-batch-run.html) | Running population projections across multiple scenarios |
| [5. BC CEF Data Import](https://essatech.github.io/CEMPRA/articles/a05-bc-cef.html) | Importing data from BC Cumulative Effects Framework |
| [6. Population Model Evaluation](https://essatech.github.io/CEMPRA/articles/a06-population-model-sensitivity.html) | Sensitivity analysis and model evaluation |

Additional guidance is available in the [CEMPRA Documentation](https://mattjbayly.github.io/CEMPRA_documentation/):

- [Setup and Installation](https://mattjbayly.github.io/CEMPRA_documentation/04_initial_setup.html)
- [Data Inputs](https://mattjbayly.github.io/CEMPRA_documentation/05_data_inputs.html)


## Package Contributors

This is a broad collaboration between Fisheries and Oceans Canada (DFO), B.C. Ministry of Environment and Climate Change Strategy (ECCS), Alberta Environment and Parks (AEP), and Simon Fraser University (SFU).

Contributors include:

- [Matthew Bayly](https://github.com/mattjbayly): MJBA, Core application development
- [Alexandra Tekatch](https://github.com/alextekatch): ESSA, Application development
- [Jordan Rosenfeld](http://www.aferu.ca/rosenfeld-lab): Project design and coordination; ECC Aquatic Ecologist
- [Andrew Paul](https://github.com/andrewpaul68): Collaborator; AEP Research Scientist
- Lauren Jarvis: Collaborator; AEP Research Scientist
- [Eva Enders](https://profils-profiles.science.gc.ca/en/profile/eva-enders): Project Lead; DFO Research Scientist
- [Kyle Wilson](https://github.com/klwilson23): Population model development
- [Isuru Dharmasena](https://www.linkedin.com/in/isuru-dharmasena-90269895/): Core Shiny app development
- [Julian Heavyside](https://github.com/julianheavyside): [ESSA Technologies Ltd](https://essa.com/), R package and Shiny app development support
- Alejandra Urcelay
- Pedro Gonzalez
- Marc Porter


## Citation

If you use CEMPRA in your research, please cite it as:

```
Bayly, M., Tekatch, A., Rosenfeld, J., Paul, A., Jarvis, L., Enders, E., & Wilson, K. (2024).
CEMPRA: Cumulative Effects Model for Prioritizing Recovery Actions.
R package. https://github.com/essatech/CEMPRA
```


## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.


## Code of Conduct

Please note that the `CEMPRA` package is released with a [Contributor Code of Conduct](https://pkgs.rstudio.com/rmarkdown/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.


## Reporting Bugs and Issues

To report bugs or request features, please open an issue at: <https://github.com/essatech/CEMPRA/issues>


### Rebuilding R-package locally

Helpful commands to run to deploy package locally:

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


### Re-install R-package locally

Helpful commands to install package from source:

```r
remove.packages("CEMPRA") # Remove local installation
install.packages(getwd(), repos = NULL, type = "source")
# library(CEMPRA)
# ?SystemCapacity
```
