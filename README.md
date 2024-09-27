

# <a href="https://syoung9836.github.io/knfi/" style="float: right; margin-right: 110px; margin-top: -15px; "><img src="man/figures/logo.png" align="right" alt="alt text" width="93" /></a>   knfi: Analysis of Korean National Forest Inventory database 

<!-- badges: start -->

[![R-CMD-check](https://github.com/tidyverse/ggplot2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/SYOUNG9836/knfi/actions/workflows/R-CMD-check.yaml)
[![cran version](https://www.r-pkg.org/badges/version/knfi)](https://cran.r-project.org/package=knfi)
[![rstudio mirror downloads](https://cranlogs.r-pkg.org/badges/knfi?color=E664A4)](https://github.com/r-hub/cranlogs.app)

<!-- badges: end -->


## Overview

Understanding the current status of forest resources is essential for monitoring changes in the forest ecosystem and producing related statistics. In South Korea, the National Forest Inventory (NFI) surveys over 4,500 sample plots nationwide every five years and records 70 items, including forest surveys, forest resource surveys, and forest vegetation surveys. Many researchers are using NFI as primary data for research when estimating biomass or analyzing the importance value of each species in the time and space suitable for the research purpose. However, the accumulation of various forest survey data across the country limits the use of vast data. Therefore, this study developed an algorithm that efficiently processes vast amounts of NFI data in time and space using the statistical analysis software R. The developed algorithm first reconstructs NFI in time and space according to the researcher's purpose and performs integrity verification to maintain data quality. Subsequently, summary statistics, biodiversity, importance value, and biomass are analyzed using the verified data. Finally, visualizing the analysis results allows spatio-temporal access to forest ecosystem changes. 

<img src="man/figures/plot_1.png" alt="Plot 1" width="400"/>

Distribution of National Forest Inventory sample plots (left) and sampling design of the National Forest Inventory (right) (Korea Forest Research Institute, 2011)

<img src="man/figures/plot_2.png" alt="Plot 2" width="400"/>

Plot design of National Forest Inventory (Korea Forest Research Institute, 2011)

## Installation

``` r
# The easiest way to get knfi is to install just knfi:
install.packages("knfi")

# Or the development version from GitHub:
remotes::install_github("SYOUNG9836/knfi")
```

