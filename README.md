
<!-- README.md is generated from README.Rmd. Please edit that file -->

# covidmodeldata

<!-- badges: start -->

<!-- badges: end -->

The goal of covidmodeldata is to download and format data

## Installation

``` r
remotes::install_github("covidmodeldata")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(covidmodeldata)
library(tidyverse)

df <- get_nyt() %>%
  format_nyt(
    skip_assignment = c("44") # don't assign Rhode Island cases
    )
```
