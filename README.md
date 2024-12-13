
<!-- README.md is generated from README.Rmd. Please edit that file -->

# blockr.core

<!-- badges: start -->

<!-- badges: end -->

Designed to democratize data analysis, `blockr.core` provides a
flexible, intuitive, and **code-free** approach to building data
pipelines.

## Installation

You can install the development version of blockr.core from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("cynkra/blockr.core")
```

## Example

A single block server instance can be spun up as

``` r
library(blockr.core)
serve(new_dataset_block("iris"))
```
