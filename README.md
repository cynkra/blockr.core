
<!-- README.md is generated from README.Rmd. Please edit that file -->

# blockr2

<!-- badges: start -->

<!-- badges: end -->

Designed to democratize data analysis, `blockr2` provides a flexible,
intuitive, and **code-free** approach to building data pipelines.

## Installation

You can install the development version of blockr2 from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("cynkra/blockr2")
```

## Example

A single block server instance can be spun up as

``` r
library(blockr2)
serve(new_dataset_block("iris"))
```
