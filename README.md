
<!-- README.md is generated from README.Rmd. Please edit that file -->

# <img src="https://i.imgur.com/m8FNhQR.png" align="right" height=88 /> Common fgeo functions with no external depencency

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/fgeo.base)](https://cran.r-project.org/package=fgeo.base)
[![Travis build
status](https://travis-ci.org/forestgeo/fgeo.base.svg?branch=master)](https://travis-ci.org/forestgeo/fgeo.base)
[![Coverage
status](https://coveralls.io/repos/github/forestgeo/fgeo.base/badge.svg)](https://coveralls.io/r/forestgeo/fgeo.base?branch=master)

The goal of **fgeo.base** is to provide functions with no external
depencency. This makes it easy to import into any **fgeo** package
withouth carrying any dependency, which helps keep each **fgeo** package
as small and independent as possible.

## Installation

    # install.packages("remotes")
    remotes::install_github("forestgeo/fgeo.base")

For details on how to install packages from GitHub, see [this
article](https://goo.gl/dQKEeg).

## Example

This is a basic example which shows you how to solve a common problem:

``` r
## TODO: Add example
```

## Information

  - [Getting help](SUPPORT.md).
  - [Contributing](CONTRIBUTING.md).
  - [Contributor Code of Conduct](CODE_OF_CONDUCT.md).

## Acknowledgements

Thanks to all partners of ForestGEO, for sharing their ideas and code.

## EDIT: READ AND DELETE THIS SECTION

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

You can also embed plots, for example:

![](README-pressure-1.png)<!-- -->

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub\!
