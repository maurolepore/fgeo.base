
<!-- README.md is generated from README.Rmd. Please edit that file -->

# <img src="https://i.imgur.com/m8FNhQR.png" align="right" height=88 /> General functions with no external depencency

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

[Install all **fgeo** packages in one
step](https://forestgeo.github.io/fgeo/index.html#installation)

    # install.packages("remotes")
    remotes::install_github("forestgeo/fgeo.base")

For details on how to install packages from GitHub, see [this
article](https://goo.gl/dQKEeg).

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(fgeo.base)

dfm <- data.frame(a = 1, b = NA)
drop_if_na(dfm, "b")
#> Warning: Dropping 1 rows with missing `b` values.
#> [1] a b
#> <0 rows> (or 0-length row.names)

drop_if_na(dfm, "a")
#>   a  b
#> 1 1 NA
```

[Get started](https://forestgeo.github.io/fgeo/articles/fgeo.html)

## Information

  - [Getting help](SUPPORT.md).
  - [Contributing](CONTRIBUTING.md).
  - [Contributor Code of Conduct](CODE_OF_CONDUCT.md).

## Acknowledgements

Thanks to all partners of ForestGEO, for sharing their ideas and code.
