
<!-- README.md is generated from README.Rmd. Please edit that file -->

# <img src="https://i.imgur.com/vTLlhbp.png" align="right" height=88 /> Independent utility functions

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/fgeo.base)](https://cran.r-project.org/package=fgeo.base)
[![Travis build
status](https://travis-ci.org/forestgeo/fgeo.base.svg?branch=master)](https://travis-ci.org/forestgeo/fgeo.base)
[![Coverage
status](https://coveralls.io/repos/github/forestgeo/fgeo.base/badge.svg)](https://coveralls.io/r/forestgeo/fgeo.base?branch=master)

The goal of **fgeo.base** is to provide functions with no external
dependency that can easily be imported into any other **fgeo** package.

**fgeo.base** is mostly a repository of internal functions for other
**fgeo.packages**: it hosts the kind of functions you would generally
place in utils.R – except it can be accessed from any other **fgeo**
package. As such, **fgeo.base** is not directly exposed to users,
although a few functions are reexported by other **fgeo** packages.

## Installation

    # install.packages("devtools")
    devtools::install_github("forestgeo/fgeo.base")

For details on how to install packages from GitHub, see [this
article](https://goo.gl/dQKEeg).

## Example

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

## Information

  - [Getting help](SUPPORT.md).
  - [Contributing](CONTRIBUTING.md).
  - [Contributor Code of Conduct](CODE_OF_CONDUCT.md).
