
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sfdbi <img src='man/figures/logo.svg' align="right" height="139" />

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/r-spatial/sfdbi/branch/master/graph/badge.svg)](https://codecov.io/gh/r-spatial/sfdbi?branch=master)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

## Overview

sfdbi helps you manipulate spatial data stored in a database as if it
was in-memory with standard sf and [dplyr](https://dplyr.tidyverse.org/)
functions. With sfdbi, you can reads and write sf objects to databases
and translate sf operations to `SQL`. We support
[`postgis`](https://postgis.net/), but feel free to open an issue for
other backends and show you interest.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("r-spatial/sfdbi")
```

sfdbi is not available on CRAN yet.

## Example

This is how you write and read spatial data to a database. Note that
sfdbi works best with dplyr.

``` r
library(sfdbi)
library(sf)
#> Linking to GEOS 3.8.0, GDAL 3.0.2, PROJ 6.2.1
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

con <- DBI::dbConnect(
  RPostgres::Postgres(), 
  host = "localhost", 
  port = 25432
) %>%
  postgis()

# Create a spatial table
pyramids <- tribble(  
  ~what, ~geom,
  "Giza",        "POINT(31.1342 29.9792)",
  "Khafre",      "POINT(31.130833 29.976111)", 
  "Menkaure",    "POINT(31.128333 29.9725)",
  "Khentkaus I", "POINT(31.135608 29.973406)", 
  "Sphynx",      "POINT(31.137778 29.975278)",
  ) %>% 
  mutate(
    geom = st_as_sfc(geom, crs = 4326)
  )

# Copy spatial data to database
x <- copy_to(con, pyramids)
#> Note: method with signature 'DBIObject#sf' chosen for function 'dbDataType',
#>  target signature 'PqConnection#sf'.
#>  "PqConnection#ANY" would also be valid

# Queue operations to the database
y <- x %>% 
  mutate(
    geom = geom %>% 
      st_transform(23035L) %>% 
      st_buffer(10)
  )

# View the query to be executed
y %>% show_query()
#> <SQL>
#> SELECT "what", st_buffer(st_transform("geom", 23035), 10.0) AS "geom"
#> FROM "pyramids"

# Execute operation in the database and load it in R memory
collect(y)
#> # A tibble: 5 x 2
#>   what                                                                      geom
#>   <chr>                                                            <POLYGON [m]>
#> 1 Giza       ((899053.3 3323853, 899053.1 3323851, 899052.6 3323849, 899051.7 3…
#> 2 Khafre     ((898740.5 3323498, 898740.3 3323496, 898739.7 3323494, 898738.8 3…
#> 3 Menkaure   ((898513.4 3323089, 898513.2 3323087, 898512.7 3323085, 898511.7 3…
#> 4 Khentkaus… ((899212.6 3323215, 899212.4 3323213, 899211.8 3323211, 899210.9 3…
#> 5 Sphynx     ((899414.7 3323430, 899414.5 3323428, 899413.9 3323426, 899413 332…
```
