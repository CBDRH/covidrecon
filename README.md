
<!-- README.md is generated from README.Rmd. Please edit that file -->

# covidrecon

<!-- badges: start -->

[![DOI](https://zenodo.org/badge/248728805.svg)](https://zenodo.org/badge/latestdoi/248728805)

[![eRum2020::CovidR](https://badgen.net/https/runkit.io/erum2020-covidr/badge/branches/master/churches-tierney-ozcoviz-dashboard-covidrecon-package?cache=300)](https://milano-r.github.io/erum2020-covidr-contest/churches-tierney-ozcoviz-dashboard-covidrecon-package.html)

<!-- badges: end -->

R tools for monitoring effectiveness of COVID-19 control efforts

See also https://github.com/CBDRH/ozcoviz

## Installation

The development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("CBDRH/covidrecon")
```

## Why Another COVID19 R package?

We created an [open source dashboard](https://cbdrh.github.io/ozcoviz/)
to combine value-adding data visualisations with updated statistical
analysis not yet widely seen. We decided to package up a lot of the code
used in the dashboard into this R package.

Now, there are many other R packages for pulling COVID19 data, so why
create another one? A few reasons. The main one being that the COVID9
data formats, and other other R packages are changing rapidly. We wanted
to perform statistical analysis that aren’t widely available. We needed
to ensure the data stayed in a format that was easily usable for our
purposes. We needed something that we could control and add features to
ourselves, that wouldn’t need to rely on from another person.

This package is still changing and evolving, and it’s primary purpose is
to implement code

## Usage

`covidrecon` is designed to add statistical analysis and data
visualisations of covid19 data. It was created to power [this
dashboard](https://cbdrh.github.io/ozcoviz/).

### Pulling data

The data source that we recommend using is from the European CDC. You
can get this data with `covid_latest()`. This will by default cache the
data downloaded for that day. Here is what the data looks like.

``` r
library(covidrecon)
covid <- covid_latest()
#> New names:
#> * dateRep -> date_rep
#> * countriesAndTerritories -> countries_and_territories
#> * geoId -> geo_id
#> * countryterritoryCode -> countryterritory_code
#> * popData2018 -> pop_data2018
#> New names:
#> * dateRep -> date_rep
#> * countriesAndTerritories -> countries_and_territories
#> * geoId -> geo_id
#> * countryterritoryCode -> countryterritory_code
#> * popData2018 -> pop_data2018
#> covid data extracted from 2019-12-31 UTC to 2020-04-14 UTC
covid
#> # A tibble: 10,742 x 13
#>    date                country_region deaths cases cumulative_cases
#>    <dttm>              <chr>           <dbl> <dbl>            <dbl>
#>  1 2020-03-03 00:00:00 Andorra             0     1                1
#>  2 2020-03-14 00:00:00 Andorra             0     1                2
#>  3 2020-03-16 00:00:00 Andorra             0     3                5
#>  4 2020-03-17 00:00:00 Andorra             0     9               14
#>  5 2020-03-18 00:00:00 Andorra             0     0               14
#>  6 2020-03-19 00:00:00 Andorra             0    39               53
#>  7 2020-03-20 00:00:00 Andorra             0    22               75
#>  8 2020-03-21 00:00:00 Andorra             0     0               75
#>  9 2020-03-22 00:00:00 Andorra             0    13               88
#> 10 2020-03-23 00:00:00 Andorra             0    25              113
#> # … with 10,732 more rows, and 8 more variables: cumulative_deaths <dbl>,
#> #   year <dbl>, month <dbl>, week <dbl>, day <dbl>, geo_id <chr>,
#> #   countryterritory_code <chr>, pop_data2018 <dbl>
```

This contains new names for the data (from European CDC)

  - dateRep –\> date\_rep
  - countriesAndTerritories –\> countries\_and\_territories
  - geoId –\> geo\_id
  - countryterritoryCode –\> countryterritory\_code
  - popData2018 –\> pop\_data2018

For full use of the package and the data visualisations, we recommend
that you look at the dashboard,
[`ozcovis`](https://cbdrh.github.io/ozcoviz/).
