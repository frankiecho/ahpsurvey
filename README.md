
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ahpsurvey

[![Build
Status](https://travis-ci.org/frankiecho/ahpsurvey.svg?branch=master)](https://travis-ci.org/frankiecho/ahpsurvey)

## Overview

The `ahpsurvey` package provides a consistent methodology for
researchers to reformat data and run the analytic hierarchy process
(AHP), introduced by Thomas Saaty, on data that are formatted with the
survey data entry mode. It is optimised for performing the AHP with many
decision-makers, and provides tools and options for researchers to
aggregate individual preferences and concurrently test multiple
aggregation options. It also allows researchers to quantify, visualise
and correct for inconsistent pairwise comparisons.

## Installation

You can install `ahpsurvey` from Github with:

``` r
# install.packages("devtools")
devtools::install_github("frankiecho/ahpsurvey")
```

A CRAN submission is under way and a direct CRAN download will be
available in the near future.

## Usage

Here, we have the simulated survey data of pairwise comparisons of 200
decision-makers who responded to a survey about the attributes they
think is important when choosing a city to live in.

``` r
library(ahpsurvey)
library(magrittr)

data(city200)
city200 %>% head()
#>   cult_fam cult_house cult_jobs cult_trans fam_house fam_jobs fam_trans
#> 1        2         -2         2         -6        -4       -4        -8
#> 2        2         -4         1         -4        -4       -2        -8
#> 3        4         -2         1         -3        -7       -3        -5
#> 4        8         -4         3         -4        -8        1        -7
#> 5        3         -3         5         -6        -8        1        -4
#> 6        6         -4         2         -4        -7       -2        -4
#>   house_jobs house_trans jobs_trans
#> 1          4          -3         -8
#> 2          4          -3         -7
#> 3          4          -3         -6
#> 4          4          -3         -9
#> 5          4          -3         -6
#> 6          4          -3         -6
```

`ahpsurvey` allows us to convert this `data.frame` into a usable list of
pairwise comparison matrices for our further action:

``` r
## Define the attributes used
atts <- c("cult", "fam", "house", "jobs", "trans")

city200 %>%
  ahp.mat(atts = atts, negconvert = TRUE) %>%
  head(2)
#> [[1]]
#>            cult   fam     house  jobs trans
#> cult  1.0000000 0.500 2.0000000 0.500     6
#> fam   2.0000000 1.000 4.0000000 4.000     8
#> house 0.5000000 0.250 1.0000000 0.250     3
#> jobs  2.0000000 0.250 4.0000000 1.000     8
#> trans 0.1666667 0.125 0.3333333 0.125     1
#> 
#> [[2]]
#>       cult   fam     house      jobs trans
#> cult  1.00 0.500 4.0000000 1.0000000     4
#> fam   2.00 1.000 4.0000000 2.0000000     8
#> house 0.25 0.250 1.0000000 0.2500000     3
#> jobs  1.00 0.500 4.0000000 1.0000000     7
#> trans 0.25 0.125 0.3333333 0.1428571     1
```

And can calculate the aggregated individual preferences of the 200
decision-makers using that list:

``` r
city200 %>%
  ahp.mat(atts = atts, negconvert = TRUE) %>%
  ahp.aggpref(atts = atts)
#>       cult        fam      house       jobs      trans 
#> 0.15265560 0.44012225 0.07217919 0.28341600 0.03911341
```

Further arguments allow you to specify the aggregation method, impute
missing values and identify and correct inconsistent responses.

## Functions

An overview of the functions in this package are as follows:

  - `ahp.mat`: Generate AHP pairwise matrices from survey data
  - `ahp.indpref`: Priority weights of individual decision-makers
  - `ahp.aggpref`: Aggregate individual priorities (AIP)
  - `ahp.aggjudge`: Aggregate individual judgements (AIJ)
  - `ahp.cr`: Saaty’s Consistency Ratio
  - `ahp.error`: The product between the pairwise comparison value and
    pj/pi
  - `ahp.pwerror`: Finds the pairwise comparisons with the maximum
    amount of inconsistency
  - `ahp.missing`: Impute missing pairwise comparsions
  - `ahp.harker`: Replace inconsistent pairwise comparisons

## Vignettes

For a detailed example of how the above function works, look no further
than the vignettes, which are stored in `/my-vignette.pdf`. There, you
can find a detailed step-by-step instruction of how to use the function
using a simulated survey dataset and visualise the output using
`ggplot2`.

## Future development

I have plans to add the following features in the future, perhaps after
I finish writing up my masters thesis :-(

  - Multiple level of attributes: right now, you can always multiply the
    weights manually, but I’m looking to develop this feature in a
    convenient function
  - Comparing alternatives: or a way to export the matrices to be used
    in other packages which does this
  - Sensitivity analysis
  - More ways to impute missing data
  - Fuzzy AHP (or integration with existing packages)

Please let me know if there are any features which could be useful to
you in a feature request or contribution.

## Author

  - **Frankie Cho** - *Author & Maintainer* -
    [frankiecho](https://github.com/frankiecho)

## License

This project is licensed under the MIT License.
