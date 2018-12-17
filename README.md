
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ahpsurvey

[![Build
Status](https://travis-ci.org/frankiecho/ahpsurvey.svg?branch=master)](https://travis-ci.org/frankiecho/ahpsurvey)
[![license](https://img.shields.io/github/license/mashape/apistatus.svg)](http://choosealicense.com/licenses/mit/)
[![Rdoc](http://www.rdocumentation.org/badges/version/ahpsurvey)](http://www.rdocumentation.org/packages/ahpsurvey)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/ahpsurvey)](https://cran.r-project.org/package=ahpsurvey)
[![CRAN\_time\_from\_release](http://www.r-pkg.org/badges/ago/ahpsurvey)](https://cran.r-project.org/package=ahpsurvey)
[![CRAN\_downloads](https://cranlogs.r-pkg.org/badges/ahpsurvey)](https://cran.r-project.org/package=ahpsurvey)

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

Install `ahpsurvey` directly from CRAN:

``` r
install.packages("ahpsurvey",repos = "http://cran.us.r-project.org")
```

Or, install the development version of `ahpsurvey` from Github with:

``` r
# install.packages("devtools")
devtools::install_github("frankiecho/ahpsurvey")
```

## Usage

The `ahpsurvey` allows one to input a `data.frame` consisting of
pairwise comparisons data collected through questionnaires and output an
informative output of the aggregated priorities of all observations, the
individual priorities, consistency ratios, and the most inconsistent
pairwise comparisons.

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

Take a `data.frame` like that above and calculate the aggregated
priorities of the 200 decision-makers.

``` r
## Define the attributes used
output <- ahp(city200, atts <- c("cult", "fam", "house", "jobs", "trans"), negconvert = TRUE, agg = TRUE)
#> [1] "Number of observations censored = 0"
output$aggpref
#>          AggPref  SD.AggPref
#> cult  0.15261018 0.033564038
#> fam   0.44827276 0.057695635
#> house 0.07052519 0.008844754
#> jobs  0.27579123 0.053734270
#> trans 0.03965027 0.006700507
```

And can show the detailed individual priorities of the 200
decision-makers and the consistency ratio of each decision-maker using
that list:

``` r
head(output$indpref)[1:6]
#>        cult       fam      house      jobs      trans         CR
#> 1 0.1709466 0.4587181 0.08547330 0.2507636 0.03409845 0.06125366
#> 2 0.2291009 0.3935620 0.08292558 0.2531962 0.04121537 0.02962755
#> 3 0.1540045 0.4921905 0.08239372 0.2213908 0.05002052 0.06327989
#> 4 0.1242495 0.4634863 0.06162027 0.3159930 0.03465092 0.09308731
#> 5 0.1521676 0.3556904 0.07239889 0.3748108 0.04493236 0.10604443
#> 6 0.1536560 0.4738939 0.07106456 0.2516808 0.04970479 0.10740624
```

Further arguments allow you to specify the aggregation method, impute
missing values and identify and correct inconsistent responses.

## Functions

An overview of the functions in this package are as follows:

  - `ahp`: A canned AHP routine
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
