# Dataframe of one decision-maker based on Saaty (2004)

A data.frame of one decision-maker with weights the same as Saaty (2004)
, with the pairwise comparisons of `atts`:
`c('cult', 'fam', 'house', 'jobs', 'trans')`, about a choosing the best
city to live in based on five attributes: Culture, Family, House, Jobs,
and Transportation. Negative values in the data denote that the
attribute on the left is more important than the right, thus if used
with `ahp.mat`, `negconvert` must be set to `TRUE`.

## Usage

``` r
data(city1)
```

## Format

A data frame with 1 row and 10 variables, which are pairwise comparisons
of `atts`.

## Source

Saaty (2004)
