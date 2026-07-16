# Generate random indices

The `ahp.ri` function calculates the mean consistency indices of a
specific numbers of random number pairwise comparison matrices.

The random index of one pairwise comparison matrix is given as below,
where \\lambda\\ is the maximum eigenvalue and \\n\\ is the number of
attributes.

\$\$RI = (\lambda-n)/((n-1)\$\$

`ahp.ri` creates `nsims` number of pairwise comparison matrices with
number of dimensions=`dim`, and returns its average.

## Usage

``` r
ahp.ri(nsims, dim, seed = 42)
```

## Arguments

- nsims:

  Number of random pairwise comparison matrices to be generated.
  Processing time increases substantially with higher `nsims`.

- dim:

  Number of dimensions of the matrix.

- seed:

  The random number generator seed for reproducibility, which is same as
  `set.seed`. By default, `seed = 42`.

## Value

The generated random index, which is numeric.

## Author

Frankie Cho

## Examples

``` r

ahp.ri(nsims = 10000, dim = 5, seed = 42)
#> [1] 1.108557

```
