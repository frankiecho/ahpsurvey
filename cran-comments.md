Dear CRAN volunteers,

Thank you for reviewing the package contents. In this iteration, I addressed a critical bug that was raised in the R CMD check, caused by the use of `ggplot2::labs` in the vignettes of the package, where the function `labs` previously included an unnamed argument that is now prohibited in newer versions. I have now added argument names to usage of `labs` and confirm that all R CMD check passes and are ready for resubmission to CRAN.

In addition, I made the following changes:
- Moved the vignette-only dependencies `knitr`, `randomNames`, and `tidyr` from Imports to Suggests to reduce dependencies
- Added the language setting in the DESCRIPTION field and made all vignette and function descriptions consistent in British English spelling

Best regards,
Frankie Cho


## Test environments
* macOS Big Sur 11.5.2 with R 4.1.1 (local system)

(tested on 22 August 2021 with 0.4.2)

## R CMD check results
0 ERRORs | 0 WARNINGs | 0 NOTES (on local system)

OK using win-builder.

## Downstream dependencies

There are currently no downstream dependencies for this package.