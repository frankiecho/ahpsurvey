## ahpsurvey 0.2.2

* Fixed a bug where `ahp.missing` throws an error when a mix of complete and incomplete pairwise matrices is passed through it.

* Fixed the html vignette file

## ahpsurvey 0.2.1

Responded to CRAN maintainer Uwe Ligges's comments:

* Added reference about the method in the Description field in the form Authors (year) <doi:.....>

* Corrected the MIT license based on the CRAN template

## ahpsurvey 0.2.0

Removed the `eigen` option in the `ahp.indpref` and `ahp.aggpref` functions -- now users have to specify `eigen` with `method = "eigen"`.


## ahpsurvey 0.1.0

* Fresh release

* PDF vignette to be added for accurate output of the vignette

* Checked with no errors on `build_win()` and on local machine running OSX High Sierra
