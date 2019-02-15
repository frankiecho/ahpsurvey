## ahpsurvey 0.4.0

* Allows for an ID column/ columns for the `ahp` routine so that the output can preserve some column with an individual identifier(s).

* Adds an `col` argument for `ahp` to specify the columns and its order of the pairwise comparison variables directly to `ahp`.

## ahpsurvey 0.3.1

* Added `ahp.ri`, which allows users to self-generate random indices used to calculate the consistency ratio.

* Replaced default values of RI in `ahp.cr` with values generated in `ahp.ri` with 500000 simulations.

## ahpsurvey 0.3.0

* Added a new canned routine, `ahp`, which provides a detailed output using some of the best functions in `ahpsurvey`.

* Edited wording in the documentation to make it a bit more consistent.

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
