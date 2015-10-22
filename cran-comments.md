## This is a resubmission

I corrected the following errors

* Changed the License to a non-FOSS license to comply with an imported package that has a non-FOSS license
* Removed vignette builder from DESCRIPTION for now
* Added import for methods::slot
* Declared the non-ASCII data as UTF-8

## Test environments
* local OS X 10.11 install, R 3.2.2
* CRAN winbuilder via devtools

## R CMD check results
There were no ERRORs or WARNINGs. 

There is a NOTE about the presence of non-ASCII in the included data

    checking data for non-ASCII characters ... NOTE
    Note: found 727 marked UTF-8 strings

The winbuilder notes that there are extensive examples, I can put even more examples inside \dontrun if really necessary, but I'd rather not.

    Examples with CPU or elapsed time > 5s
         user system elapsed
    lmap 4.81   0.19    5.06

## Downstream dependencies
There are no downstream dependencies (yet)
