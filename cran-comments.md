## Test environments
* local OS X 10.11 install, R 3.2.2
* CRAN winbuilder via devtools

## R CMD check results
There were no ERRORs. 

There is a WARNING because of non-ASCII characters in the data supplied. This is found in words from non-European languages and some village names in Germany. I would really like to keep this data in UTF-8 included, as the whole point of this package is using language data from many different languages and scripts.

    * checking data for non-ASCII characters ... WARNING
      Note: found 624 marked UTF-8 strings
      Warning: found non-ASCII strings

There is a NOTE about the license. I have tried to add a license, but I am no lawyer...

    Package has a FOSS license but eventually depends on the following
    packages which restrict use:
      alphahull, tripack

Further: The winbuilder notes that there are extensive examples, I can put even more examples inside \dontrun if really necessary, but I'd rather not.

    Examples with CPU or elapsed time > 5s
         user system elapsed
    lmap 4.81   0.19    5.06

## Downstream dependencies
There are no downstream dependencies (yet)
