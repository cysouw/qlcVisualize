## Test environments
* local OS X 10.11 install, R 3.2.2
* CRAN winbuilder via devtools

## R CMD check results
There were no ERRORs or WARNINGs. 

There is a WARNING because of non-ASCII characters in the supplied exemplary data

    * checking data for non-ASCII characters ... WARNING
      Note: found 624 marked UTF-8 strings
      Warning: found non-ASCII strings

This is found in words from non-European languages and some village names in Germany. I would really like to keep this data in UTF-8 included, as the whole point of this package is using language data from many different languages and scripts.

## Downstream dependencies
There are no downstream dependencies (yet)
