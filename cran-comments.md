# qlcVisualize, version 0.3

  New submission
  
  Package was archived on CRAN
  
  CRAN repository db overrides:
    X-CRAN-Comment: Archived on 2024-05-27 as requires archived package
      'geodata'.

## Comments on automatic incoming checks

- Package 'geodata' is back on CRAN
- The spelling of "Voronoi" is correct
- Reference to package "deldir" is removed in Rd
- I have reduced the examples to an absolute minimum running time. 

Note: There are various individual examples that each cannot be reduced further. 
I could only reduce the time by removing examples, which I would rather not do.

## Changes

- Update to move towards library 'sf' for geographic routines
- Deprecating various functions that rely on outdated functions
- Replacing them with a new approach 'weightedMap'

## Known issues

The runtime of the examples is high, but reducing would result in warnings 
of failing convergence of the function 'weightedMap'.

There is an S3 conflict between two dependencies: both the package "sf" and
the package "seriation" appear to overwrite the method 'print.registry_field'
from 'proxy'.

## Test environments

- devtools::check(remote = TRUE, manual = TRUE) on local macOS 14.5 install, R version 4.4.0
- devtools::check_win_devel() for Windows on r-devel
- rhub::rhub_check() for Linux, Windows and old macOS

## R CMD check results

There were 0 errors, 0 warning, 1 notes

\donttest examples in the Rd of 'weightedMap' gives a warning to the user, 
which is intentional as an example to the functionality.

## Downstream dependencies

checked via revdepcheck::revdep_check() on local macOS X 14.5
None found
