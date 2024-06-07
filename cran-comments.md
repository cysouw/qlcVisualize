## qlcVisualize, version 0.3

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

- local macOS X 14.5, R version 4.4.0
- rhub::rhub_check() for Linux, Windows and old macOS

## R CMD check results

There were 0 errors, 1 warning, 0 notes

## Downstream dependencies

checked via revdepcheck::revdep_check() on local macOS X 14.5
None found
