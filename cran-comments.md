# qlcVisualize, version 0.4

Revision

## Changes

- Update dependency on cartogramR
- Rewrite of 'weightedMap'

## Known issues

There is one /dontrun example, because it involves a download. It is included to show a more involved example

## Test environments

- devtools::check(remote = TRUE, manual = TRUE) on local macOS 14.5 install, R version 4.4.0
- devtools::check_win_devel(args = "--resave-data") for Windows on r-devel
- rhub::rhub_check() for Linux, Windows and old macOS

## R CMD check results

There were 0 errors, 0 warning, 0 notes

## Downstream dependencies

checked via revdepcheck::revdep_check() on local macOS X 14.5
None found
