## qlcVisualize, version 0.2.1

New submission because package was archived on CRAN
Package was archived on 2019-01-25 as issues were not corrected in time.
This submission intends to simply get everything working again.

## Comments to second review

- The word "Voronoi" is now capitalized throughout all text in the package
- The methods implemented in this package are not yet published. However,
  what is called "level-map" here is simply an implementation of widely-used
  visualizations, normally drawn by hand (as explained in the Rd file). Voronoi
  diagrams of course are well-known. Finally, the method "level-image" is
  completely new AFAIK, explicitly made for my own research (which I hope will
  be publishable soon).

## Comments to review

- Description in DESCRIPTION has been extended.
- There are some literature references in the Rd-files, but they pertain to
  individual function, not to the packages as a whole. It does not seem sensible
  to elevate them to the level of the whole package.
- All example marked as \dontrun have some problem which preclude them from being 
  passed through CRAN testing as \donttest. This is explained in the actual 
  examples. There is one example that takes a long time to download data, and 
  there are two examples that have Unicode phonetic symbols that result in 
  errors on some machines. I would still like to keep these examples, as they 
  show the linguistic community why these function are actually useful (other 
  than for some made-up simple examples).
- Changing of "par" had been revised to a more sensible strategy as suggested 
  (with "on.exit")

## Test environments

- local macOS X 14.4.1, R version 4.3.0
- CRAN win-builder https://win-builder.r-project.org, R version 4.4.0
- rhub::rhub_check() for Linux and old macOS

note: Windows on rhub_check failed to load package "terra"

## R CMD check results

There were no ERRORs, NOTEs or WARNINGs. 

## Downstream dependencies

checked via revdepcheck::revdep_check() on local macOS X 14.2.1
None found
