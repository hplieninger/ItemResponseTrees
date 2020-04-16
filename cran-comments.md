## Resumbmission

This is a resubmission. Changes from 0.2.3 to 0.2.4:

* Previously, the authorship of the internal function sym_diff()
  was not appropriately declared. To resolve this issue, 
  sym_diff() was replaced with sets::set_symdiff(), and 'sets' 
  was added to Imports.
* Previously, the authorship of the internal function 
  myTryCatch() was not appropriately declared. To resolve this
  issue, myTryCatch() was replaced with
  simsalapar::tryCatch.W.E(), and 'simsalapar' was added to 
  Suggests.
* Previously, the authorship of the internal function sort2() 
  was not appropriately declared. To resolve this issue, sort2()
  was removed. In the previous version, it was used for 
  convenience, but it is actually not needed.

## Test environments
* macOS-latest, R release (via GitHub Actions)
* macOS-latest, R devel   (via GitHub Actions)
* Windows,      R release (local)
* Windows,      R devel   (via win-builder)
* ubuntu-16.04, R release (via GitHub Actions)

## R CMD check results

0 errors | 0 warnings | 1 note

* New submission (hence, 1 note)

## Downstream dependencies

* This package currently has 0 downstream dependencies.
