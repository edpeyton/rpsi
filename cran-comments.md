## Resubmission (2)
This is a resubmission. In this version I have:

* Added the citation to the description field of the DESCRIPTION file and the <doi:...>
* Have added the missing \value to .Rd files
* The output class 'rpsi' is outlined in ?rpsi which has a link from the @return tag of rpsi::psi
  * if this is not sufficient I'll need to copy over the outline to the @return tag of rpsi::psi
* Have wrapped the example in a \donttest{} and reduced the example complexity
  * on the systems I tested on the runtime wasn't >5s but the above changes should circumvent that regardless
  * 

## Resubmission (1)
This is a resubmission. In this version I have:

* Wrapped the examples in \dontrun to prevent the two NOTEs on elapsed time

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs. 

Package has been deployed on macOS, Windows and Ubuntu via GitHub Actions:
(https://github.com/edpeyton/rpsi/actions).

* macOS-latest (release)
* windows-latest (release)
* ubuntu-latest (devel)
* ubuntu-latest (release)
* ubuntu-latest (oldrel-1)

Also tested on (`devtools::check_win_devel(..., manual = FALSE)`) 

* windows (devel)

Github repo available:
https://github.com/edpeyton/rpsi
