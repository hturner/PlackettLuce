## Comments

This update addresses the errors resulting from the change of method for 
generating from a discrete uniform distribution.

## Test environments

* Local
 - Ubuntu 18.04.2 LTS, R 3.5.3
 - Ubuntu 18.04, R Under development (unstable) (2019-03-19 r76252)
 - Windows 8, R 3.5.3
 
* Via R-hub 
 - Mac OS 10.11 El Capitan, R-release (experimental)

### Check results

I get one warning and one note on R-hub macOS.

* The warning `pandoc: Could not fetch https://www.r-pkg.org/badges/version/PlackettLuce` is due to missing https support in an older version of pandoc.
* The note `Found the following (possibly) invalid URL/DOIs` is a false alarm, I have checked the URLs work and the DOIs resolve (all go to jstor.org).
