## Comments

This is a re-submission to fix the failed tests on r-oldrel-windows-ix86+x86_64 and r-patched-solaris-x86, plus additional issues on ATLAS and noLD.

The error on r-oldrel-windows-ix86+x86_64 was due to use of `isFALSE` which is no longer used.

The other errors were due to tests that compared the number of iterations between models - these comparisons were unnecessary and have been removed.

## Test environments

* Ubuntu 18.04.2 LTS, R 3.5.3
* Ubuntu 18.04.2 LTS, R-devel (2019-03-31 r76305) --disable-long-double
* Via Win-builder
 - Windows Server 2008 (64-bit), R-devel
 - Windows Server 2008 (64-bit), R-oldrelease
    
### Check results

On Windows there is a note regarding possibly invalid URLs/DOIs which is a false alarm. They are all links to jstor.org.

On Windows R-oldrelease there is a note regarding ORCIDs in the Author field.
