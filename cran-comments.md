## Comments

This submission is mainly to fix a bug in the vcov method that causes NA values for a particular parameterization. Some other minor fixes and improvements have been made at the same time.

## Test environments

1. (Local) Ubuntu 18.04.2 LTS, R 3.6.1
2. (R-hub) Fedora Linux, R-devel, clang, gfortran
3. (R-hub) Ubuntu Linux 16.04 LTS, R-release, GCC          
4. (R-hub) Windows Server 2008 R2 SP1, R-devel, 32/64 bit
5. (R-hub) macOS 10.11 El Capitan, R-release
6. (Win-builder) Windows Server 2008 (64-bit), R-devel
    
### Check results

Check 3, 4 & 6 returns a note regarding URLs/DOIs. This is a false alarm: all the links redirect to valid pages on jstor.

## revdepcheck results

The reverse dependencies, ClimMobTools and PLMIX, pass check locally with this version.
