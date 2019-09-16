## Comments

This submission:
  - uses alternative to base::asplit in R < 3.6.0 (to fix errors on CRAN)
  - fixes bug in vcov method that causes NA values for a particular parameterization. 
  
Some other minor fixes and improvements have been made at the same time.

## Test environments

1. (Local) Ubuntu 18.04.2 LTS, R 3.6.1
2. (R-hub) Fedora Linux, R-devel, clang, gfortran
3. (R-hub) Ubuntu Linux 16.04 LTS, R-release, GCC          
4. (R-hub) Windows Server 2008 R2 SP1, R-devel, 32/64 bit
5. (R-hub) macOS 10.11 El Capitan, R-release
6. (Win-builder) Windows Server 2008 (64-bit), R-devel
7. (Win-builder) Windows Server 2008 (64-bit), R-oldrelease
    
### Check results

Checks 3, 4, 6  & 7 return a note regarding URLs/DOIs. This is a false alarm: all the links redirect to valid pages on jstor.org.

Check 7 gives an additional note: "no visible global function definition for 'asplit'". However, the code conditions on R version, so asplit is only called if R >= 3.6.0. This conditional execution is validated by the fact that the examples and tests do not fail on this installation, which has R-3.5.3.

## revdepcheck results

The reverse dependencies, ClimMobTools and PLMIX, pass check locally with this version.
