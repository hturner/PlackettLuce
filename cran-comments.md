## Comments

This is a re-submission fixing a bug that was found by an example in one of the reverse dependencies: ClimMobTools. This version causes warnings in ClimMobTools due to deprecated functions, which the authors have fixed in their development version (0.2-7, https://github.com/agrobioinfoservices/ClimMobTools).

## Test environments

* Ubuntu 18.04.2 LTS, R 3.6.1
* Via Win-builder: Windows Server 2008 (64-bit), R-devel
    
### Check results

The Win-builder check returns a note regarding URLs/DOIs that is a false alarm, all the links redirect to valid pages on jstor.
