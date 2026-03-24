## Comments

This submission fixes the issues in the CRAN checks:

 - Installation failed (due to CVXR API changes)
 
It also adds a new dataset to avoid a dependency and ensures the vignette can 
be built without the suggested package kableExtra.

## Test environments

1. (Local) macOS 26.3.1, R 4.5.3
2. (Win-builder) Windows Server 2022, R-devel (2026-03-21 r89670 ucrt)
3. (GitHub Actions) macOS Sequoia 15.7.4 R 4.5.3, Windows Server 2022 R 4.5.3, Ubuntu 24.04.3 LTS with R-devel (2026-03-20 r89667), R 4.5.3 and R 4.4.3

### Check results

On Win-builder- possibly invalid URLs: https://www.preflib.org/ is redirected to 
https://preflib.github.io/PrefLib-Jekyll/; there is an open issue about 
changing the HTTP redirect to a temporary redirect
https://github.com/PrefLib/PrefLib-Jekyll/issues/1
