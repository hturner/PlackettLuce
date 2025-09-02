## Comments

This submission fixes the issues in the CRAN checks:

 - Incorrectly escaped braces and missing package anchors in Rd files
 - Failing test with `_R_CHECK_DEPENDS_ONLY_` set to TRUE

## Test environments

1. (Local) macOS 15.6.1 (24G90), R 4.5.1
2. (Win-builder) Windows Server 2022, R-devel (2025-09-01 r88761 ucrt)
3. (GitHub Actions) macOS Sequoia 15.5 R 4.5.1, Windows Server 2022 R 4.5.1, Ubuntu 24.04.3 LTS with R-devel (2025-08-31 r88749), R 4.5.1 and R 4.4.3

### Check results

No errors, warnings or notes.
