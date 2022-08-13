## Comments

This submission fixes the issue in the CRAN checks (broken test due to update in survival package).

## Test environments

1. (Local) Ubuntu 20.04.2 LTS, R 4.2.1
2. (Win-builder) Windows Server 2008 (64-bit), R-devel (2022-08-11 r82712 ucrt)
3. (GitHub Actions) MacOS Big Sur R 4.2.1, Windows Server 2022 R 4.2.1, Ubuntu 20.04 with R-devel (2022-08-07 r82694), R 4.2.1 and R 4.1.3

### Check results

Local and Wind builder checks return a note regarding URLs/DOIs. This is a false alarm, I have checked all links manually
 - https://doi.org/10.1214/aos/1079120141 -> projecteuclid.org
 - https://doi.org/10.2307/2346567 -> jstor.org
 - https://doi.org/10.2307/2348134 -> jstor.org
 - https://doi.org/10.3102/1076998609359791 -> journals.sagepub.com
 - https://www.preflib.org/ - this one passes check intermittently, loads okay manually
