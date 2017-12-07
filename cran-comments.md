## Resubmission

As requested the DESCRIPTION now contains a reference for the Plackett-Luce model.

## Test environments

(Re-tested with the same results)

* Local
 - Ubuntu 14.04, R 3.4.3
 - Ubuntu 14.04, R Under development (unstable) (2017-12-04 r73829)
 - Windows 8, R 3.4.3
 
* Via R-hub 
 - Mac OS 10.11 El Capitan, R-release (experimental)
 
## Comments

On Mac/Ubuntu, R CMD check only returns note that this is a new submission.

On Windows, R CMD check returns the same note in the log, however in the terminal there is the following warning:

   * checking CRAN incoming feasibility ...Warning: running command '"pandoc" "C:\Users\hturner\PlackettLuce.Rcheck\00_pkg_src\PlackettLuce\README.md" -s --email-obfuscation=references --self-contained -o "C:\Users\hturner\AppData\Local\Temp\RtmpyQZw3Z\READMEcf025d05abf.html"' had status 99

The warning seems to be due to pandoc not finding image files in `man/figures`. I could successfully run the pandoc command by either changing directory to "C:\Users\hturner\PlackettLuce.Rcheck\00_pkg_src\PlackettLuce" and calling pandoc on "README.md", or adding `--resource-path "C:\Users\hturner\PlackettLuce.Rcheck\00_pkg_src\PlackettLuce` to the call shown in the warning.
