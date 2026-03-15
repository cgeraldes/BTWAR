## R CMD check results

0 errors | 0 warnings | 2 notes

## Notes

* checking for future file timestamps: unable to verify current time
  This is a known issue in sandboxed environments and is not actionable.

* checking top-level files: 'cran-comments.md'
  This file is intentionally included for CRAN submission comments.

## Resubmission

This is a resubmission addressing the following points raised by the reviewer:

* Removed single quotes from non-software terms in Title and Description.
* Added 'BTWAR' in single quotes as it is the package name.
* Added DOI reference in Description: Bras-Geraldes, Rocha and Martins (2026) <doi:10.3390/math14030479>.
* Replaced all \dontrun{} with \donttest{} throughout the package examples.
* Added new function plot_amplitude() with full documentation and vignette example.
* Fixed example in poles_AR() where fit object was not defined.
* Fixed a bug in plot_bode() where the frequency axis was linearly spaced instead of log-spaced, causing distortion on the logarithmic scale; the example was also updated to pass fs = 12000 consistently.
* Updated all plot function examples to use simulate_ar_split() for consistency with the vignette.
