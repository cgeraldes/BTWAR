# BTWAR 1.0.1

## Bug fixes

* Fixed a bug in `plot_bode()` where the frequency axis was linearly spaced
  instead of log-spaced, causing distortion on the logarithmic scale.

## New features

* Added `plot_freq_amplitude()` to display the frequency amplitude spectrum of a
  time series with reference lines at the cutoff and Nyquist frequencies.

## Documentation

* Fixed example in `poles_AR()` where `fit` object was not defined.
* Updated all plot function examples to use `simulate_ar_split()` for
  consistency with the vignette.
* Removed `set.seed()` calls from examples.
* Replaced all `\dontrun{}` with `\donttest{}` throughout the package.
* Added DOI reference in `DESCRIPTION`.
* Removed single quotes from non-software terms in `DESCRIPTION`.

# BTWAR 1.0.0

* Initial CRAN submission.
