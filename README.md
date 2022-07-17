
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rpsi

[![](https://cranlogs.r-pkg.org/badges/pkgdepR)](https://cran.r-project.org/package=pkgdepR)
[![](http://cranlogs.r-pkg.org/badges/grand-total/pkgdepR)](https://cran.r-project.org/package=pkgdepR)
[![R build
status](https://github.com/edpeyton/pkgdepR/workflows/R-CMD-check/badge.svg)](https://github.com/edpeyton/pkgdepR/actions/)
[![CRAN
status](https://www.r-pkg.org/badges/version/pkgdepR)](https://CRAN.R-project.org/package=pkgdepR)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)

# Introduction

The population stability index (PSI) is a commonly applied metric to
evaluate how much a (discrete) variable has shifted in distribution over
time. It is widely used in the field of credit risk modelling for
diagnosing changes in the characteristics of a population over time and
the consequential effects on model performance.

Unfortunately, there is a tendency is the field for the importance of
the PSI to be misstated. Practitioners of the PSI (particularly model
validators) often define arbitrary thresholds for the PSI, which when
crossed, they interpret as meaningful indicators of some underlying
change in behaviour. These arbitrary thresholds are typically values of
5%, 10%, etc. and are often mapped to outputs of *Very stable*,
*Stable*, …, *Very unstable*. These values are common in the credit risk
literature but are without justification.

In this package we try to address this blind application of the PSI in
industry by applying the statistical properties discussed in
<a href="#links">\[1\]</a>

## Moving away from arbitrary thresholds

Better critical values have already been derived (Yurdakul, 2018). The
PSI was shown to have an approximate *χ*<sup>2</sup> distribution. That
is

``` math
\text{PSI}\sim\chi^{2}_{\alpha,B-1}\cdot(1/M+1/N)
```

## Contributors

[Ed Peyton](https://github.com/edpeyton)

------------------------------------------------------------------------

<a href="#top">Back to top</a>
