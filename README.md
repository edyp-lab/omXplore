<!-- badges: start -->
[![Project Status: Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check-bioc](https://github.com/edyp-lab/omXplore/workflows/R-CMD-check-bioc/badge.svg)](https://github.com/edyp-lab/omXplore/actions?query=workflow%3AR-CMD-check-bioc)
[![R-CMD-check](https://github.com/edyp-lab/omXplore/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/edyp-lab/omXplore/actions/workflows/R-CMD-check.yaml)
[![Bioc release status](http://www.bioconductor.org/shields/build/release/bioc/omXplore.svg)](https://bioconductor.org/checkResults/release/bioc-LATEST/omXplore)
[![Bioc devel status](http://www.bioconductor.org/shields/build/devel/bioc/omXplore.svg)](https://bioconductor.org/checkResults/devel/bioc-LATEST/omXplore)
<!-- badges: end -->



### What is omXplore?

`omXplore` is a [Bioconductor
package](http://bioconductor.org/packages/omXplore) that provides
functions for the visualization and the statistical analysis of proteomics data.
It can deal with common Bioconductor formats such as as Msnset, `QFeatures`, 
`MultiAssayExperiment`.

It is also possible to write your own plot modules so as to embed it into
the GUI of `omXplore`.

> Evolving the `DAPAR` package plots towards Shiny modules.



### Getting started

See the
[omXplore introduction](https://edyp-lab.github.io/omXplore/articles/omXplore.html)
to get started with the visualization of data.



### License

The `omXplore` code is provided under a permissive [Artistic 2.0
license](https://opensource.org/licenses/Artistic-2.0). The
documentation, including the manual pages and the vignettes, are
distributed under a [CC BY-SA
license](https://creativecommons.org/licenses/by-sa/4.0/).


# Installation

To install this package, start R (version "4.3") and enter:

```
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("omXplore")
```

This will also install dependencies.

It is also possible to install `omXplore` from Github:

```
library(devtools)
install_github('edyp-lab/omXplore')

```

For older versions of R, please refer to the appropriate Bioconductor release.

