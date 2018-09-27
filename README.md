# Pcclust

## Overview

Pcclust is a versatile and comprehensive R package for PCA analysis and visualization, specifically designed to tailor PC selection for optimizing downstream cluster analysis.

The analysis pipeline employs 3 metrics to quantify the cluster quality
that is obtained when using different combinations of PCs.
Using the [R mclust package](https://cran.r-project.org/web/packages/mclust/index.html)
it calculates Bayesian information criterion (BIC) and
maximum log likelihood for Gaussian mixture models (GMM), as well as
adjusted rand index in order to determine the subset of PCs containing
the most significant cluster structure.

The novel visualization component of the pipeline then produces an
informative PCA plot with added functionality enabling one to "zoom-in"
on a specific point, generating a supporting graphic detailing the
variance contribution for the complete set of PCs.

## Dependencies
* R packages: mclust, ggplot2, rio

## General usage

Pcclust will accept any numeric multidimensional matrix or dataframe as
input to the analysis pipeline; optionally accepting an appropriate file.

## Install

````
library(devtools)
install_github("audrina/pcclust")
library(pcclust)
````

