# Pcclust

## Overview

Pcclust is a versatile and comprehensive R package for PCA analysis and visualization, specifically designed to tailor PC selection for optimizing downstream cluster analysis.

The analysis pipeline employs 2 metrics to quantify the cluster quality
that is obtained when using different combinations of PCs.
Using the [R mclust package](https://cran.r-project.org/web/packages/mclust/index.html)
it calculates Bayesian information criterion (BIC) and
maximum log likelihood for Gaussian mixture models (GMM)
in order to determine the subset of PCs containing
the most significant cluster structure.

The comprehensive visualization component of the pipeline then produces 
several informative PCA plots using the optimal choice of PCs for clustering:
* Baseline PCA visualizations including standard 2-component PCA plot, density plot, and variance contribution pie chart.
* Includes additional functionality enabling one to "zoom-in" on a specific point, generating a supporting graphic detailing the PC contribution for the complete set of PCs.
** sample-specific metadata including standard 2-component PCA plot labeled with query, a diverging lollipop chart showing the PC breakdown for the query, and an interactive PCA plot showing the location of the query in 3D space.

## Dependencies
* R packages: mclust, ggplot2, rio, plotrix, rgl, car

## General usage

Pcclust will accept any numeric multidimensional matrix or data frame as
input to the analysis pipeline; optionally accepting an appropriate file.

## Install

````
library(devtools)
install_github("audrina/pcclust")
library(pcclust)
````

