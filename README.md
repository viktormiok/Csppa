<img src="https://github.com/viktormiok/viktormiok.wordpress.com/blob/main/software/csppa.png" align="right" height="200" width="200">

![](https://img.shields.io/badge/language-R-orange.svg) ![version](https://img.shields.io/badge/GiHub_version-1.1.0-519dd9) ![GitHub code size in bytes](https://img.shields.io/github/languages/code-size/viktormiok/Csppa) ![GitHub issues](https://img.shields.io/github/issues/viktormiok/Csppa)

![dependencies](https://img.shields.io/badge/dependencies-up%20to%20date-orange)  	![commit](https://img.shields.io/github/last-commit/viktormiok/Csppa) ![GitHub](https://img.shields.io/github/license/viktormiok/Csppa)

[![Edit with Gitpod](https://gitpod.io/button/open-in-gitpod.svg)](https://gitpod.io/#https://github.com/viktormiok/Csppa) 



# Csppa

- [Overview](#overview)
  * [Application](#application)
- [Installation](#installation)
- [Data](#data)
- [Tutorials](#tutorials)
- [License](#license)
- [References](#references)

## Overview

<img src="https://github.com/viktormiok/Csppa/blob/main/Figures/RandomForestClassifier.gif" align="right" height="540" width="430">

Understanding how the cells' anatomical location and spatial molecular distribution determine the cellular response to a high-caloric diet requires developing machine-learning methods for analysis and visualization.

The R-package __`Csppa`__ employs machine learning for cellular spatial point pattern analysis and visualization. Hence, __`Csppa`__ enables a comprehensive characterization of how specific cellular gene expression patterns evolve spatially and temporally during different stages of adaptation to a high‑caloric diet.

### Application

Here, we focus on astrocytes in the arcuate nucleus of the mouse brain, examining the expression of the Gfap and Aldh1l1 genes and their spatial distributions under standard chow (SC), as well as 5‑day and 15‑day high-fat high-sugar (HFHS) diet conditions. The R package __`Csppa`__ is used to assess whether these astrocyte populations are spatially organised and whether they form local clusters in response to the HFHS diet over time.

To address this, the method quantifies the spatial coherence of each astrocyte subtype across the experimental conditions (SC, 5‑day HFHS, and 15‑day HFHS). Spatial coherence—representing similarity among neighbouring cells—is measured using Moran’s I spatial autocorrelation coefficient, a well-established metric of spatial dependence. In addition, a random forest classifier is applied to characterise how astrocytes expressing Gfap and Aldh1l1 are distributed within the shared feature space for each experimental group.

## Installation

The package __`Csppa`__ depends on [__`spatstat`__](https://github.com/markvdwiel/ShrinkBayes) and on [R >= 3.0.0](https://cran.r-project.org/) and is available from GitHub. This requires the package [__`devtools`__](https://cran.r-project.org/web/packages/devtools/index.html):

``` r
devtools::install_github("viktormiok/Csppa", build_vignettes=TRUE)
```

Please restart R before loading the package and its documentation:

``` r
library(Csppa)
utils::help(Csppa)
utils::vignette("Csppa")
```

## Data
Data required for cellular spatial point pattern analysis will be deposited online soon:
| Data type     | Data link |
| ------------- | ------------- |
| Aldh1l1 only  | [link](https://github.com/viktormiok/AstrocytesHeterogenityARC/blob/main/SPP_data_all.csv) |
| Gfap only      | [link](https://github.com/viktormiok/AstrocytesHeterogenityARC/blob/main/SPP_data_all.csv) |
| Double positive      | [link](https://github.com/viktormiok/AstrocytesHeterogenityARC/blob/main/SPP_data_all.csv) |



## Tutorials

Please see the following tutorials for detailed examples of how to use __`Csppa`__: 

### Csppa walkthrough:
* [Notebook](https://github.com/viktormiok/Csppa/blob/main/notebooks/asppa_code.ipynb)

## License

__`Csppa`__ is distributed under the MIT license. Please read the license, which is distributed in the `LICENSE` file, before using it.

## References

Publications related to __`Csppa`__ include:

- Lutomska, L.M., **Miok, V.**, Krahmer, N., González García, I., Gruber, T., Le Thuc, O., De Bernardis Murat, C., Legutko, B., Sterr, M., Saher, G., Lickert, H., Ussar, S., Tschöp, M., Lutter, D., García-Cáceres, C. (2022), [Diet triggers specific responses of hypothalamic astrocytes in time and region dependent manner](https://onlinelibrary.wiley.com/doi/full/10.1002/glia.24237), *Glia 70 (10), 1795-2008*.
  
- Lutomska, L.M., **Miok, V.**, Krahmer, N., González García, I., Gruber, T., Le Thuc, O., De Bernardis Murat, C., Legutko, B., Sterr, M., Saher, G., Lickert, H., Ussar, S., Tschöp, M., Lutter, D., García-Cáceres, C.(2021), [Hypercaloric diet selectively triggers a transient molecular rearrangement of astrocytes in the arcuate nucleus]( https://www.biorxiv.org/content/10.1101/2022.03.30.486358v1.abstract), bioRxiv, 4(1).

Please cite the relevant publications if you use __`Csppa`__.
