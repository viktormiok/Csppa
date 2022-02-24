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

Machine learning approach for analysis and visualisation of astrocyte spatial point patterns from the medial arcuate nucleus of the hypothalamus under hypercaloric diet shifts. Repository comprise R functions for analysis and visualisation together with a jupyter notebook of the whole analysis of Gfap and Aldh1l1 astrocytes spatial point patterns from mouse under standard chow, 5 and 15 days high fat high sugar diet.

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
Data required for cellular spatial point pattern analysis will be deposited on line soon:
| Data type     | GEO number |
| ------------- | ------------- |
| Aldh1l1 only  | Not available yet |
| Gfap only      | Not available yet |
| Double positive      | Not available yet |



## Tutorials

Please see the following tutorials for detailed examples of how to use __`Csppa`__: 

### Csppa walkthrough:
* [Notebook](https://github.com/viktormiok/Csppa/blob/main/notebooks/asppa_code.ipynb)

## License

__`Csppa`__ is distributed under the MIT license. Please read the license before using __`Csppa`__, which it is distributed in the `LICENSE` file.

## References

Publications related to __`Csppa`__ include:

- Lutomska, L.M., Miok, V., Krahmer, N., González García, I., Gruber, T., Le Thuc, O., De Bernardis Murat, C., Legutko, B., Sterr, M., Saher, G., Lickert, H., Ussar, S., Tschöp, M., Lutter, D., García-Cáceres, C.(2022), "Hypercaloric diet selectively triggers a transient molecular rearrangement of astrocytes in the arcuate nucleus", *Submitted.

Please cite the relevant publications if you use __`Csppa`__.
