
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- badges: end -->
PLEASE NOTE:This package as well as this readme are still under development.
============================================================================

Welcome to disperseR
====================

What is disperseR?
------------------

`disperseR` is an R package designed based on the [hyspdisp package](https://github.com/lhenneman/hyspdisp) and the [SplitR package](https://rdrr.io/github/rich-iannone/SplitR/). It is very important to note that many functions in `disperseR` are just sightly redesigned functions from the two mentioned packages.

It runs the [HYSPLIT](https://www.ready.noaa.gov/HYSPLIT.php) many times and calculates the HYSPLIT Average Dispersion (or HyADS) exposure metric The results can then be aggregated to ZIP code level to create national estimates of exposure from various sources.

Thanks to the [hyspdisp package](https://github.com/lhenneman/hyspdisp), for example, plumes from several power plants can be tracked for many days and cumulative impacts estimated. disperseR laverages [hyspdisp package](https://github.com/lhenneman/hyspdisp) and allows the user have a more friendly interaction with the package.

### What is improved?

`disperseR` is a new version of the hispdisp package. What has been improved?

-   Input data manipulation is handled at the package level. The user only has to read the data in using the `disperseR::get_data()` function. We show how to do it in the main vignette.

-   We also created additional vignettes should the user want to see how the attached data was preprocessed. We show every single step of preprocessing starting from the step of data download. This is key for reproducible research.

-   Very clear project struture and automatization does not make the user lost in the maze of multiple folders. The `disperseR::create_dirs()` automatically creates the whole project structure either in the specified location or on the desktop. The function also assigns path to each folder to the R environment. These paths are then used by other `disperseR` functions. **Note** that the `disperseR::create_dirs()` function does not overwrite the project folders if they already exists in the specified location.

-   Until now the `units` data for different years was separated and only four years of data were available with the package. Now data for years 1995 to 2015 has been added and aggregated to one data file called `units` attached to `disperseR`.

-   ZIP code linkage procedure requires a ZCTA-to-ZIP code crosswalk file. These crosswalk data has also been attached to the package. It not only provides the crosswalk between ZCTA and ZIP but also contains information about population sizes.

-   Before the user could only run analysis for one year. `disperseR` allows to process all the needed years together.

-   Graph functions now have many automatic features.

### Vignettes attached with the package

### Data attached with the package

-   crosswalk
-   PP.units.monthly1995\_2017.RData (we currently do not have a vignette for these data due to server problems of the data owner)
-   units1995 - units2015
-   zipcode coordinate data

Instructions
------------

Start by typing the following in your R console.

``` r
devtools::install_github("garbulinskamaja/disperseR", force = TRUE, build_vignettes = TRUE)
library(disperseR)
```

References / Resources Used
---------------------------

-   NCEP Reanalysis data provided by the NOAA/OAR/ESRL PSD, Boulder, Colorado, USA, from their Web site at <https://www.esrl.noaa.gov/psd/>

-   

Citation: Mesinger, F., G. DiMego, E. Kalnay, K. Mitchell, P.C. Shafran, W. Ebisuzaki, D. Jović, J. Woollen, E. Rogers, E.H. Berbery, M.B. Ek, Y. Fan, R. Grumbine, W. Higgins, H. Li, Y. Lin, G. Manikin, D. Parrish, and W. Shi, 2006: North American Regional Reanalysis. Bull. Amer. Meteor. Soc., 87, 343–360, <https://doi.org/10.1175/BAMS-87-3-343> )
