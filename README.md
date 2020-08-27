DisperseR
================
11 Sep 2019

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

<!-- badges: end -->

# PLEASE NOTE:This package as well as this readme are still under development.

# Welcome to disperseR

## Package Authors:

  - **Main author**: Lucas Henneman
  - **Contributor**: Christine Choirat
  - **Contributor**: Maja Garbulinska

## What is disperseR ?

`disperseR` is an R package designed based on the [hyspdisp
package](https://github.com/lhenneman/hyspdisp) and the [SplitR
package](https://rdrr.io/github/rich-iannone/SplitR/). It is very
important to note that many functions in `disperseR` are just sightly
redesigned functions from the two mentioned packages.

`disperseR` runs the [HYSPLIT](https://www.ready.noaa.gov/HYSPLIT.php)
many times and calculates the HYSPLIT Average Dispersion (or HyADS)
exposure metric. The results can then be aggregated to ZIP code level to
create national estimates of exposure from various sources. `disperseR`
includes functions that make it possible for the user to plot the
results easily.

Thanks to the [hyspdisp package](https://github.com/lhenneman/hyspdisp),
for example, plumes from several power plants can be tracked for many
days and cumulative impacts estimated. disperseR laverages [hyspdisp
package](https://github.com/lhenneman/hyspdisp) and allows the user have
a more friendly interaction with the package.

### What is improved?

`disperseR` is a new version of the `hyspdisp` package. What has been
improved?

  - Input data manipulation is handled at the package level. The user
    only has to read the data in using the `disperseR::get_data()`
    function. We show how to do it in the main vignette.

  - We also created additional vignettes should the user want to see how
    the attached data was preprocessed. We show every single step of
    preprocessing starting from the step of data download. This is key
    for reproducible research.

  - Very clear project struture and automatization does not make the
    user lost in the maze of multiple folders. The
    `disperseR::create_dirs()` automatically creates the whole project
    structure either in the specified location or on the desktop. The
    function also assigns path to each folder to the R environment.
    These paths are then used by other `disperseR` functions. **Note**
    that the `disperseR::create_dirs()` function does not overwrite the
    project folders if they already exists in the specified location.

  - Until now the `units` data for different years was separated and
    only four years of data were available with the package. Now data
    for years 1995 to 2015 has been added and aggregated to one data
    file called `units` attached to `disperseR`.

  - ZIP code linkage procedure requires a ZCTA-to-ZIP code crosswalk
    file. These crosswalk data has also been attached to the package. It
    not only provides the crosswalk between ZCTA and ZIP but also
    contains information about population sizes.

  - Before the user could only run analysis for one year. `disperseR`
    allows to process all the needed years together.

  - Graph functions now have many automatic features.

  - Documentation has been much improved. The `?FUNCTION` syntax should
    work to access help files.

### Vignettes attached with the package

We know it is sometimes difficult to start working with a new package,
especially if you are not very familiar with R. We also believe in
reproducible research. This is why we have included several vignettes to
help you with the process.

  - [The main vignette for
    disperseR](https://htmlpreview.github.io/?https://github.com/lhenneman/disperseR/blob/dev/vignettesHTML/Vignette_DisperseR.html)
  - [Load data one by
    one](http://htmlpreview.github.io/?https://github.com/lhenneman/disperseR/blob/master/vignettesHTML/Vignette_Get_Data_One_by_One.html)
  - [Crosswalk data preparation
    (optional)](http://htmlpreview.github.io/?https://github.com/lhenneman/disperseR/blob/master/vignettesHTML/Vignette_Crosswalk_preparation.html)
  - [Planetary layers data preparation
    (optional)](http://htmlpreview.github.io/?https://github.com/lhenneman/disperseR/blob/master/vignettesHTML/Vignette_Planetary_Layers_Data_Preparation.html)
  - [ZIP Code coordinate data preparation
    (optional)](http://htmlpreview.github.io/?https://github.com/lhenneman/disperseR/blob/master/vignettesHTML/Vignette_Zip_Code_Coordinate_Data_Preparation.html)
  - [Vignette ZCTA shapefile preparation
    (optional)](http://htmlpreview.github.io/?https://github.com/lhenneman/disperseR/blob/master/vignettesHTML/Vignette_ZCTA_Shapefile_Preparation.html)
  - [Vignette units preparation
    (optional)](http://htmlpreview.github.io/?https://github.com/lhenneman/disperseR/blob/master/vignettesHTML/Vignette_Units_Preparation.html)

### Data attached with the package

Unfortunatelly, `disperseR` requires a lot of data to run the models. We
could not include all the data sets with the package. For example the
ZCTA shapefile is more than 140 MB. You can access it very simply with
the help of the `disperseR::get_data()` function. Here however are the
data that are attached:

  - **crosswalk**: ZIP code linkage procedure requires a ZCTA-to-ZIP
    code crosswalk file. ZCTAs are not exact geographic matches to ZIP
    codes, and multiple groups compile and maintain Crosswalk files. We
    used the Crosswalk maintained by [UDS
    Mapper](%22https://www.udsmapper.org/zcta-crosswalk.cfm%22) and
    prepossessed it also including information about the population
    size. While not necessary for the HYSPLIT model or processing of its
    outputs, population-weighted exposure metrics allow for direct
    comparisons between power plants. If you would like to know more
    details about how this crosswalk was prepared, we have attached a
    vignette that explains it. You can see it by clicking
    [here](vignettes/Vignette_Crosswalk_Preparation/html).

  - **PP.units.monthly1995\_2017** : The `disperseR` package also
    includes monthly power plant emissions, load, and heat input data.
    (we currently do not have a vignette for these data due to server
    problems of the data owner). This will be updated as soon as
    possible.

  - **units**(data for 1995-2015): This package contains annual
    emissions and stack height data from [EPA’s Air Markets Program
    Data](https://ampd.epa.gov/ampd/) and the [Energy Information
    Agency](https://www.eia.gov/electricity/data/eia860/). Again, if you
    would like to know how these data were prepared please see the
    special vignette that we have attached to this package. You can see
    it by clicking [here](vignettes/Vignette_Units_Preparation.html).

  - **zipcode coordinate data**: The `disperseR` package contains a data
    set with coordinates of ZIP codes. This might be useful for
    plotting, but it is not necessary as it will be used automatically
    by our plotting functions where required. Please click
    [here](vignettes/Vignette_Zip_Code_Coordinate_Data_Preparation.html)
    for more information.

### Example graphical output

`disperseR` has functions that let you plot your results. Here is just
one of many
examples.

<img width="500" alt="Screen Shot 2019-09-11 at 10 51 52" src="https://user-images.githubusercontent.com/43005886/64708191-344fe900-d482-11e9-952e-d21007f8c846.png">

## Instructions

### Download and installation.

First, not having the `Rcpp` package installed on your computer can lead
to problems with `disperseR` installation (problems with version
installation). We recommend you first type the following into your R
console.

``` r
install.packages("Rcpp")
```

\*\*Please note*If you are using a Windows machine and you want R to
render the vignettes for you, you will need to download Rtools from
[here](https://cran.rstudio.com/bin/windows/Rtools/). If you prefer to
avoid this step you can go ahead and proceed with the instalation as we
have added links to access already rendered vignettes on GitHub.*

Continue by typing the following in your R console. This will download
the package from GitHub, install it and build the vignettes. This might
take some
minutes.

``` r
devtools::install_github("lhenneman/disperseR", force = TRUE, build_vignettes = TRUE)
```

Load `disperseR` into your R session.

``` r
library(disperseR)
```

### See the vignettes

You should be able to see the main vignette like this. This will be
opened by your RStudio.

``` r
vignette("Vignette_DisperseR")
```

The rest of the vignettes can be accessed by typing the corresponding
commands.

``` r
vignette("Vignette_Crosswalk_Preparation")
vignette("Vignette_Load_Data_One_by_One")
vignette("Vignette_Units_Preparation")
vignette("Vignette_Zip_Code_Coordinate_Data_Preparation")
vignette("Vignette_Planetary_Layers_Data_Preparation")
vignette("Vignette_ZCTA_Shapefile_Preparation")
```

\*\* NOTE: IF THIS DOES NOT WORK:\*\*

In case this does not work for you. We have rendered all the vignettes
for you and you can access them from your browser by clicking at the
corresponding hyperlinks in **Vignettes attached with the package**
section above.

### Set up the project.

The vignettes will instruct you to do so but you can already start by
creating the project folder. Use `disperseR::create_dirs()` function to
do so. Point `disperseR` to the location where you want your project to
be created. For example the following code will create the project in
the user’s Dropbox. If you do not specify the location and just type
`disperseR::create_dirs()` it will still work and the project will be
created on your desktop.

``` r
disperseR::create_dirs(location = "/Users/username/Dropbox")
```

This will set up is the following folders and paths to them :

  - `main`: the main folder where the project will be located.
      - `input`: the input that we need for calculations.
          - `zcta_500k`: ZCTA (A Zip Code Tabulation Area) shape files
          - `hpbl`: monthly global planetary boundary layer files.
          - `meteo`: (reanalysis) meteorology files
      - `output`
          - `hysplit`: disperseR output (one file for each emissions
            event)
          - `ziplink`: files containing ZIP code linkages
          - `rdata`: RData files containing HyADS source-receptor
            matrices
          - `exp`: exposure per zipcode data
          - `graph`: graphs saved here as pdf when running functions
      - `process`: temporary files that are created when the model is
        running and then deleted

Here is a screen shot of what it should look
like:

<img width="856" alt="Screen Shot 2019-09-01 at 16 43 58" src="https://user-images.githubusercontent.com/43005886/64078186-c2381100-ccd7-11e9-96e4-be8f6ef97875.png">

And these are the variables with paths that will appear in your
environment.

![Screen Shot 2019-09-03 at 19 06
54](https://user-images.githubusercontent.com/43005886/64193883-167cf580-ce7e-11e9-9833-2f1a482d3f58.png)

### Get the data

You can get most of the data required for the analysis by using the
following function. This function will download the data necessary and
for the data that is already attached with the package it will
automatically assign it to variables in your R environment. If you want
to load the data step by step check our vignette
[here](%22https://htmlpreview.github.io/?https://github.com/garbulinskamaja/disperseR/blob/master/vignettesHTML/Vignette_Get_Data_One_by_One.html%22).
It also contains more information about the data and their sources.

The arguments `start.year`, `start.month`,`end.year`, and `end.month`
are necessary to download the meteorology reanalysis files. They will be
downloaded if they are not already in the `meteo_dir` folder. The
reanalysis met files are about 120 MB each.

If you, for example, you want to download files for January-March 2005,
you just have to use the `get_data()` function and set `data = "all"`,
`start.year = "2005"`, `start.month = "01"`, `end.year = "2005"`, and
`end.month = "03"`. See below.

``` r
disperseR::get_data(data = "all", 
  start.year = "2005", 
  start.month = "01", 
  end.year="2005", 
  end.month="03")
```

If it runs correctly you should see the following in our R environment.

![Screen Shot 2019-09-03 at 19 10
34](https://user-images.githubusercontent.com/43005886/64194080-88edd580-ce7e-11e9-95a7-a52f5d5f7677.png)

#### The units data

The units data should be loaded separately so that you are able to
select which units to process.

This package contains annual emissions and stack height data from [EPA’s
Air Markets Program Data](https://ampd.epa.gov/ampd/) and the [Energy
Information Agency](https://www.eia.gov/electricity/data/eia860/) for
years 2003-2012. Again, if you would like to know how these data were
prepared please see the special vignette that we have attached to this
package. Access it
[here](https://htmlpreview.github.io/?https://github.com/garbulinskamaja/disperseR/blob/master/vignettesHTML/Vignette_Units_Preparation.html)

You can visualize the data like this in RStudio:

``` r
view(disperseR::units)
```

**Please note:** If you decide to use a specific unit but for many years
you must have a row of data for each year. For example this is out data
from the main vignette. Look at row 1 and row 3. They contain data for
the same unit but a different
year.

<img width="681" alt="Screen Shot 2019-09-11 at 13 05 52" src="https://user-images.githubusercontent.com/43005886/64718825-f3ad9b00-d494-11e9-951e-020db25d0c8a.png">

### Analysis

We suggest you have a look at our main vignette
[here](vignettes/Vignette_Disperse.html) for details about the analysis.

## Graphical output

Graphical output is authomatically saved to the `graph_dir` by the
plotting functions.

## Packages used in functions and vignettes.

  - base (R Core Team 2019a)
  - data.table (Dowle and Srinivasan 2019)
  - dplyr (Wickham et al. 2019)
  - ggmap (Kahle and Wickham 2013)
  - ggplot2 (Wickham 2016)
  - ggrepel (Slowikowski 2019)
  - ggsn (Santos Baquero 2019)
  - gridExtra (Auguie 2017)
  - lubridate (Grolemund and Wickham 2011)
  - measurements (Birk 2019)
  - ncdf4 (Pierce 2019)
  - parallel (R Core Team 2019b)
  - raster (Hijmans 2019)
  - readxl (Wickham and Bryan 2019)
  - scales (Wickham 2018)
  - sf (Pebesma 2018)
  - sp (Pebesma and Bivand 2005)
  - tidyr (Wickham and Henry 2019)
  - tidyverse (Wickham 2017)
  - viridis (Garnier 2018)

## References / Resources Used

NCEP Reanalysis data provided by the NOAA/OAR/ESRL PSD, Boulder,
Colorado, USA, from their Web site at <https://www.esrl.noaa.gov/psd/>
Mesinger, F., G. DiMego, E. Kalnay, K. Mitchell, P.C. Shafran, W.
Ebisuzaki, D. Jović, J. Woollen, E. Rogers, E.H. Berbery, M.B. Ek, Y.
Fan, R. Grumbine, W. Higgins, H. Li, Y. Lin, G. Manikin, D. Parrish, and
W. Shi, 2006: North American Regional Reanalysis. Bull. Amer. Meteor.
Soc., 87, 343–360, <https://doi.org/10.1175/BAMS-87-3-343>

[hyspdisp package](https://github.com/lhenneman/hyspdisp)

[SplitR package](https://rdrr.io/github/rich-iannone/SplitR/)

<div id="refs" class="references">

<div id="ref-gridExtra">

Auguie, Baptiste. 2017. *GridExtra: Miscellaneous Functions for "Grid"
Graphics*. <https://CRAN.R-project.org/package=gridExtra>.

</div>

<div id="ref-measurements">

Birk, Matthew A. 2019. *Measurements: Tools for Units of Measurement*.
<https://CRAN.R-project.org/package=measurements>.

</div>

<div id="ref-Crosswalk">

“Crosswalk Zip Code to Zcta Crosswalk Table Developed by John Snow, Inc.
(JSI) for Use with Uds Service Area Data. Not an Official Usps or Census
Product.” n.d. <https://www.udsmapper.org/zcta-crosswalk.cfm>.

</div>

<div id="ref-data.table">

Dowle, Matt, and Arun Srinivasan. 2019. *Data.table: Extension of
‘Data.frame‘*. <https://CRAN.R-project.org/package=data.table>.

</div>

<div id="ref-EIA">

“EIA Data.” n.d. <https://www.eia.gov/electricity/data/eia860/>.

</div>

<div id="ref-EPA">

“EPA’s Air Markets Program Data Data.” n.d.
<https://ampd.epa.gov/ampd/>.

</div>

<div id="ref-viridis">

Garnier, Simon. 2018. *Viridis: Default Color Maps from ’Matplotlib’*.
<https://CRAN.R-project.org/package=viridis>.

</div>

<div id="ref-lubridate">

Grolemund, Garrett, and Hadley Wickham. 2011. “Dates and Times Made Easy
with lubridate.” *Journal of Statistical Software* 40 (3): 1–25.
<http://www.jstatsoft.org/v40/i03/>.

</div>

<div id="ref-disperseR">

Henneman, Lucas, Christine Choirat, and Maja Garbulinska. n.d.
*DisperseR: Run Hysplit Many Times in Parallel, Aggregate to Zip Code
Level, Plot the Results, Save the Plots.*
<https://github.com/garbulinskamaja/disperseR>.

</div>

<div id="ref-raster">

Hijmans, Robert J. 2019. *Raster: Geographic Data Analysis and
Modeling*. <https://CRAN.R-project.org/package=raster>.

</div>

<div id="ref-ggmap">

Kahle, David, and Hadley Wickham. 2013. “Ggmap: Spatial Visualization
with Ggplot2.” *The R Journal* 5 (1): 144–61.
<https://journal.r-project.org/archive/2013-1/kahle-wickham.pdf>.

</div>

<div id="ref-sf">

Pebesma, Edzer. 2018. “Simple Features for R: Standardized Support for
Spatial Vector Data.” *The R Journal* 10 (1): 439–46.
<https://doi.org/10.32614/RJ-2018-009>.

</div>

<div id="ref-sp">

Pebesma, Edzer J., and Roger S. Bivand. 2005. “Classes and Methods for
Spatial Data in R.” *R News* 5 (2): 9–13.
<https://CRAN.R-project.org/doc/Rnews/>.

</div>

<div id="ref-ncdf4">

Pierce, David. 2019. *Ncdf4: Interface to Unidata netCDF (Version 4 or
Earlier) Format Data Files*. <https://CRAN.R-project.org/package=ncdf4>.

</div>

<div id="ref-base">

R Core Team. 2019a. *R: A Language and Environment for Statistical
Computing*. Vienna, Austria: R Foundation for Statistical Computing.
<https://www.R-project.org/>.

</div>

<div id="ref-parallel">

———. 2019b. *R: A Language and Environment for Statistical Computing*.
Vienna, Austria: R Foundation for Statistical Computing.
<https://www.R-project.org/>.

</div>

<div id="ref-ggsn">

Santos Baquero, Oswaldo. 2019. *Ggsn: North Symbols and Scale Bars for
Maps Created with ’Ggplot2’ or ’Ggmap’*.
<https://CRAN.R-project.org/package=ggsn>.

</div>

<div id="ref-ggrepel">

Slowikowski, Kamil. 2019. *Ggrepel: Automatically Position
Non-Overlapping Text Labels with ’Ggplot2’*.
<https://CRAN.R-project.org/package=ggrepel>.

</div>

<div id="ref-zcta">

“United States Census Bureau Zcta Shape Files.” n.d.
<http://www2.census.gov/geo/tiger/GENZ2017/shp/cb_2017_us_zcta510_500k.zip>.

</div>

<div id="ref-ggplot2">

Wickham, Hadley. 2016. *Ggplot2: Elegant Graphics for Data Analysis*.
Springer-Verlag New York. <https://ggplot2.tidyverse.org>.

</div>

<div id="ref-tidyverse">

———. 2017. *Tidyverse: Easily Install and Load the ’Tidyverse’*.
<https://CRAN.R-project.org/package=tidyverse>.

</div>

<div id="ref-scales">

———. 2018. *Scales: Scale Functions for Visualization*.
<https://CRAN.R-project.org/package=scales>.

</div>

<div id="ref-readxl">

Wickham, Hadley, and Jennifer Bryan. 2019. *Readxl: Read Excel Files*.
<https://CRAN.R-project.org/package=readxl>.

</div>

<div id="ref-dplyr">

Wickham, Hadley, Romain François, Lionel Henry, and Kirill Müller. 2019.
*Dplyr: A Grammar of Data Manipulation*.
<https://CRAN.R-project.org/package=dplyr>.

</div>

<div id="ref-tidyr">

Wickham, Hadley, and Lionel Henry. 2019. *Tidyr: Easily Tidy Data with
’Spread()’ and ’Gather()’ Functions*.
<https://CRAN.R-project.org/package=tidyr>.

</div>

<div id="ref-opendatasoft">

“ZIP code latitude and longitude Public Open Datasoft.” n.d.
['https://public.opendatasoft.com/explore/dataset/us-zip-code-latitude-and-longitude/download/?format=csv\&timezone=Europe/Berlin\&use\_labels\_for\_header=true']('https://public.opendatasoft.com/explore/dataset/us-zip-code-latitude-and-longitude/download/?format=csv&timezone=Europe/Berlin&use_labels_for_header=true').

</div>

</div>
