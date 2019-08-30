
# This package as well as this readme is under development. 

# What is disperseR?


disperseR is a packaged designed based on the hyspdisp package and the SplitR package. It is an R package that runs HYSPLIT many times and calculates the HYSPLIT Average Dispersion (or HyADS) exposure metric. The results can then be aggregated to ZIP code level to create national estimates of exposure from various sources. For example, plumes from several power plants can be tracked for many days and cumulative impacts estimated. disperseR is a new version of hyspdisp. It makes it easier for the user to interact with the functions and computations. disperseR uses some code borrowed from the SplitR package in a slightly modified version.


https://github.com/lhenneman/hyspdisp https://rdrr.io/github/rich-iannone/SplitR/

# This package as well as this readme is under development. 

# What is improved? 

disperseR is a new version of the hispdisp package. What has been improved?

- All the data data manipulation that had to be done by the user in order to prepare these data for calculations has been handled at the package level. The user has to read the data in. We show how to do it in the main vignette. We also created additional vignettes should the user want to see how the attached data was preprocessed. We show every single step of preprocessing starting from the step of data download. This is key for reproducible research. 

- Very clear project struture and automatization does not make the user lost in the maze of multiple folders. 

- Until now units data for different years was separated. One could only do an analysis for one year. disperseR allows to process all the needed years together. 

- Graph functions are automatized. For example titles are automatic but they can be changed by the user. 

- Data download been handled at the package level. The user does not have to go through all the data download and manipulation. 

# Data attached with the package 
- crosswalk : attached vignette 
- PP.units.monthly1995_2017.RData server problems
- units2003 - units2009 : attached vignette 
- zipcode coordinate data: attached vignette 

# disperseR
New version of the hyspdisp package
https://github.com/lhenneman/hyspdisp
>>>>>>> c8bd5805bc89089ce8e0725869e9cde979add1ae


# Resources used
NCEP Reanalysis data provided by the NOAA/OAR/ESRL PSD, Boulder, Colorado, USA, from their Web site at https://www.esrl.noaa.gov/psd/

Citation: Mesinger, F., G. DiMego, E. Kalnay, K. Mitchell, P.C. Shafran, W. Ebisuzaki, D. Jović, J. Woollen, E. Rogers, E.H. Berbery, M.B. Ek, Y. Fan, R. Grumbine, W. Higgins, H. Li, Y. Lin, G. Manikin, D. Parrish, and W. Shi, 2006: North American Regional Reanalysis. Bull. Amer. Meteor. Soc., 87, 343–360, https://doi.org/10.1175/BAMS-87-3-343
