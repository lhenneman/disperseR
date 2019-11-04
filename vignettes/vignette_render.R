library(rmarkdown)

output_dir <- "~/Dropbox/Rpackages/disperseR/vignettesHTML"

render("./vignettes/Vignette_Crosswalk_Preparation.Rmd", output_dir = output_dir)
render("./vignettes/Vignette_DisperseR.Rmd", output_dir = output_dir)
render("./vignettes/Vignette_Get_Data_One_by_One.Rmd", output_dir = output_dir)
render("./vignettes/Vignette_Planetary_Layers_Data_Preparation.Rmd", output_dir = output_dir)
render("./vignettes/Vignette_Units_Preparation.Rmd", output_dir = output_dir)
render("./vignettes/Vignette_ZCTA_Shapefile_Preparation.Rmd", output_dir = output_dir)
render("./vignettes/Vignette_Zip_Code_Coordinate_Data_Preparation.Rmd", output_dir = output_dir)
