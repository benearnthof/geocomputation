# packages to be loaded
packages <- c(
  "tidyverse", "broom", "sf", "geosphere", "numbers", "raster", "dplyr", "spData", "remotes",
  "RQGIS", "mlr", "vegan", "mapview", "tmap", "leaflet", "ggplot2", "shiny",
  "scales", "zoo", "rbenchmark", "tmaptools", "shinyjs",
  "mapdeck", "RSAGA", "rgrass7", "link2GI", "osmdata", "parallelMap", "rasterVis",
  "arm", "latticeExtra", "grid", "tree", "ranger", "pROC", "maptools",
  "randomForest", "doParallel", "kernlab", "shinythemes", "hrbrthemes"
)
# checking which packages have been installed
packs <- lapply(packages, FUN = function(packages) {
  do.call("require", list(packages))
})
packs <- unlist(packs, use.names = F)
# generating list of packages yet to be installed
instpacks <- packages[!packs]

# installing all packages that have not yet been installed
lapply(instpacks, FUN = function(instpacks) {
  do.call("install.packages", list(instpacks))
})

# spDataLarge is not on CRAN installing from github:
# install.packages("spDataLarge", repos = "https://nowosad.github.io/drat/", type = "source")
# rqgis3 is needed to work with qgis3 and above
# devtools::install_github("jannes-m/RQGIS3")
# should return a vector of TRUE entries - one entry for every successfully loaded package
check <- unlist(lapply(packages, FUN = function(packages) {
  do.call("require", list(packages))
}))

failed <- which(check == FALSE)
failed <- packages[failed]

if (identical(character(0), failed)) {
  print("All packages loaded successfully.")
} else {
  cat("Packages", "\n", failed, "\n", "could not be loaded.")
}

# if this returns TRUE then the installation of all packages was successful

# package tree needs R version 3.6 or higher
# if(!require(installr)) {
# if(!require(installr)) {
#   install.packages("installr"); require(installr)
#   }
# updateR()

# remotes::install_github("geocompr/geocompkg")
