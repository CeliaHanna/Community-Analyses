##map of the SWIR for intro
install.packages('mapproj')
install.packages('maptools')
install.packages('gdal')

library(ggplot2)
library(maps)
library(mapproj)
library(maptools)
library(sf)
library(gdal)

data("wrld_simpl")
sw_io <- readShapePoly("path/to/sw_io.shp")
