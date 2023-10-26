###SPATIAL ANALYSIS WITH SF AND RASTER IN R
##following slides that can be found at https://s3.amazonaws.com/assets.datacamp.com/production/course_4422/slides/chapter1.pdf
#### https://www.youtube.com/watch?v=a6H9-9tHTF8

###two common kinds of spatial data: raster data and vector data.
##raster data is designed to store geospatial data that is continuous across space 
## is the most common kind of data for eg satellite data
## it is stored in a gridded format
##images are a form of raster data
##images are a grid of pixels, in which each pixel contains a numeric value. So fundamentally images are a matrix of numbers
##these numbers are translated into colours through a key of some kind and this produces the pixel values in larger visual images 

###Packages needed - 'sf' for vectors and 'raster' for grids
library(sf)
library(raster)
library(dplyr)
install.packages('stars')
library(stars)

##### READING SPATIAL DATA IN R ######
### read in raster data using the raster package and either the raster() or brick() functions#
###read in vector data with the sf package and st_read() function 

##single band raster data: have one value per grid cell. 
##use raster() for single band rasters
##multi band rasters: ech grid cell has multiple values, one for each layer 
##use brick() for multiband rasters 


##first set wd to wherever files are. 
setwd('/Users/user/Desktop/Rasterfiles/')
getwd()

##open file using raster 
file <-raster('melvilleclust_terrainfine5_depthcor.tif')

##to tell if the file is a single band or multi band raster
class(file)

##output shows its a single band raster
file

###FUNCTIONS TO EXTRACT PIECES OF THE METADATA###

###extent() gives the minimum and maximum X and Y coordinates of the raster
extent(file)

###ncell() and nlayers() give the total number of grid cells or layers, respectively

ncell(file)
nlayers(file)

##crs() gives the coordinate reference system 

crs(file)


###Creating quick maps of your rasters with plot() and plotRGB()###

plot(file)
plotRGB(file)


raster('MOW_interp_geotiff.tif')
plotRGB('MOW_interp_geotiff.tif')

file2<-raster('Atlantis_25m_v3_interp.tif')
class(file2)
##single band rasters have a class of 'raster layer'
plot(file2)
file2

str(file2)

##raster files can contain discrete features eg habitat type

##terra is a package that came after raster. it is quicker and improved
##https://geocompr.robinlovelace.net/spatial-class.html

###terra provides a specific set of functions to create, read, export, manipulate and process raster files
##terra functions are usually more computationally efficient than raster equivalents
install.packages("terra")
class(file2)
file2
plot(file2)
library(raster)
library(terra)

getwd()
setwd('/Users/user/Desktop/Rasterfiles')
getwd()

file <-raster('melvilleclust_terrainfine5_depthcor.tif')
file
class(file)
plot(file)

install.packages('utils')

