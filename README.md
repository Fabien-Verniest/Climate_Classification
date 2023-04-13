# Climate_Classification
An R function that generates a Köppen-Geiger climate classification in a raster format from rasters of monthly mean temperature and precipitations.

## Input
Monthly mean temperature (in Degrees Celsius) and precipitations (in millimetres), provided in a raster format. 

## Arguments
- Temp: a list of the 12 rasters of temperature (ranked from January to December)
- Prec: a list of the 12 rasters of precipitations (ranked from January to December)
- level: 1, 2 or 3 (3 by default). Level of Köppen-Geiger climate classification

## Output


## Version of the Köppen-Geiger classification
This function uses the Köppen-Geiger classification provided by Peel et al. (2007) : https://hess.copernicus.org/articles/11/1633/2007/
