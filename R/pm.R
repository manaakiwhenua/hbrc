#' Modelling of particulate matter air pollution removed by tree cover
#'
#' This function models removal of particulate matter air pollution by tree leaves over 24 hours.
#' @param lcm Land cover map including vegetation categories.
#' @param lai Leaf Area Index map.
#' @param pm10 PM 10 concentration in air, (μg/ m3)
#' @param ltb Lookup table as matrix with two columns. First column must include all categories of land cover included in lcm. Second column indicates land covers that contribute to tree canopy cover, indicated by a 1.
#' @param Vd Deposition velocity (m/s). Should already account for resuspension rate.
#' @return A rast object with two layers. First layer is pm removal per m2. Second layer is actual pm removal per grid cell. Units are kg per day.
#' @export

#ltb<- cbind( eslookup[,2], eslookup[,12])
# Write function
hbrc.pm10 <- function(lcm,
                        lai,
                      pm10 = 22,
                      Vd = 0.0064,
                      #Rd = 0.5,
                       ltb){

 # Model for one day
# micrograms/ m2/ day
  # convert lai to pm removal per m2 of land area
  pmremoval.per.m2 <- ((Vd /1000000) * (pm10/1e-6) ) *# Covert Vd into m/s and pm10 in grams per m2
    lai *  (24*60 * 60)  # units are in micrograms, so convert to kg
  pmremoval.per.m2  <- pmremoval.per.m2 *1e-9#converted to kg

  # Test based on Cavanagh et al data if setting LAI to 6.4
  #(pmremoval.per.m2 *431 *1000000*0.08*365)*0.001

# Convert to removal per pixel
 pmremoval<- (pmremoval.per.m2  *terra::cellSize(pmremoval.per.m2 , unit="m"))

  # Return output
pmremoval <- c(pmremoval.per.m2 , pmremoval)
names(pmremoval)<-c("pmremoval.per.m2" , "pmremoval")
return(pmremoval)

}
