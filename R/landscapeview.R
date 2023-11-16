#' Modelling of landscape view attractiveness
#'
#' This function models relative landscape attractiveness using a viewshed.
#' @param lcm Land cover map including vegetation categories.
#' @param chm Canopy height model including vegetation height above the ground.
#' @param dsm Digital surface model.
#' @param observerh Height of observer above the ground in same units as chm and dsm.
#' @param resc Rescaling factor for reducing resolution. Integer or NA if no aggregation required.
#' @param subs Number of regularly-spaced locations to calculate viewshed from. 120 takes approximately 10 minutes on a normal laptop, so be careful.
#' @param ltb Lookup table as matrix with two columns. First column must include all categories of land cover included in lcm. Second column indicates land covers that contribute to landscape attractiveness, indicated by a 1.
#' @return A SpatRaster stack including three layers; lascore = landscape aesthetics score within the viewshed, vsarea = viewshed area, combinedscore = multiplicative landscape aesthetic score.
#' @export

# Write function
#ltb<-hb.eslookup[,c(2,15)]
hbrc.aesthetic <- function(lcm,
                   chm,
                   dsm,
                   observerh = 1.6,
                   resc=NA,
                   subs = 120,
                   ltb){

  if(!is.na(resc)){
    lcm<- aggregate(lcm, resc, fun="modal")
    chm<- aggregate(chm, resc)
    dsm<- aggregate(dsm, resc)
  }else{

  }


# Create grid of points
lpts<- terra::spatSample(dsm,subs, "regular", na.rm=TRUE, as.df=TRUE,xy=TRUE)
lpts<-lpts[,1:2]
# Exclude any not on lcm

# Add zeroes to canopy height to replace na
chm2<-chm
chm2[is.na(chm2)]<-0

# reclassify lcm by aesthetic value
laes<- terra::classify(lcm, ltb)

# Make a place to store values
lpts$lascore <-NA
lpts$vsarea<- NA

# Distance weighting is according to Schirpke et al 2013
schirpke <- data.frame(zone =c("near", "mid","far","outside"),
                       mindist=c(0,1.5,10,50),
                       maxdist =c(1.5, 10, 50,Inf),
                       weighting =c(0.48, 0.32, 0.2,0))

for(j in 1:dim(lpts)[1]){

  # If in tall canopy, can only see within 30 m
  if(terra::extract(chm2, lpts[j,1:2])[1,2]> observerh){
    vX<- terra::rasterize(x=
                     terra::buffer(terra::vect(lpts[j,1:2],
                          geom=names(lpts[,1:2]),
                          crs=terra::crs(dsm)),30),
                   y=dsm,
                   values=1,
                   background=0)
  }else{
# loop viewsheds
vX<- terra::viewshed(dsm, as.matrix(lpts[j,1:2]),
                     observer=
                       (observerh-terra::extract(chm2, lpts[j,1:2])[1,2]))
  }# We lower the observer height in areas of canopy, by the height of the canopy

  # Distance
  dX<- terra::distance(vX,
                       terra::vect(lpts[j,1:2],
                            geom=names(lpts[,1:2]),
                            crs=terra::crs(dsm)),
                            unit="km" )

  # Classify and add weighting factors
dX<- terra::classify(dX,schirpke[,c(2,3,4)])

# mask by viewable
dX<- terra::mask(dX, vX,maskvalues=FALSE)

# Multiply by landscape attractiveness scores
dX<- dX*laes
lpts$lascore[j]<-terra::global(dX, mean,na.rm=TRUE)
# Find area of viewshed
dX<-terra::cellSize(dX,unit="ha")*!is.na(dX)
lpts$vsarea[j]<-terra::global(dX, sum,na.rm=TRUE)

}
lpts$lascore<-unlist(lpts$lascore)
lpts$vsarea<-unlist(lpts$vsarea)
# Multiplicative scaling
lpts$combinedscore<- lpts$lascore*lpts$vsarea

# Interpolate values for eq_pt1
lascore<- terra::interpNear(chm, as.matrix(lpts[,c(1,2,3)]), 1, interpolate=TRUE)
vsarea<- terra::interpNear(chm, as.matrix(lpts[,c(1,2,4)]), 1, interpolate=TRUE)
combinedscore<- terra::interpNear(chm, as.matrix(lpts[,c(1,2,5)]), 1, interpolate=TRUE)


# # Make a raster using voronoi
# lpts2<- terra::vect(lpts,geom=names(lpts[,1:2]),
#              crs=terra::crs(dsm))
# laras<-terra::voronoi(lpts2, bnd = terra::as.polygons(terra::ext(dsm)))
#
# # Rasterize each value of v1
# nams <- names(lpts2)
#
# laras2<- lapply(nams, function(x) {
#   terra::rasterize(laras, dsm,
#             field = x,
#             touches = TRUE
#   )
# })
# Merge (bind) all objects
laras2 <- c(lascore, vsarea, combinedscore)
names(laras2)<-c("lascore", "vsarea", "combinedscore")

  # Return output
return(laras2)
}
