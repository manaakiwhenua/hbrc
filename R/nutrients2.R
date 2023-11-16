#' Modelling of nutrient retention using InVEST
#'
#' This function models runoff retained by all land covers and additionally separates the contribution of ecosystems.
#' @param lcm Land cover map including vegetation categories.
#' @param dem Digital elevation model.
#' @param runoff Runoff potential in mm as a SpatRaster. Can be created using the hbrc.runoff.cn function.
#' @param bpt Table of parameters for InVEST, with specific names (see example).
#' @param resc Rescaling factor for reducing resolution. Integer or NA if no aggregation required.
#' @param subs Number of regularly-spaced locations to calculate viewshed from.
#' @param ltb Lookup table with two columns. First column must include all categories of land cover included in lcm. Second is binary indicator for whether the land cover is water.
#' @return A SpatRaster stack including one layer indicating the proportion of nutrient load exported.
#' @export
#' @examples
#' # Example of creating the bpt variable from information in data(eslookup)
#'  bpt<-data.frame(
#'  LULC_desc = eslookup$name,
#'  lucode = eslookup$lcdb,
#'  load_n= eslookup$n.load,
#'  eff_n= eslookup$n.retention,
#'  crit_len_n= 25,
#'  proportion_subsurface_n= 0)

# bpt<-data.frame(
#   LULC_desc = hb.eslookup$name,
#   lucode = hb.eslookup$lcdb,
#   load_n= hb.eslookup$n.load,
#   eff_n= hb.eslookup$n.retention,
#   crit_len_n= 25,
#   proportion_subsurface_n= 0)
# precip<-67
# ltb<-cbind(hb.eslookup$lcdb,
#            0)
# ltb[11:13,2]<-1

hbrc.nutrient <- function(lcm,
                        dem,
                        runoff,
                        resc=NA,
                        subs=120,
                        ltb,
                        bpt){


  # rescaling
  if(!is.na(resc)){
    lcm<- aggregate(lcm, resc, fun="modal")
    dem<- aggregate(dem, resc)
    runoff<- aggregate(runoff, resc)
    }else{

  }


# Based on Invest Model
  # http://releases.naturalcapitalproject.org/invest-userguide/latest/en/ndr.html

  # Upstream and downstream area
  # Write slope and dem to working directory temporary
  tmpdir <- tempdir()
  terra::writeRaster(dem, paste(tmpdir, "dem.tif",
                                  sep = ""), overwrite = T)
 slo<- terra::terrain(dem, v="slope", unit="degrees")
 terra::writeRaster(slo, paste(tmpdir, "slope.tif",
                               sep = ""), overwrite = T)


  whitebox::wbt_fill_depressions(dem = paste(tmpdir, "dem.tif",
                                             sep = ""),
                                 output = paste(tmpdir, "fill_dem.tif",
                                                                       sep = ""))

# Upslope area
  whitebox::wbt_max_upslope_flowpath_length(dem = paste(tmpdir, "fill_dem.tif",
                                  sep = ""), output = paste(tmpdir, "out_mus.tif",
                                                            sep = ""))
# Slope in the upslope zone
  whitebox::wbt_max_upslope_value(dem = paste(tmpdir, "fill_dem.tif",
                                                        sep = ""),
                                  values =paste(tmpdir, "slope.tif",
                                                sep = ""),
                                  output = paste(tmpdir, "out_slopeat_mus.tif",
                                                                                  sep = ""))


# Read back in from whitebox
  musa <- terra::rast(paste(tmpdir, "out_mus.tif",
                             sep = ""))
  slope_at_musa <- terra::rast(paste(tmpdir, "out_slopeat_mus.tif",
                            sep = ""))

  # plot(terra::rast(paste(tmpdir, "fill_dem.tif",
  #                   sep = "")))


  # Downslope distance to stream. We will assume is from water bodies
 water<-  terra::classify(lcm, ltb)
# water[water==0]<-NA
  waterd<- terra::gridDist(water,target=1)


  # loads etc
  load_n <- terra::classify(lcm, cbind(bpt$lucode,bpt$load_n))
  eff_n <- terra::classify(lcm, cbind(bpt$lucode,bpt$eff_n))
  crit_n <- terra::classify(lcm, cbind(bpt$lucode,bpt$crit_len_n))
  prop_n <- terra::classify(lcm, cbind(bpt$lucode,bpt$proportion_subsurface_n))

  # Surface o
  # Remove zeros
  slo[slo<0.005]<-0.005
  slope_at_musa[slope_at_musa<0.005]<-0.005
  musa[musa==0]<-terra::cellSize(musa)[musa==0]

  # calculations
dup<- slope_at_musa *musa
ddown<- waterd/ slo
IC<- log10(dup/ddown)


IC0 <- terra::values(IC,na.rm=TRUE)
IC0[is.infinite(IC0)]<-NA
IC0<- range(IC0,na.rm=TRUE)
IC0<- sum(IC0)/2

k<-2
eq_pt2<- (1+exp((IC0-IC)/k))^-1


# Slope calculation
# Create grid of points
lpts<- terra::spatSample(eq_pt2,subs, "regular", na.rm=TRUE, as.df=TRUE,xy=TRUE,cells=TRUE)
names(lpts)[4]<-"eq_pt2"
lpts$eq_pt1<-NA


# Make D8 pointer file
whitebox::wbt_d8_pointer(dem = paste(tmpdir, "fill_dem.tif",
                                            sep = ""),

                                output = paste(tmpdir, "out_d8.tif",
                                               sep = ""))
# For Loop
for(j in 1:length(lpts[,1])){

lpti<- terra::vect(as.matrix(lpts[j,2:3]),crs=terra::crs(dem), type="points")
  terra::writeVector(lpti, paste(tmpdir, "lpti.shp",
                                sep = ""), overwrite = T)

  whitebox::wbt_trace_downslope_flowpaths(seed_pts=paste(tmpdir, "lpti.shp",
                                                         sep = ""),
    d8_pntr = paste(tmpdir, "out_d8.tif",
                                       sep = ""),
                           output = paste(tmpdir, "lpti_trace.tif",
                                          sep = ""))

  lpti_trace<- terra::rast(paste(tmpdir, "lpti_trace.tif",
                    sep = ""))
  #plot(terra::aggregate(lpti_trace,10,"max",na.rm=TRUE))

  # Extract pixels of eff_n
  lpti_eff_n<- eff_n * lpti_trace
  lpti_eff_n<-values(c(lpti_eff_n, lcm,
                       (sqrt(terra::cellSize(lcm))*sqrt(2)),
                       crit_n
                       ),na.rm=TRUE)
  lpti_eff_n<-as.data.frame(lpti_eff_n)
  names(lpti_eff_n)<-c("eff","luc", "length","crit")

  # We have to make some assumptions, so order by land cover type
  lpti_eff_n<-lpti_eff_n[order(lpti_eff_n$luc),]
  lpti_eff_n$cumlength<-NA
  lpti_eff_n$cumlength[1]<-0
for(y in 2:length(lpti_eff_n[,1])){
  if(lpti_eff_n$luc[y] ==lpti_eff_n$luc[(y-1)] ){
  lpti_eff_n$cumlength [y] <-lpti_eff_n$length [y] +lpti_eff_n$cumlength [(y-1)]
  } else{
    lpti_eff_n$cumlength [y] <-0
  }
}

  # trim to crit length
  lpti_eff_n <- lpti_eff_n[lpti_eff_n$cumlength < lpti_eff_n$crit, ]
  lpti_eff_n$si<- exp((-5*lpti_eff_n$cumlength)/lpti_eff_n$crit)

  lpti_eff_n$effi<- (1-lpti_eff_n$si)*lpti_eff_n$eff

  lpts$eq_pt1[j] <- 1- max(lpti_eff_n$effi,na.rm=TRUE)

}# about 1 second per iteration

# Interpolate values for eq_pt1
eq_pt1<- terra::interpNear(dem, as.matrix(lpts[,c(2,3,5)]), 1, interpolate=TRUE)

# Do the math
ndr <- eq_pt1 * eq_pt2

# Create runoof-modified load
mload<- load_n * (runoff/ terra::global(runoff,"mean",na.rm=TRUE)[1,1])

# Nutrient export per pixel
xexp<- ndr * mload

#plot(xexp/mload) # Proportion exported
#plot((load_n - xexp)/load_n) # Proportion not exported

out<-xexp/mload
  names(out)<-c("prop_export")
  return(out)
}
