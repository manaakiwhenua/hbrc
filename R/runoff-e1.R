#' Modelling of runoff retention based on MBIE Building Code Clause E1 Surface Water
#'
#' This function models runoff retained by all land covers and additionally separates the contribution of ecosystems.
#' @param lcm Land cover map including vegetation categories.
#' @param stm Soil type map in three broad classes indicating 1 = high infiltration, 2 = mid infiltration, 3 = low infiltration.
#' @param slo Slope map in units of percentage slope.
#' @param ltb Lookup table as matrix with four columns. First column must include all categories of land cover included in lcm. The remaining three columns must give curve numbers for high, mid, and low infiltration soils respectively.
#' @return A SpatRaster with one layer indicating the proportion runoff retained.
#' @export

# ltb <- eslookup[,c(2,4,5,6)]
#slo <- tan(terra::terrain(dem, v="slope", unit="degrees"))*100
#slo[slo<0]<-0
#slo[slo>100]<-100
#stm<-hsg-1
# Write function
hbrc.runoff.e1 <- function(lcm,
                        stm,
                        slo,
                        ltb){

  # Convert land cover map to curve numbers
  rcmap<- c(terra::classify(lcm,
                            cbind(ltb[,1], ltb[,2])),
            terra::classify(lcm,
                            cbind(ltb[,1], ltb[,3])),
            terra::classify(lcm,
                            cbind(ltb[,1], ltb[,4])))

  # Select correct layer from stm
  rcmap2<- app(c(stm, rcmap),
     fun = function(x){x[2:4][x[1]]})

  # This method is not working as of 13-12-2023, so we go to the slower app method
  #rcmap2 <- rcmap[[1]] # note that this is quicker than the more elegant way using app
 # rcmap2[stm==2]<- rcmap[[2]][stm==2]
 # rcmap2[stm==3]<- rcmap[[3]][stm==3]
rm(rcmap)

# Slope adjustment factor
saf<- data.frame(smin = c(0,5,10,20),
                 smax = c(5, 10, 20 , Inf),
                 adjfactor = c(-0.05, 0, 0.05, 0.1))

rcmap2<- rcmap2 +terra::classify(slo, saf)
rcmap2[rcmap2>1]<-1
rcmap2[rcmap2<0]<-0

#rcmap2 shows proportion of runoff
# convert from runoff to retained
Retained<- 1-rcmap2

# Return output
return(Retained)
}
