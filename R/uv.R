#' Modelling of UV protection following Na et al 2014
#'
#' This function models UV protection provided by the tree canopy.
#' @param lcm Land cover map including vegetation categories.
#' @param chm Canopy height model including vegetation height above the ground.
#' @param ccutoff Cutoff value for minimum vegetation height to consider as providing shade.
#' @param uvi Background UV Index values, either as SpatRaster or single numeric value.
#' @param skc Sky condition (cloudiness), either as SpatRaster or single value.
#' @param sza Solar zenith angle in degrees, either as SpatRaster or single numeric value.
#' @param resc Rescaling factor for reducing resolution to calculate canopy cover. Integer or NA if no aggregation required.
#' @param ltb Lookup table as matrix with two columns. First column must include all categories of land cover included in lcm. Second column indicates land covers that contribute to tree canopy cover, indicated by a 1.
#' @return A SpatRaster stack including four layers. First is the UV exposure below the canopy. Second is the UV exposure across the area. Third is UVPF below canopy. Fourth is UVPF across the area.
#' @export

# Write function
#ltb<- cbind( eslookup[,2], eslookup[,12])
#uvi=5.8
#skc="CLR"
#sza=25
hbrc.uv<- function(lcm,
                   chm,
                   ccutoff=1.6,
                   uvi,
                   sza,
                   resc=NA,
                   ltb,
                   skc="CLR"){

  # Extract parameters for sky condition
  data(skylookup)
  skcX<- skylookup[skylookup$skycondition ==skc,]

  # Convert land cover map to canopy cover
  canc <- c(terra::classify(lcm,
                  cbind(ltb[,1], ltb[,2])))

  # Find only canopy that meets the height threshold
canc[chm < ccutoff]<-0

  # Need to convert to proportion canopy cover (canm)
  if(!is.na(resc)){
    canm<- aggregate(canc, resc)

  }else{
   canm<-canc
  }

  # Convert solar zenith angle to radians
  sza2 <- sza *pi/180

  # Ts is the proportion of uv irradiance received below the canopy. So low is good
 # Average in-shade locations
Tsshaded<- ((skcX$t4a + (skcX$t4b * (sza2^skcX$t4c)))*(1-canm))-((sza2^skcX$t4d)/skcX$t4e)*sin(pi*canm)

# Average unshaded conditions
Tsoverall <-(skcX$t5a*(1- canm))-((sza2^skcX$t5b/ skcX$t5c)*sin(pi * canm))

# Make sure not below 0 or above 1
Tsshaded[Tsshaded<0]<-0
Tsshaded[Tsshaded>1]<-1
Tsoverall[Tsoverall<0]<-0
Tsoverall[Tsoverall>1]<-1

# Create UVIo from UVI according to transmision rate
uvi0 <- uvi/ skcX$transmission

# Predicted uvi fraction below canopy
bcuvi <- c(Tsshaded,Tsoverall) * uvi0

# UV protection factor
uvpf<-  c(uvi,uvi0)  / bcuvi
# Trim to 100 because it sometimes goes to infinite in high canopy cover
uvpf[uvpf>100]<-100

uvout<- c(bcuvi, uvpf)
names(uvout)<- c("bcuvi.shade","bcuvi.overall","uvpf.shade","uvpf.overall")
  # Return output
return(uvout)
}
