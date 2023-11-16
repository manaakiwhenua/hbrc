#' Modelling of Leaf Area Index from Landsat images
#'
#' This function models Leaf Area Index from Landsat
#' @param lcm Land cover map including vegetation categories.
#' @param lsat Pre-processed Landsat 8 image with bands 3, 4, 5, and 7.
#' @param ltb Lookup table as matrix with two columns. First column must include all categories of land cover included in lcm. Second column indicates land covers that contribute to tree canopy cover, indicated by a 1.
#' @return A rast object with one layer indicating leaf area index.
#' @export

#ltb<- cbind( eslookup[,2], eslookup[,12])
# Write function
hbrc.lai <- function(lcm,
                        lsat,
                       ltb){


  lsat2<-terra::crop(lsat,lcm)
  lsat2<- (lsat2[[2]]/ lsat2[[1]])*
    (1-
       (lsat2[[3]] -terra::global(lsat2[[3]],min,na.rm=TRUE)[1,1] )/
       (terra::global(lsat2[[3]], max,na.rm=TRUE)[1,1] -terra::global(lsat2[[3]], min,na.rm=TRUE)[1,1]))
  lsat[lsat< 0]<- 0


  # from Kato et al 2013
  lsat2<- 1.6507269 + (lsat2* -2.2459678)+
    (lsat2^2 *  0.7918144 )
  lsat2[lsat2<0]<-0#trim to range of Kato et al dataset
  lsat2[lsat2>5.5]<-5.5

  # Mask to tree dominated areas only
  tco<- terra::classify(lcm, ltb)
  tco<- terra::resample(tco, lsat)
  # Tree dominated is defined as more than 60% cover
lsat2[tco < 0.6]<-0


names(lsat2)<-c("lai")
return(lsat2)

}
