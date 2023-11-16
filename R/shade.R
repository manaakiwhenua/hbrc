#' Modelling of shade provided by vegetation cover
#'
#' This function models shade provided by vegetation cover.
#' @param lcm Land cover map including vegetation categories.
#' @param dsm Digital surface model including full canopy.
#' @param chm Canopy height model including vegetation height above the ground.
#' @param ccutoff Cutoff value for minimum vegetation height to consider as providing shade.
#' @param resc Rescaling factor for reducing resolution. Integer or NA if no aggregation required.
#' @param sv Azimuth and zenith of the sun. Matrix with two columns. Can be created from a sequence of dates/ times using insol; see Example.
#' @return A rast stack including four layers, all indicate proportion of time shaded. First is the surface shade with vegetation. Second is surface shade without vegetation. Third is shade with vegetation including under canopy. Fourth is contribution of vegetation in providing shade.
#' @export
#' @examples
#' # Example of creating the sv variable from a vector of dates and location
#' sv<- insol::sunpos(insol::sunvector(
#'  c(insol::JD(seq(ISOdate(2019,02,01,8),ISOdate(2019,02,01,18),by='hour'))),
#'  latitude = -43.52,
#'  longitude = 172.64,
#'  timezone=13))

# Write function
# sv<- insol::sunpos(insol::sunvector(
#   c(insol::JD(seq(ISOdate(2019,02,01,8),ISOdate(2019,02,01,18),by='hour'))),
#   latitude = -43.52,
#   longitude = 172.64,
#   timezone=13))
# resc <- 10
hbrc.shade <- function(lcm,
                        chm,
                        ccutoff = 0,
                        resc = NA,
                        dsm,
                        sv){

  # Make no veg canopy
  chm[is.na(chm)]<-0
  t2<- dsm-chm
  # Make inverse of veg canopy
  t3 <- t2-chm

if(!is.na(resc)){
  t1<- aggregate(dsm, resc)
  t2<- aggregate(t2, resc)
  t3<- aggregate(t3, resc)
}else{
  t1<-dsm
}

  ps<-t1
  ps[,]<-0
  ps2<-ps
  ps3<-ps


  for(i in 1:length(sv[,1])){

    # Veg canopy
    tg<- rayshader::ray_shade(as.matrix(t1, wide=TRUE),
                   sv[i,1],
                   sv[i,2],
                   lambert=FALSE)#,
    #multicore = TRUE)
    tg<-terra::rast(tg, extent = terra::ext(t1),crs=terra::crs(t1) )
    tg<- terra::flip(tg,direction="vertical")

    tg<-1-tg

    ps<- ps+tg
    tgs<- tg

    # No veg canopy
    tg<- rayshader::ray_shade(as.matrix(t2, wide=TRUE),
                   sv[i,1],
                   sv[i,2],
                   lambert=FALSE)
    tg<-terra::rast(tg, extent = terra::ext(t1),crs=terra::crs(t1) )
    tg<- terra::flip(tg,direction="vertical")

    tg<- 1-tg
    ps2<- ps2+tg


    # Inverse of veg canopy
    tg<- rayshader::ray_shade(as.matrix(t3, wide=TRUE),
                   sv[i,1],
                   sv[i,2],
                   lambert=FALSE)
    tg<-terra::rast(tg, extent = terra::ext(t1),crs=terra::crs(t1) )
    tg<- terra::flip(tg,direction="vertical")

    tg<-1-tg
    tg[t3 == 0]<-0

    tg<- max(c(tg,tgs))

    ps3<- ps3+tg
  }

    shaded<-c(ps,ps2,ps3, ps3-ps2)
    names(shaded)<-c("vegshade","novegshade","vegundershade", "vegcontribution") # undershade is both under and tree canopy shade
 shaded[[4]][shaded[[4]]< 0]<-0 # Just a check, should never be less than 0
 shaded<-shaded/length(sv[,1])

return(shaded)
}
