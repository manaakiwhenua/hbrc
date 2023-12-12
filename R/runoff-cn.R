#' Modelling of runoff retention using a curve number approach
#'
#' This function models runoff retained by all land covers and additionally separates the contribution of ecosystems.
#' @param lcm Land cover map including vegetation categories.
#' @param hsg Map of Hydrologic Soil Group as a SpatRaster with numeric values indicating 1 = HSG A, 2 = HSG B, 3 = HSG C, 4 = HSG D.
#' @param ltb Lookup table as matrix with five columns. First column must include all categories of land cover included in lcm. The remaining four columns must give curve numbers for HSG A, B, C, and D respectively.
#' @param precip Precipitation value given either as a single numeric value or SpatRaster.
#' @param antecedent Character value describing antecedent soil moisture conditions, either "dry", "average", or "wet".
#' @return A SpatRaster with one layer indicating the proportion runoff retained.
#' @export

# Write function
# ltb <- hb.eslookup[,c(2,7,8,9,10)]
hbrc.runoff.cn <- function(lcm,
                        hsg,
                        ltb,
                        precip,
                        antecedent = "average"){

  # Convert land cover map to curve numbers
  cnmap<- c(terra::classify(lcm,
                  cbind(ltb[,1], ltb[,2])),
    terra::classify(lcm,
                    cbind(ltb[,1], ltb[,3])),
    terra::classify(lcm,
                    cbind(ltb[,1], ltb[,4])),
    terra::classify(lcm,
                    cbind(ltb[,1], ltb[,5])))

  # Select correct layer from hsg
  cnmap<- app(c(hsg, cnmap),
      fun = function(x){x[2:5][x[1]]})

  # This method is not working as of 13-12-2023, so we go to the slower app method
  #cnmap2 <- cnmap[[1]] # note that this is quicker than the more elegant way using app
  #cnmap2[hsg==2]<- cnmap[[2]][hsg==2]
  #cnmap2[hsg==3]<- cnmap[[3]][hsg==3]
  #cnmap2[hsg==4]<- cnmap[[4]][hsg==4]

  # antecedent conversion
  if(antecedent == "average"){

  }else if(antecedent == "dry"){
cnmap2 <- (4.2*cnmap2)/(10- (0.058*cnmap2))
  } else if( antecedent == "wet"){
    cnmap2 <- (23*cnmap2)/(10+ (0.13*cnmap2))
  }

  # Convert cn to runoff
 S<- 25400/cnmap2 -254
 Q<- (precip-(0.2*S))/(precip+(0.8*S))

  # if precip is less than (0.2*S), then no retention
  nor<-precip < S*0.2
  Q[nor == 1]<-1

  # convert from runoff to retained
  Retained<- 1-Q

  # Return output
return(Retained)
}
