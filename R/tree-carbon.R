#' Modelling of biomass carbon stocks in trees
#'
#' This function models biomass carbon stocks in trees based on allometric equations.
#' @param trees Tree locations as a SpatVector.
#' @param treeheight Vector of tree height. Must be same length as trees.
#' @param treedbh Vector of tree diameter at breast height. Must be same length as trees. Optional - tree dbh will be estimated from height if no values are provided.
#' @param method Method of approximation of carbon stock - currently either "beets" for Beets et al 2012 approach or "sm" for Schwendenmann and Mitchell 2014.
#' @param maxheight Maximum height allowable, can be used to constrain very high value.
#' @return A SpatVector including estimates of carbon stocks.
#' @export

# Write function
# ltb <- hb.eslookup[,c(2,7,8,9,10)]
#treeheight=hb.trees$height
hbrc.tree.c <- function(trees,
                        treeheight,
                        treedbh = NA,
                        method ="beets",
                        maxheight = Inf){

  # Trim maxheight
  treeheight[treeheight > maxheight]<- maxheight

  # If DBH is not provided, estimate it
  # Christchurch street trees provide the relationship
  # this is a log10-log10 model
  hdmodel<- data.frame( parameters=
  c("intercept","height"),
  estimates=
  c(-1.453288, 1.086455),
   se=
c(0.009817, 0.13572))

  if(is.na(treedbh)){
treedbh2 <- 10^((log10(treeheight)*hdmodel$estimates[2]) +hdmodel$estimates[1])
  } else {
    treedbh2<- treedbh
  }

  # Logic checks
treedbh2[treedbh2 <0]<-0

treedbh2b<-treedbh2*100

  if(method=="beets"){
    # Beets general multi-species equation
    treedbh3<- (treedbh2*100)*treeheight
    kgc <- (4.83e-5 * treedbh3^0.978 +
                       1.62e-2 * treedbh3^0.943 +
                       1.75e-2 * treedbh2b^2.2 +
              1.71e-2 * treedbh2b^1.75)
  } else if(method == "sm"){
    # Schwendenmann + Mitchell 2014 for urban trees
    treedbh3<- treedbh2*100
    kgc <-  (0.0162 * (((( treedbh3)^2)*treeheight)^0.943 ))+
      (0.0175 * ((treedbh3)^2.2 )) +
      (0.0171 * ((treedbh3)^1.75 ))

}

# convert by + 0.25 for root biomass - Dale 2013
# kgc*1.25


  # Return output
trees$kgc <- kgc
return(trees)
}



# Beets general multi-species equation from another source?
# treedbh3<- (treedbh2*100)
# kgc <- (0.0023 * treedbh3^3.3885 +
#           0.0121 * treedbh3^2.5276 +
#           0.009 * treedbh2^2.4966)
