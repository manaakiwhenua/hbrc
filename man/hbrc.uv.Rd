% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/uv.R
\name{hbrc.uv}
\alias{hbrc.uv}
\title{Modelling of UV protection following Na et al 2014}
\usage{
hbrc.uv(lcm, chm, ccutoff = 1.6, uvi, sza, resc = NA, ltb, skc = "CLR")
}
\arguments{
\item{lcm}{Land cover map including vegetation categories.}

\item{chm}{Canopy height model including vegetation height above the ground.}

\item{ccutoff}{Cutoff value for minimum vegetation height to consider as providing shade.}

\item{uvi}{Background UV Index values, either as SpatRaster or single numeric value.}

\item{sza}{Solar zenith angle in degrees, either as SpatRaster or single numeric value.}

\item{resc}{Rescaling factor for reducing resolution to calculate canopy cover. Integer or NA if no aggregation required.}

\item{ltb}{Lookup table as matrix with two columns. First column must include all categories of land cover included in lcm. Second column indicates land covers that contribute to tree canopy cover, indicated by a 1.}

\item{skc}{Sky condition (cloudiness), either as SpatRaster or single value.}
}
\value{
A SpatRaster stack including four layers. First is the UV exposure below the canopy. Second is the UV exposure across the area. Third is UVPF below canopy. Fourth is UVPF across the area.
}
\description{
This function models UV protection provided by the tree canopy.
}
