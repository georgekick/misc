#' function: kml_dens
#' @param <name> <description>
#' @param <name> <description>
#' @return <description>
#' @export (*delete* exports function (no documentation))
#' @examples <Rcode>
#' \donttest{
#' 
#' }
#' @example <path to R file>
#' @note <contents>
#' @section <name> : <contents>
#' @references <references>

kml_dens<- function(spdf, h=0.05, n=128, kml.folder=getwd()){
  sp.pix <- kde.points(spdf, h=h, n=n)

  sp.grd <- as(sp.pix, "SpatialGridDataFrame")
  sp.grd@data$kde[sp.grd@data$kde < quantile(sp.grd@data$kde, 0.5)] <- NA

  #kernel2KML.R
  sp.grd.kml <- GE_SpatialGrid(sp.grd)
  tf <- file.path(kml.folder, "Density")
  png(file=paste(tf, ".png", sep=""), width=sp.grd.kml$width,
    height=sp.grd.kml$height, bg="transparent")
  par(mar=c(0,0,0,0), xaxs="i", yaxs="i")
  Lab.palette <-
    colorRampPalette(c("green", "yellow", "red"), space = "Lab")
  image(as.image.SpatialGridDataFrame(sp.grd[1]), col=Lab.palette(10),
      xlim=sp.grd.kml$xlim, ylim=sp.grd.kml$ylim)
  # kmlOverlay(sp.grd.kml, paste(tf, ".kml", sep=""), paste(tf, ".png", sep=""))
  kmlOverlay(sp.grd.kml, kmlfile=paste(tf, ".kml", sep=""), imagefile="Density.png", name="Kernel_Density")

  dev.off()
    
}

