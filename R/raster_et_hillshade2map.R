

#' raster_et_hillshade2map
#'
#' @param r un raster avec son crs défini
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' library(raster)
#' fich<-"N:/4_developpements/COPERNICUS/IASI/NH3/nh3_2020-03-10_2020-04-20EU_mg_cm-2_lisse.tif"
#' raster(fich)->r
#' r <- crop(r, extent( -9, 9, 40, 50))
#' }
#'
raster_et_hillshade2map<-function(r,zoom_elev=5)
{



library(elevatr)
  library(raster)
  library(rasterVis)
  library(sp)
  library(maptools)
  library(maps)

DEM<-   get_elev_raster(r, z = zoom_elev)

slope <- terrain(DEM, 'slope')
aspect <- terrain(DEM, 'aspect')
hs <- hillShade(slope=slope, aspect=aspect,
                angle=20, direction=30)

## hillShade theme: gray colors and semitransparency
hsTheme <- modifyList(GrTheme(), list(regions=list(alpha=0.6)))

world.outlines <- map("world", plot=FALSE)
world.outlines.sp <- maptools::map2SpatialLines(world.outlines, proj4string = CRS("+proj=longlat"))


rasterVis::levelplot(r, #panel=panel.levelplot.raster,
          margin=FALSE, colorkey=FALSE) +
  rasterVis::levelplot(hs, par.settings=hsTheme, maxpixels=1e6) +
  latticeExtra::layer(sp.polygons(world.outlines.sp , fill='black', alpha=1),
                      data=list(world.outlines.sp =world.outlines.sp ))


}
