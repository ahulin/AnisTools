#' project_dataframe
#'
#' @param d : dataframe with 2 columns minimum : "x", "y" ou "lat","lon"
#' @param srid_in : srid de la projection source : https://app.dogeo.fr/Projection/#/point-to-coords
#' @param nomx as text : le nom du champs contenant les x dans le dataframe d'entree
#' @param nomy  as text : le nom du champs contenant les y dans le dataframe d'entree
#' @param srid_out : srid de la projection de sortie : https://app.dogeo.fr/Projection/#/point-to-coords
#'
#' @description : permet de convertir les coordonees geographiques d'un data frame d'un système vers un autre. On precise le nom des colonnes x et y
#' @return
#' @export
#'
#' @examples
#' d<-data.frame(id=1:5,lon=seq(-3,3,length.out=5),lat=seq(45,46,length.out=5))

project_dataframe<-function(d,srid_in=NULL,srid_out=NULL,nomx=NULL,nomy=NULL)
{

  library(sp)
  d_out<-d
  names(d_out)<-toupper( names(d_out))
  if (is.null(nomx)|is.null(nomy))
  {
    if (is.element("X",names(d_out)))
    {
        spd<-SpatialPointsDataFrame(d_out[,c("X", "Y")],data=d_out)
    } else  if (is.element("LAT",names(d_out)))
    {
        spd<-SpatialPointsDataFrame(d_out[,c("LON", "LAT")],data=d_out)
    } else {stop("Le dataframe doit contenir soit les champs x/y, soit les champs lat/lon")}
  } else if (!is.element(nomx,names(d))|!is.element(nomy,names(d)))
  {
    stop("Les noms de champs definis n'existent pas")
  } else {spd<-SpatialPointsDataFrame(d[,c(nomx,nomy)],data=d)}

  if (is.null(srid_in))
  {
      print(info_srid())
       # 1. renseigner les srid

    # demande a l'utilisateur le srid d'origine
      while(is.null(srid_in))
      {
        srid_in<-readline(prompt=("SRID source (srid_in: )"))
      }

    }

  proj4string(spd) <- CRS(paste0('+init=EPSG:',srid_in))
  if (is.null(srid_out))
  {
    print(info_srid())
    # 1. renseigner les srid

    # demande a l'utilisateur le srid d'origine
    while(is.null(srid_out))
    {
      srid_out<-readline(prompt=("SRID de sortie (srid_out: )"))
    }

  }
  new_proj<-CRS(paste0('+init=EPSG:',srid_out))
  spd<-spTransform(spd, new_proj)
  names(spd)<-names(d)
  return(data.frame(spd))

}

#' info_srid
#'
#' @return
#' @export
#'
#' @examples
#' print(info_srid())
info_srid<-function()
{
  #https://app.dogeo.fr/Projection/#/point-to-coords
  liste<-data.frame(
    noms=c("wgs84","lambert93","web mercator wgs84","GPS WGS84 (UTM) zone 31"),
    srid=c("4326","2154","3857","32631"),
    crs=c("+proj=longlat +ellps=WGS84 +datum=WGS84",
           "",
           "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs",
           "+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs"
           ),
    comment=c(
          "wgs84",
          "lambert93",
          "wgs84 pour representation de la France non deformee",
          "pour certaines donnees satellites" )


    )
  return(liste)



  }
