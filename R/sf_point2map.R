

#' sf_point2map
#'
#' @param data  : point shape, format sp ou sf
#' @param champ : as text nom du champ à représenter
#' @param titre as text : le titre à ajouter en haut de la carte (NULL par defaut = blanc)
#' @param etiquettes as text : le champs à utiliser pour représenter des étiquettes à droite des points ou sur le point (cf placement_etiquettes)
#' @param representation as text : "taille" ou "couleur" pour la représentation du champs des points
#' @param zoom_ : zoom pour la carte du fond. 6 par defaut. (1 : monde, + de 10 : ville)
#' @param placement_etiquettes as text : "droite" ou "dessus"
#'
#' @description permet de créer une carte avec un fond OSM pour représenter une couche de points sf ou sp. On peut aussi (optionnel) afficher un champs en etiquette
#' @return une carte au format ggmap
#' @export
#'
#' @examples
#' \dontrun{
#' data_keep2<-data.frame(xco=c(-1,0,0.5),yco=c(46,46.2,46.3),desc=c("g","f","p"),desc2=1:3)
#' data_keep2<-st_as_sf(x=data_keep2,coords = c(1, 2), remove = FALSE, crs = st_crs("+proj=longlat +ellps=WGS84"))
#' sf_point2map(data_keep2,"desc2",zoom_=9,titre="localisation des mesures",etiquette="desc",representation="couleur",placement_etiquette="droite")
#' }
sf_point2map<-function(data,champ=NULL,zoom_=3,titre=NULL,etiquettes=NULL,representation="taille",placement_etiquette="droite")
{
  library(sp)
  library(sf)

  library(dplyr, warn.conflicts = F)
  library(ggmap, quietly = T)

  # on vérifit que le champ est renseigne et est l'un des champs de data
  if (is.null(champ)|!is.element(champ,names(data))) stop("Le champ doit être spécifié et valide")

  #on verifit le format du sh
  if(!any(class(data)[1] %in% c("SpatialPointsDataFrame", "sf") )) stop("data doit être un vecteur de points sf ou sp")
  if (class(data)[1] == "SpatialPointsDataFrame") data<-st_as_sf(data) # on converti en sf si le shape est passe en sp

  if (!is.numeric(data.frame(data)[,champ]) ) {stop("Le champ doit etre numeric")}

  if (st_crs(data)=="") {stop("Pas de projection definie pour la couche de points")}

  meuse <- st_as_sf(data)
  meuse <- meuse %>%
    st_transform(4326)

  if (!is.null(etiquette))
  {
    dmeuse<-as.data.frame(meuse,xy=TRUE)
    dmeuse_coords <- do.call(rbind, st_geometry(meuse))
    dmeuse$lon=  dmeuse_coords[,1]
    dmeuse$lat=  dmeuse_coords[,2]
  }

  bb<-st_bbox(meuse)
  lon2<-(bb[3]-bb[1])*0.15
  lat2<-(bb[4]-bb[2])*0.15
  bb[1]<-bb[1]-lon2
  bb[2]<-bb[2]-lat2
  bb[3]<-bb[3]+lon2
  bb[4]<-bb[4]+lat2

  keyapi<-NULL
  keyapi<-get_api_ggmap()
  if (!is.null(keyapi))   ggmap::register_stadiamaps(keyapi)

  #meuse_map <- get_map(
  #  bbox = unname(bb),
  #  zoom = zoom_ ,  source = "stadia",maptype="stamen_terrain"
  #) %>% ggmap()

  meuse_map<-ggmap::get_map(location = unname(bb), source = "stadia",maptype = "stamen_terrain", zoom =zoom_)

  if (toupper(representation)=="COULEUR")
  {
  meuse_map<-ggmap(meuse_map) +
    ggplot2::geom_sf(
      data = meuse,
      aes(color = !!sym(champ)),
      size = 3, alpha = 0.5,
      show.legend = 'point', inherit.aes = F
    )
  } else
  {
    meuse_map<-ggmap(meuse_map) +
      ggplot2::geom_sf(
        data = meuse,
        aes(size = !!sym(champ)),
        color = 'red', alpha = 0.5,
        show.legend = 'point', inherit.aes = F
      )
  }
  meuse_map<-meuse_map+  scale_color_gradient(low = "blue", high = "red") + ggtitle(titre)

  # ajout des etiquettes de donnees
  if (!is.null(etiquettes))
  {
    if (toupper(placement_etiquette)=="DESSUS")
    {
      meuse_map<- meuse_map +
        geom_label(data=data,aes(label= !!sym(etiquettes),
                                  x=st_coordinates(st_centroid(meuse))[,1],
                                  y=st_coordinates(st_centroid(meuse))[,2]),size=3)
    } else    {
      meuse_map<- meuse_map +
        geom_label(data=data,aes(label= !!sym(etiquettes),
                                 x=st_coordinates(st_centroid(meuse))[,1],
                                 y=st_coordinates(st_centroid(meuse))[,2]),size=3,nudge_x=(bb[3]-bb[1])/40)
    }


  } # fin de !is.null(etiquettes)

  print(meuse_map)
  return(meuse_map)

}
