
#' data_frame2map
#'
#' @param data as dataframe avec à minima une colonne pour les x, une colonne pour les y et une colonne pour le champs de valeur
#' @param crs_epsg as text
#' @param champs as text le nom du champs de valeur pour le plot? si champs=NA : seule l'emplacement des points sera dessiné
#' @param nom_champsx as text
#' @param nom_champsy as text
#' @param nom_champs_etiquette as text pour affichage des etiquettes
#' @param nom_champs_popup as text pour affichage quand on click sur le point
#' @param type_carte as text : "Leaflet" ou "Classique". Tout autre nom que Leaflet donnera une carte classique (defaut).
#' @param taille_etiquettes as real : taille d'affichage des etiquettes en mode classique uniquement
#' @description Permet de visualiser sur une carte un data frame contenant des données spatiales. Il corrige un bug connu de décallage des données
#' représentées sur les fonds Google map. Pour ne pas représenter un champs de valeur mais juste la position des points : champs=NA
#' @note auteur : AH/AtmoNA, modification : 22/04/2024 : ajout de la possibilite de creer une carte avec Leaflet
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' # tracer une diagonale de point au dessus de l'ouest de l'afrique associés à des valeurs aléatoires
#' df<-data.frame(x=-5:4,y=41:50,val=rnorm(400))
#' data_frame2map(df,champs="val")
#' data_frame2map(df,champs="val",nom_champs_etiquette="x",taille_etiquettes=5)
#' data_frame2map(df,champs="val",nom_champs_etiquette=NULL,type_carte="Leaflet")
#'}
data_frame2map<-function(data,crs_epsg=4326,champs=NA,nom_champsx="x",nom_champsy="y",nom_champs_etiquette=NULL,type_carte="Classique",taille_etiquettes=0.8,nom_champs_popup=NULL)
{
  #library(ggplot2)
  library(ggmap)
  library(sf)
  library(sp)
  library(raster)

  data_keep2<-data
  data_keep2<-st_as_sf(data_keep2,coords = c(nom_champsx, nom_champsy), remove = FALSE, crs = st_crs(crs_epsg))
  data_keep2<-st_transform(data_keep2, "+proj=longlat +ellps=WGS84")


  if (toupper(type_carte)=="LEAFLET")
  {
    library(leaflet)

    sh_taxon<-sf:::as_Spatial(data_keep2)
    crs_wgs84<-"+proj=longlat +ellps=WGS84 +datum=WGS84 "
    crs(sh_taxon)<-crs_wgs84
    if (!is.null(nom_champs_etiquette))
    {
        if (is.element(nom_champs_etiquette,names(sh_taxon@data)))
        {
          is_etiquettes<-TRUE
        } else {is_etiquettes<-FALSE;nom_champs_etiquette<-names(sh_taxon@data)[1]}
    }else {is_etiquettes<-FALSE;nom_champs_etiquette<-names(sh_taxon@data)[1]}


    if (is.null(nom_champs_popup)) {nom_champs_popup<-names(sh_taxon@data)[1]}


    leaflet(sh_taxon) %>%
      addMarkers(label=as.vector(sh_taxon@data[,nom_champs_etiquette]),
                 labelOptions = labelOptions(noHide = is_etiquettes),
                 popup = as.vector(sh_taxon@data[,nom_champs_popup])) %>%
      addTiles()



  } else {


      # Convert to 3857
      uk_lads_3857 <- st_transform(data_keep2, 3857)
      if (!is.na(champs)) {names(uk_lads_3857 )<-gsub(champs,"label_local94",names(uk_lads_3857))}
      bb<-st_bbox(data_keep2)
      bb<-bb+c(-(bb[3]-bb[1] )*0.2,-(bb[4]-bb[2])*0.2,(bb[3]-bb[1] )*0.2, (bb[4]-bb[2])*0.2 )
      test_map_uk <- ggmap::get_map(location = unname(bb), source = "stadia",maptype="stamen_terrain")

      # Define a function to fix the bbox to be in EPSG:3857
      ggmap_bbox <- function(map) {
        if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
        # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector,
        # and set the names to what sf::st_bbox expects:
        map_bbox <- setNames(unlist(attr(map, "bb")),
                             c("ymin", "xmin", "ymax", "xmax"))

        # Coonvert the bbox to an sf polygon, transform it to 3857,
        # and convert back to a bbox (convoluted, but it works)
        bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = crs_epsg)), 3857))

        # Overwrite the bbox of the ggmap object with the transformed coordinates
        attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
        attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
        attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
        attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
        map
      }

      # Use the function:
      test_map_uk <- ggmap_bbox(test_map_uk)


      if (!is.na(champs)) {
      map_here<-ggmap(test_map_uk) +
        coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
        geom_sf(data =  uk_lads_3857,aes(color = label_local94),
                inherit.aes = F)+ scale_color_gradient(low = "blue", high = "red")+ labs(color = champs)
      } else {
        map_here<-ggmap(test_map_uk) +
          coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
          geom_sf(data =  uk_lads_3857,
                  inherit.aes = F)

      }

      if (!is.null(nom_champs_etiquette)) {
          map_here + annotate("text",x=st_coordinates(uk_lads_3857)[,1],y=st_coordinates(uk_lads_3857)[,2],
                              label=as.vector(data[,nom_champs_etiquette]),size = taille_etiquettes,colour = "#4A495F")
      } else {

          map_here
        }
  }
}
