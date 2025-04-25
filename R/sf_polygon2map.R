
#' sf_polygon2map
#'
#' @param sf_poly un vecteur polygone de type sf. Si non renseigne, la foncion ouvre une fenetre pour selectionner le fichier.
#' @param champs as text un champs numerique ou nom pour la representation
#' @param titre as text titre a ajouter au graphique
#' @param nom_legende as text titre de la legnde. Si NULL, sera le nom de la variable
#' @param echelle_couleur_viridis as text. "viridis" "magma" "plasma" "inferno" "cividis" "mako" "rocket" "turbo" . cf https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
#' @param transparence transparence de la couche polygone
#' @param contour_poly as text : faire apparaitre le contour ou non pour les polygones ("oui"/"non"), avec la meme couleur que le remplissage ("oui_idem")
#' @param legende as boolean : est ce que la legende doit apparaitre
#' @param zoom_fond as interger : map zoom, an integer from 3 (continent) to 21 (building), default value 10 (city). openstreetmaps limits a zoom of 18, and the limit on stamen maps depends on the maptype. "auto" automatically determines the zoom for bounding box specifications, and is defaulted to 10 with center/zoom specifications. maps of the whole world currently not supported.
#' @param rev_col as interger : 1 ou -1 . -1 : va du plus clair au plus fonce.1: l'inverse.
#' @param echelle_cut as text.Correspond au type d'echelle a utiliser, NULL par defaut. valeurs possibles : one of "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih", "headtails", or "maximum"
#' @param n_class as integer : imposer le nombre de classe. 5 par defaut
#' @param type_carte options available  "stamen_terrain", "stamen_toner", "stamen_toner_lite", "stamen_watercolor", "stamen_terrain_background", "stamen_toner_background", "stamen_terrain_lines", "stamen_terrain_labels", "stamen_toner_lines", "stamen_toner_labels" (Stadia Maps)
#' @param cex_text as numeric : facteur multiplicatif proportionel qui sera applique a la taille du texte du titre, de la legende et du titre de la legende. 1 par defaut
#' @param interval_leg as vector of 2 num : ex : c(0,3) : impose un interval lineaire entre les 2 bornes. Les bornes peuvent depasser ou être inferieures à la gamme des valeurs du champs represente.
#' @return
#' @export
#'
#' @details pour utiliser la fonction avec un raster r3 : st_as_sf(rasterToPolygons(r3 ))->poly
#' @note  dernieres modifications : septembre 2023 : ajout de la possibilite de mettre des etiquettes
#' !! octobre 2023 : stamen nous laisse tomber pour les fonds de carte. Desormais il faut demander une cle API a stadiamaps. ggmap doit etre reinstalle.
#' https://docs.stadiamaps.com/guides/migrating-from-stamen-map-tiles/
#' remove.packages("ggmap")
#' devtools::install_github("stadiamaps/ggmap")
#' library(ggmap)
#' register_stadiamaps("xxx-xxx-xxx-xxx",write = TRUE). write=true pour que ce soit permanent et qu'on ai plus a faire de register pour ce poste
#' us <- c(left = -125, bottom = 25.75, right = -67, top = 49)
#' get_stadiamap(us, zoom = 5, maptype = "stamen_toner") %>% ggmap()

#' @examples
#' \dontrun{
#' fich_com_na<-"K:\\Partenariat_Innovation\\SIG\\France\\GEOFLA\\communes_NA.shp"
#' com_na<-sf::st_read(fich_com_na,quiet=TRUE)
#' sf_polygon2map(sf_poly=com_na,champs="iasi2",zoom_fond=7,contour_poly = "non",interval_leg=c(1000,4000),echelle_cut="fixed",n_class=40)
#'
#' sf_polygon2map(titre="Titre du graphique", nom_legende="variable cartographiee")
#' }
sf_polygon2map<-function(sf_poly=NULL,champs=NULL,titre=NULL, nom_legende=NULL,echelle_couleur_viridis="viridis",interval_leg=NULL,
                         transparence=0.4,contour_poly="oui",legende=TRUE,zoom_fond=10,rev_col=1,echelle_cut=NULL,n_class=5,cex_text=1,etiquettes=NULL,type_carte="stamen_terrain")
{

  library(ggplot2)
  library(ggmap)
  library(sf)
  library(rlang)
  #if(!any(x %in% c("SpatRaster", "RasterLayer", "stars") )
  if(!inherits(sf_poly, c("sf", "SpatialPolygonsDataFrame"))) stop("sf_poly doit etre un vecteur de polygones sf ou sp")

  if (inherits(sf_poly, "SpatialPolygonsDataFrame")) sf_poly<-st_as_sf(sf_poly) # on converti en sf si le shape est passe en sp




  #> Linking to GEOS 3.6.2, GDAL 2.3.0, proj.4 5.1.0

  # gerer le parametre *cex_text pour la taille du texte
  if (is.na(cex_text)|is.null(cex_text))
  {
    cex_text=1
  } else if (cex_text==0)
  {
    print("Cex_text ne peut pas etre egal a zero, il est ramene a la valeur par defaut (1)")
    cex_text<-1
  } else if (cex_text<0.1|cex_text>10)
  {
    message("la valeur de cex_text depasse les limites autorisees[0.1-10], la valeur est ramene a la valeur par defaut (1).")
    cex_text<-1
  }
  #nc <- st_read(nn, quiet = TRUE)
  if (is.null(sf_poly))
  {
    nn<-file.choose()
    sf_poly <- st_read(nn)
  }

  if (is.null(champs))
  {
    print(names(sf_poly))
    champs=""
    while (!is.element(champs,names(sf_poly)))
    {
      champs <- readline(prompt="Enter name nomchamps (sans guillements): ")
    }
  }

  if (dim(sf_poly)[1]==0) {stop("Le polygone ne contient pas d'elements")}

  #si on a impose un interval pour les valeur de la legende, alors echelle_cut est forcement fixed (pour le moment, c'est une piste d'amelioration)
  if (!is.null(interval_leg)) {
    echelle_cut<-"fixed"
    print("Echelle_cut a ete forcee à 'fixed' car interval_leg n'est pas null")
    if (interval_leg[1]>min(data.frame(sf_poly)[,champs],na.rm=TRUE)) message("Attention : il y a des valeurs en dessous de la borne inferieure de la legende. Elles ne seront pas representees.")
    if (interval_leg[2]<max(data.frame(sf_poly)[,champs],na.rm=TRUE)) message("Attention : il y a des valeurs au dessus de la borne superieure de la legende. Elles ne seront pas representees.")

  }

  if (is.null(nom_legende)) {nom_legende<-champs}

  # on ne garde que le champs de valeurs et l'eventuel champs des etiquettes
  sf_poly<-sf_poly[,c(champs,etiquettes), drop = FALSE]


  nc<-st_transform(sf_poly, "+proj=longlat +ellps=WGS84")
  # Transform nc to EPSG 3857 (Pseudo-Mercator, what Google uses)
  nc_3857 <- st_transform(nc, 3857)

  keyapi<-NULL
  keyapi<-get_api_ggmap()
  if (!is.null(keyapi))   ggmap::register_stadiamaps(keyapi)
  map <- ggmap::get_map(location = unname(st_bbox(nc)), source = "stadia",maptype = type_carte, zoom =zoom_fond)




  #us <- c(left = -125, bottom = 25.75, right = -67, top = 49)
  #get_stadiamap(us, zoom = 5, maptype = "stamen_toner") %>% ggmap()



  #map <- get_map("north carolina", maptype = "satellite", zoom = 6, source = "google")

  # Define a function to fix the bbox to be in EPSG:3857
  ggmap_bbox <- function(map) {
    if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
    # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector,
    # and set the names to what sf::st_bbox expects:
    map_bbox <- setNames(unlist(attr(map, "bb")),
                         c("ymin", "xmin", "ymax", "xmax"))

    # Coonvert the bbox to an sf polygon, transform it to 3857,
    # and convert back to a bbox (convoluted, but it works)
    bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))

    # Overwrite the bbox of the ggmap object with the transformed coordinates
    attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
    attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
    attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
    attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
    map
  }

  # Use the function:
  map <- ggmap_bbox(map)

  if (legende) {lp="right"} else {lp="none"}



  gg<- ggmap(map) +
    coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
    ggtitle(titre)+
    labs(    fill = nom_legende)+
    theme(legend.position = lp, plot.title = element_text(size=18*cex_text),
          legend.title = element_text( size = 14*cex_text),
          legend.text = element_text( size = 10*cex_text)
    )  +  guides(color = FALSE)


  l<-NULL
  if (!is.null(echelle_cut))
  {
    if (!is.null(interval_leg))
    {
      #interval_leg<-c(0,20)
      # on crée 2 polygones fictifs autour de la coordonées 0,0 avec un qui prend la valeur min et l'autre la valeur max de l'interval voulu dans la légende.
      for (vv in interval_leg)
      {
        outer1 = list(list(matrix(c(0,0,0.001+vv/1000,0,0.001+vv/1000,0.001+vv/1000,0,0.001,0,0),ncol=2, byrow=TRUE)))
        pts3 = lapply(outer1, function(x) lapply(x, function(y) cbind(y, 0)))
        mp2 = st_multipolygon(pts3)
        polc = st_sfc(mp2, crs=3857)
        polc<-st_as_sf(polc)
        polc[1,champs]<-vv
        names(polc)[1]<-"geometry"
        st_geometry(polc)<-"geometry"
        polc<-polc[,c(champs,"geometry")]
        nc_3857<-rbind(nc_3857,polc)
        nc_3857<-sf::st_zm(nc_3857,DROP=TRUE)
      } # fin de for vv

      classes <-NULL
      classes$brks <- seq(from=interval_leg[1],to=interval_leg[2],length.out=n_class)
    } else {
      classes <-NULL
      classes <- classInt::classIntervals(nc_3857[[champs]], n = n_class, style = echelle_cut)
    } # fin de if (!is.null(interval_leg))

    # on crée un label propre pour la légende parceque l'automatique n'est pas très beau
    if (max(classes$brks)<0.01) labs<-format(classes$brks, scientific = TRUE,digits = 2)
    if (max(classes$brks)>=0.01) labs<-format(classes$brks, digits = 2)
    if (max(classes$brks)>=100000) labs<-format(classes$brks, scientific = TRUE,digits = 2)

    labs<-gsub("^\\s+|\\s+$", "", labs)  # on retire les espaces
    for (nn in 1:(length(labs)-1))
    {
      l<-c(l,paste0("[",labs[nn],",",labs[nn+1],"]"))
    }


    nc_3857[[champs]]<-cut(nc_3857[[champs]],classes$brks,labels =l) ##
    nc_3857<- subset(nc_3857,!is.na(nc_3857[[champs]])) ##
  } # fin de !is.null(echelle_cut))






  # echelle de couleur discrete (d) ou continue (c)
  if (is.numeric(data.frame(nc_3857)[,champs]))
  {
    gg<-gg + scale_fill_viridis_c(option=echelle_couleur_viridis,direction=rev_col) + scale_color_viridis_c(option=echelle_couleur_viridis,direction=rev_col)
  } else {
    gg<-gg + scale_fill_viridis_d(option=echelle_couleur_viridis,direction=rev_col) + scale_color_viridis_d(option=echelle_couleur_viridis,direction=rev_col)
  }

  # avec contour de polygon ou non, avec ou non  la couleur de remplissage (oui_idem)
  if (contour_poly=="oui_idem")
  {
    gg<-  gg+geom_sf(data = nc_3857, aes(fill = !!sym(champs),color = !!sym(champs)), inherit.aes = FALSE, alpha = transparence)
  } else if (contour_poly=="non") {
    gg<-  gg+geom_sf(data = nc_3857, aes(fill = !!sym(champs)), inherit.aes = FALSE, alpha = transparence,color = NA)

  } else  { # contour_poly=="oui"
    gg<-  gg+geom_sf(data = nc_3857, aes(fill = !!sym(champs)), inherit.aes = FALSE, alpha = transparence,color = "#4F4F4F")
  }


  # ajout des etiquettes de donnees
  if (!is.null(etiquettes))
  {

    gg<- gg +
      geom_label(data=nc_3857,aes(label= !!sym(etiquettes),
                                  x=st_coordinates(st_centroid(nc_3857))[,1],
                                  y=st_coordinates(st_centroid(nc_3857))[,2]),size=4)
  }

  # plot la carte
  gg

  return(gg)
  #scale_fill_gradient(low = "green", high = "red")
  #scale_fill_continuous  (low = "green", high = "red",guide = FALSE)  +
  #theme_void()  + ggtitle(titre)+


}
