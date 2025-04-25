

#' create_grille_poly
#'
#' @param xmin as numeric
#' @param xmax as numeric
#' @param ymin as numeric
#' @param ymax as numeric
#' @param n_ligne as integer
#' @param n_col as integer
#' @param res as integer : resolution dela grille (dans l'unite de travail)
#' @param poly a shapefile, sf format poly<-sf::st_read(fich_poly,quiet=TRUE)
#' @param buffer l'extension a rajouter autour des valeurs limites ou du shape donnees (dans l'unite de travail)
#' @param type_grille as text : square ou hexagonal
#' @description permet de creer une grille reguliere. Plusieures possibilite : soit on rentre les xmin/xmax ymin/ymax, soit on rentre un polygone (sp) dasn poly,
#' soit on laisse les 3 parametres nul et alors la fonction va ouvrir une boite de dialogue pour demander de choisir un shapefile.
#' Pour la resolution de la grille : soit on rentre nligne et ncol, soit on rentre la resolution. Si les trois parametres sont rentres, la fonction ne prendra que la resolution.
#' On peut rajouter un buffer, dans ce cas un contor sera pris d'autant autour du shapefile ou des coordonees xy
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' g<-create_grille_poly(xmin=368880,xmax=410068,ymin=6554456,ymax=6582756,n_ligne=100,n_col=100)
#' g<-create_grille_poly(poly=fr,res=1,type_grille="HEXAGONAL") # fr est un shapefile au format sf
#' plot(g)
#' g2<-create_grille_poly(poly=fr,nligne=10,ncol=10)
#' g2<-create_grille_poly(poly=fr,res=1,buffer=2)
#' }
create_grille_poly<-function(xmin=NULL,xmax=NULL,ymin=NULL,ymax=NULL,n_ligne=NULL,n_col=NULL,res=NULL,poly=NULL,buffer=0,type_grille="square")
{
  library(sf)

  projection<-NA
  if (!is.null(poly)) {
    box<-extent(poly)
    xmin<-box[1]
    xmax<-box[2]
    ymin<-box[3]
    ymax<-box[4]
    projection<-crs(poly)

  }

  # on applique le buffer
  xmin<-xmin-buffer
  ymax<-ymax+buffer
  ymin<-ymin-buffer
  ymax<-ymax+buffer


  if (ymax<=ymin|xmax<=xmin) stop("les coordonees ne sont pas correctes (xmin>max ou ymin>ymax) ")

  # si la resolution est indiquee elle prevaut sur n_ligne et n_col
  if (!is.null(res))
  {
    if (!is.null(n_ligne)|!is.null(n_col)) print("Le nombre de ligne/colonne a ete specifie en plus de la resolution. Seule la resolution sera prise en compte. Pour forcer le nombre de ligne/colonne, mettre res=NULL")
    n_ligne<-ceiling((ymax-ymin)/res)
    n_col<-ceiling((xmax-xmin)/res)
    ymax<-ymin+res*n_ligne
    xmax<-xmin+res*n_col
  }


  # cree un polygone fictif qui couvre le domaine + buffer
  outer = matrix(c(xmin,ymin,xmax,ymin,xmax,ymax,xmin,ymax,xmin,ymin),ncol=2, byrow=TRUE)
  domaine<-st_polygon(list(outer))

  sq<-TRUE
  if (toupper(type_grille)=="HEXAGONAL"|toupper(type_grille)=="HEXAGONALE") sq=FALSE

  # fait la grille
  gr<-st_make_grid(
    domaine,
  n = c(n_col, n_ligne), # ncol, nligne
  what = "polygons",
  square = sq
 )

if (!is.na(projection))    st_crs(gr)<-projection
return(gr)
}

