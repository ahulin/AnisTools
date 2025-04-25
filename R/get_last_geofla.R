
#' get_last_geofla
#'
#' @param echelle "commune"  "departement" "canton" "arrondissement" ou "tout" "epci" "region" "chef_lieu".
#' @param dos_sortie
#' @param echelle
#'
#' @return
#' @export
#' @description la fonction permet de télécharger les couches sig  administratives cgeofla. Certains couches sont téléchargées individuellement
#' en lamb93 (commune, departement,canton arrondissement) puis converties en wgs84. Les autres proviennent du pack complet, où seules les couches
#' souhaitées sont extraites en wgs84. Si "tout" est demandé, c'est le pack
#' complet en wgs84 qui est extrait (avec plein de couches inutiles).
#'
#' @examples
#' \dontrun{
#' get_last_geofla(echelle="region")
#' get_last_geofla(echelle="commune",dos_sortie="C://TEMP")
#' }
get_last_geofla<-function(echelle,dos_sortie=NULL)
{


library(sp)
require(curl)
require(RCurl)
require(archive)
#library('rgdal')



echelle<-toupper(echelle)
echelle_save  <-echelle

if (is.element(echelle,c("TOUT","EPCI","REGION" ))) {
    echelle<-"ADMIN-EXPRESS-COG"
    syst<-"wgs84"} else {syst<-"lamb93"}

if (is.null(dos_sortie))
{
  dos_sortie_racine<-"K:/Partenariat_Innovation/SIG/France/GEOFLA/"
}

print("Etape 1 : telecharchement de l'archive sur Admin_Express")

adresse_geofla<-"ftp://Admin_Express_ext:Dahnoh0eigheeFok@ftp3.ign.fr/"

result<-NULL
result2 <- NULL

# liste totale des fichiers zippé geofla
result <- getURL(adresse_geofla,verbose=TRUE,ftp.use.epsv=FALSE, dirlistonly = TRUE, crlf = TRUE)

if (result=="") {stop(paste0("Adresse vide : ",adresse_geofla))}
result2 <-  strsplit(result, "\r*\n")[[1]]

# on cherche le fichier souhaité le plus récent
result2<-result2[grep(paste0(echelle),result2)]

if (length(result2)==0) {stop("Aucun fichiers trouvés avec l'échelle demandee")}

if (is.element(echelle,c("COMMUNE","DEPARTEMENT","CANTON","ARRONDISSEMENT")))
{
  result2<-result2[grep("SHP_LAMB93_FXX",result2)]
  date<-max(substr(sapply(strsplit(result2, "_"), `[`, 7),1,10))
} else
{
  result2<-result2[grep("SHP__FRA_WGS84",result2)]
  date<-max(substr(sapply(strsplit(result2, "_"), `[`, 8),1,10))
}



result2<-result2[grep(date,result2)]
result2<-result2[which(unlist(lapply(result2, nchar))==min(unlist(lapply(result2, nchar))) )]


#dossier de sortie
dos_sortie<-file.path(dos_sortie_racine,echelle_save,date)

if (!dir.exists(dos_sortie)) {dir.create(dos_sortie,recursive=TRUE)}

# on construit l'URL de téléchargement et on télécharge
geofla_com_url<-paste0("ftp://Admin_Express_ext:Dahnoh0eigheeFok@ftp3.ign.fr/", result2)
tf <- file.path(dos_sortie,result2)
download.file( geofla_com_url , tf , mode = "wb" )


#---------------
print("Etape 2 : Extraction des donnees  de l'archive ")

#on fait l'extraction, mais uniquement des fichiers souhaites
# on commence par lister les fichiers de l'archive
d<-archive(tf)
data<-grep(paste0("/",echelle_save,"."),data.frame(d)[,1])

# on retire les fichiers associés "LIMITE" qui donnent les contours
limites<-grep("LIMITE",data.frame(d)[,1])
if (length(limites)>0) { data<-data[-which(is.element(data,limites))] }

a <- system.file(package = "archive", "extdata", tf)
dout <- dos_sortie

if (!dir.exists(file.path(dout,syst)) ) {dir.create(file.path(dout,syst))}

# When called with default arguments extracts all files in the archive.
nb_arbo<- length(strsplit(as.character(d[data[1],]),"/")[[1]]) -1
if (nb_arbo==4)
{
  archive_extract(tf,files =data,file.path(dout,syst),strip_components = 4L)
  } else if (nb_arbo==5) {
  archive_extract(tf,files =data,file.path(dout,syst),strip_components = 5L)
  } else if (nb_arbo==6) {
    archive_extract(tf,files =data,file.path(dout,syst),strip_components = 6L)
  }else if (nb_arbo==3) {
    archive_extract(tf,files =data,file.path(dout,syst),strip_components = 3L)
  }


if (length(list.files(file.path(dout,syst)))>0) {unlink(tf, recursive = FALSE, force = FALSE)}

#si les shape sont les communes, cantons, arrondissements, département : ils sont en lambert93, et on les converti en wgs84
if (syst=="lamb93")
{
  print("Etape 3 : Conversion en wgs84 (nb : si echelle='tout', pas besoin de conversion) ")

    if (!dir.exists(file.path(dout,"wgs84" )) )
    {
      dir.create(file.path(dout,"wgs84" ))
    }
    # on charge le shapefile
    library('rgdal')
    list.files(file.path(dout,"lamb93"),pattern="shp")->fichsl93

    for (f in fichsl93)
    {
      if (!file.exists(file.path(dout,"wgs84",f)))
      {
          cl93=readOGR(file.path(dout,"lamb93",f) ,stringsAsFactors=FALSE)
          clwgs84<-spTransform(cl93, "+proj=longlat +ellps=WGS84")
          writeOGR(clwgs84, file.path(dout,"wgs84"), f, driver="ESRI Shapefile")
      }
    }


}

}
