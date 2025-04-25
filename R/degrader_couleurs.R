#' degrader_couleurs
#'
#' @param coul colors to interpolate; must be a valid argument to col2rgb(). par exemple : c("red", "green")
#' @param seuilmini : minima de l'échelle
#' @param seuilmaxi : maxima de l'échelle
#' @param range : nombre de classes
#'
#' @return
#' @export
#'
#' @description : permet de creer une echelle de couleur en dégradé avec le nombre de classes demandées a partie des couleurs choisies.
#'
#' @examples
#' degrader_couleurs(c("red", "green"),0,10,4)
#' degrader_couleurs(coul=c("red", "green","purple"),0,10,4)
degrader_couleurs<-function(coul,seuilmini,seuilmaxi,range=4)
{
  library(grDevices)
  coulfin<-colorRampPalette(coul)( (length(coul))*range)
  #require(graphics)
  #length(coulfin)

  seuil<-c(seuilmini,max(seuilmaxi))
  stemp<-NULL
  for (i in 1:(length(seuil)-1))
  {
    #s<-seq(seuil[i],seuil[i+1],by =ceiling((seuil[i+1]-seuil[i])/range))
    s<-round(seq(seuil[i],seuil[i+1],length.out=range+1),1)
    s<-s[-length(s)]
    if (is.null(stemp)){stemp<-s} else {stemp<-c(stemp,s)}

  }
  stemp<-c(stemp,max(seuilmaxi))
  stemp<-sort(stemp)
  smin<-stemp[1:(length(stemp)-1)]
  smax<-stemp[2:(length(stemp))]

  echelle_fin<-data.frame(code=coulfin, seuilmin=smin, seuilmax=smax)
  return(echelle_fin)

  plot(echelle_fin$seuilmin,col=echelle_fin$code,pch=19)

}



