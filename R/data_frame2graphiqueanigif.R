

#' data_frame2graphiqueanigif
#'
#' @param df as dataframe au format 3 colonnes : x, y, groupe . x est soit numerique, soit date. Seule la position du champs compte, pas son nom.
#' @param vitesse as  float : speed of the anigif
#' @param outdos dossier de sortie
#' @param titre title to write above the anigif
#' @param xylabs as a vector of two string for respectively xlab and ylab. # titre des axes x et y, si non renseignes dasn l'appel de fonction, ce sera le titre des colonnes
#' @param vide_temp_avant delete all files in temp dir before.   vide le dossier si il existe deja et si vide_temp_avant=T
#' @param size a vector of 2 integer for the out size of the anigif
#'
#' @description Permet de creer un graphique anime où la courbe des valeurs se trace au fur et a mesure. Attention, ne fonctionne que si ImageMagick (logiciel) est installe sur le poste.
#' @return
#' @export
#'
#' @examples
#' df<-data.frame(x=1:20,y=rnorm(20),groupe=rep(c("a","b"),10))
#' data_frame2graphiqueanigif(df,outdos="C:/TEMP/",vide_temp_avant=TRUE)
data_frame2graphiqueanigif<-function(df,vitesse=10,outdos=NULL,titre="",xylabs=NULL,vide_temp_avant=TRUE,size=c(700,300))
{

  # df : au format 3 colonnes : x, y, group.
  # x est soit numerique, soit date
  library(ggplot2)

  # titre des axes x et y, si non renseignes dasn l'appel de fonction, ce sera le titre des colonnes
  xylabs<- if(is.null(xylabs)) c(names(df)[1],names(df)[2]) else xylabs

  # dossier de sortie de l'anigif
  if(is.null(outdos)) {outdos="."
  nomdos<-getwd()
  } else  { 	nomdos<-outdos }


  # limite basse et haute de l'axe y du graphique
  ylim<-c(min(df[,2],na.rm=T),max(df[,2],na.rm=T))

  # la boucle va parcourir les valeurs dans l'ordre de l'axe des x
  xval<-sort(unique(df[,1]))
  ngraphique<-length(xval)

  #cree le dossier qui va contenir les png si il n'existe pas deja
  if (!file.exists(paste0(outdos,"//temp"))) dir.create(paste0(outdos,"//temp"))

  # le vide si il existe deja et si vide_temp_avant=T
  if (vide_temp_avant)
  {
      if (length(list.files(paste0(nomdos,"/temp/"),pattern=".png"))>0)     file.remove(paste0(nomdos,"/temp/",list.files(paste0(nomdos,"/temp/"),pattern=".png")))
  }
  df_g<-df

  for (x in 1:ngraphique)
  {
    # courbes lineaires
    maxx<-xval[x]
    df_g<-df
    df_g[df_g[,1]>maxx,2]<-NA

    png (file=paste0(outdos,"//temp//graphe_temp",as.numeric(Sys.time())*100000,".png"), width=size[1], heigh=size[2])


    p <- ggplot(df_g, aes(x=df_g[,1], y=df_g[,2], group = as.factor(df_g[,3])), environment = environment())
    print(
      p + geom_point(aes( color = df_g[,3]),size=2, alpha = 0.5) +
        geom_line(aes( color = df_g[,3])) +
        theme(legend.position="bottom") +
        ylab(xylabs[2]) +
        xlab(xylabs[1]) +
        ggtitle(titre) +
        labs(color = names(df_g)[3],title=titre) +
        ylim(ylim) +
        theme_minimal()
    )

    dev.off()
  }


  # convert pngs to one gif using ImageMagick
  system(paste0("magick -verbose -delay ",vitesse," ",outdos,"/temp/*.png ",outdos,"/anigif.gif"))
  # cleaning up
  file.remove(paste0(outdos,"//temp//",list.files("temp//",pattern=".png")))


  if (file.exists("anigif.gif")) print(paste0("Fichier 'anigif.gif' cree dans le dossier : ",nomdos))

}
