#' data_summary_2var
#'return(res)
#' @param data un data frame, avec 2 colonnes qui contiennent les valeurs à comparer, ainsi qu'un nom de champs pour grouper les comparaisons
#' @param varname1 as character : nom du premier champs
#' @param varname2 as character: nom du second champs
#' @param groupnames as character : nom du champs qui comporte le groupe.
#' Pour ne pas utiliser de groupes : groupnames=NULL
#'
#' @description  sert à carculer des stats sur des séries de valeurs
#' data_summary_2 : format : un data frame, avec 2 colonnes qui contiennent les valeurs à comparer,
#' et une autre colonne qui contient le nom des groupes (ex : mois, ...)
#' @return
#' @export
#'
#' @examples
#' df<-data.frame(x=1:20,y1=rnorm(20),y2=rnorm(20)*10,groupe=rep(c("a","b","c","d"),5))
#' data_summary_2var(df,varname1="y1",varname2="y2",groupnames="groupe")
#' #library(openair) # aussi possible de cette maniere
#' #modStats(df,"y1","y2")
data_summary_2var <- function(data, varname1,varname2, groupnames){
  require(plyr)
  require(Metrics)


  NMSE<- function(mdataici,nmod,nmes)
  {
    m<-mdataici[mdataici[,nmes]!=0 & mdataici[,nmod]!=0 ,]
    m[,nmod]<-round(m[,nmod],0)
    m<-m[m[,nmod]!=0 ,]
    res=NULL
    res<-(m[,nmod]-m[,nmes])^2
    res<-mean(res/(m[,nmod]*m[,nmes]),na.rm=T)
  }

  summary_func <- function(x, varname1,varname2){

    reglin<-lm(x[,varname1]~x[,varname2])


    R2=summary(reglin)$r.squared
    pvalue=as.vector(pf(summary(reglin)$fstatistic[1],summary(reglin)$fstatistic[2],summary(reglin)$fstatistic[3],lower.tail=FALSE))
    score <- if (pvalue<=0.001) "***" else
      if (pvalue<0.01) "**"  else
        if (pvalue<0.05) "*"  else
          if (pvalue<0.1) ":|"  else ":( "
    c(
      cor = cor(na.omit(x[,c(varname1,varname2)]))[1,2],
      rmse = rmse(x[,varname1],x[,varname2]),
      nmse= NMSE(x,varname1,varname2),
      rae=rae(x[,varname1],x[,varname2]),
      bias=bias(x[,varname1],x[,varname2]),
      compte=dim(x)[1],
      reglin_interc=as.numeric(reglin$coefficients[1]),
      reglin_pente=as.numeric(reglin$coefficients[2]),
      reglin_R2=R2,
      reglin_pvalue=pvalue,
      reglin_score=score)

  }

  if (is.null(groupnames))
  {
    data<-na.omit(data[,c(varname1,varname2)])
    data_sum<-summary_func(data,varname1,varname2)
  } else {

    data<-na.omit(data[,c(varname1,varname2,groupnames)])
    # pretraitement pour retirer les sous-groupe a 0 qui font planter la fonction
    data_agg<-aggregate(data[,varname1],list(data[,groupnames]),sum,na.rm=T)
    data_agg2<-aggregate(data[,varname2],list(data[,groupnames]),sum,na.rm=T)
    retirer<-c(data_agg[data_agg[,2]==0,1],data_agg2[data_agg2[,2]==0,1])
    data<-data[!is.element(data[,groupnames],retirer),]
    # pretraitement pour retirer les sous-groupe de taille<3 qui font planter la fonction
    data_agg<-aggregate(data[,varname1],list(data[,groupnames]),length)
    data_agg2<-aggregate(data[,varname2],list(data[,groupnames]),length)
    retirer<-c(data_agg[data_agg[,2]<3,1],data_agg2[data_agg2[,2]<3,1])
    data<-data[!is.element(data[,groupnames],retirer),]
    # lancement de la fonction
    data_sum<-ddply(data,groupnames , .fun=summary_func,
                    varname1,varname2)
  }
  #data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}
