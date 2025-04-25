#' data_summary_stat
#'
#' @param data as dataframe. 3 colonnes : une colonne d'identifiant qui s'appelle obligatoirement "ID". Une colonne de valeurs, une colonne avec les groupes.
#' @param varname le nom de la variable (du champs) du data frame
#' @param groupnames le nom du champs du data frame qui contient les groupes
#'
#' @description : permet de dire quel ID correspond au min et au max d'une variable donnee dans chaque groupe.
#' @return dataframe
#' @export
#'
#' @examples
#' df<-data.frame(ID=1:20,y=rnorm(20),groupe=rep(c("a","b","c","d"),5))
#' data_summary_stat(df,"y","groupe")

data_summary_stat <- function(data, varname, groupnames){
  library(plyr)
  library(Metrics)
  summary_func <- function(x, col){
    c(max = x[which(x[[col]]==max(x[[col]],na.rm=T)),"ID"],
      min = x[which(x[[col]]==min(x[[col]],na.rm=T)),"ID"]
    )
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,  varname)
  data_sum <- rename(data_sum, c("max" = paste("max",varname,sep="_")))
  data_sum <- rename(data_sum, c("min" = paste("min",varname,sep="_")))

  return(data_sum)
}
