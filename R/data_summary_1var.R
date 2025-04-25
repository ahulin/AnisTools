#Auteur : Agnès Hulin
#date de création : 10/02/2021
# sert à carculer des stats sur des séries de valeurs

#--------------Fonctions complémentaires à charger au préalable----------------------



#' data_summary_1var
#'
#' @param data dataframe
#' @param varname as character
#' @param groupnames as character
#'
#' @return
#' @export
#'
#' @examples
#' df<-data.frame(x=1:20,y=rnorm(20),groupe=rep(c("a","b","c","d"),5))
#' data_summary_1var(df,"y","groupe")
data_summary_1var <- function(data, varname, groupnames){
library(plyr)
library(Metrics)

    summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE),
      min = min(x[[col]], na.rm=TRUE),
      max = max(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,  varname)
  data_sum <- rename(data_sum, c("mean" = paste0(varname,"_mean")))
  data_sum <- rename(data_sum, c("sd" = paste0(varname,"_sd")))
  data_sum <- rename(data_sum, c("min" = paste0(varname,"_min")))
  data_sum <- rename(data_sum, c("max" = paste0(varname,"_max")))
  return(data_sum)
}








