
#' get_google_trends
#'
#' @param mot as text. Peut être un vecteur de texte. si deux mots: la recherche doit contenir les deux mots dans n'importe quel ordre.
#' "+" devant un mot équivaut a "ou". '' pour avoir l'expressioin exacte
#' pour acoir les trends deplusieurs mots : c("mot1","mot2")
#' @param periode  A string specifying the time span of the query. Possible values are:
#'   "now 1-H" Last hour
#' "now 4-H" Last four hours
#' "now 1-d" Last day
#' "now 7-d" Last seven days
#' "today 1-m" Past 30 days
#' "today 3-m" Past 90 days
#' "today 12-m" Past 12 months
#' "today+5-y" Last five years (default)
#' "all" Since the beginning of Google Trends (2004)
#' "Y-m-d Y-m-d" Time span between two dates (ex.: "2010-01-01 2010-04-03")
#' @param zone "FR" pour a france ou pour NA c("FR-T","FR-L","FR-B") . utiliser la variables countries pour avoir d'autres zones
#' @param langue as text : langue de recherche
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' res<-NULL
#' res <-get_google_trends(mot = "pollution air",zone = c("FR"), langue = "fr",periode = "today+5-y")
#' res <-get_google_trends(mot = "pollen + allergie ",zone = c("FR-T","FR-B"), langue = "fr",periode = "2022-01-01 2022-12-31")
#' res <-get_google_trends(mot = c("particule","pollen","NOX","ozone"),zone = c("FR-B"), langue = "fr",periode = "2022-01-01 2022-12-31")
#' }
get_google_trends<-function(mot,periode,zone="FR",langue="fr")
{

library("gtrendsR")
#categories
#countries
fr<-subset(countries,country_code=="FR")
#pc<-"FR-T"
#lim<-"FR-L"
#aqu<-  FR-B"

res<-gtrends(
 keyword = mot,
   geo = zone,
  time =  periode,
  gprop = c("web", "news", "images", "froogle", "youtube"),
  category = 0,
  hl = langue,
  compared_breakdown = FALSE,
  low_search_volume = TRUE,
  cookie_url = "http://trends.google.com/Cookies/NID",
  onlyInterest = FALSE

)


plot(res)
return(res)
}
