
################################################################################################

################################################################################################


#' data_geodair
#'
#' @param polluant as text :selon les codes iso de geodair ("03" : NO2, "10": CO, "24" : PM10,"T1":Nb PM 7nm-1µm, "T3" : Nb PM 10nm-1µm...). un fichier contenant les codes iso est sur le reseau atmona partenariat/outils/geodair
#' @param date_d as text : date de debut au format YYYY-MM-DD
#' @param date_f as text : date de debut au format YYYY-MM-DD
#' @param stat as text : "MoyH" : moyenne horaire, "MoyA": myenne annuelle , "MoyJ" moyenne jour,"MaxJH" max horaire journalier
#' @param codesite as text : le code du site a conserver au format GEODAIR
#' @param aasqa as text "ATMO SUD", "ATMO OCCITANIE" "ATMO NOUVELLE-AQUITAINE" "ATMO AUVERGNE-RHÔNE-ALPES". Pour toute la France : aasqa=NULL
#'
#' @description : la fonction permet de recuperer plusieurs jours de donnees pour un polluant a partir de GEODAIR publique.Elle utilise la fonction get_data_pub() qui fait fonctionner l'API pour un jour un polluant.
#' L'API telecharge les valeurs pour tous les sites de France. Les parametres "codesite" et "aasqa" permettent de filtrer le resultat telecharge, mais toutes les donnees
#' du territoire seront quand meme telechargees (pas le choix). Attention : le nombre de requete (jour) par heure calendaire est limite (ex : 3000 jours /heure)
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' library(AnisTools)
#' vals<-data_geodair(polluant="T3",date_d="2024-11-01",date_f="2024-12-17",aasqa=NULL)
#' vals<-NULL
#' vals<-data_geodair(polluant="T3",date_d="2023-01-01",date_f="2023-01-02")
#' }
data_geodair<-function(polluant,date_d,date_f,stat="MoyH",codesite=NULL,aasqa=NULL)
{
  if (as.Date(date_d)>as.Date(date_f)){stop("La date de debut est superieur a la date de fin")}
  s<-as.character(seq(as.Date(date_d),as.Date(date_f),1))
  data<-NULL
  pb = txtProgressBar(min = 0, max = length(s), initial = 0, style = 3)
  n=0
  for (d in s)
  {
    n<-n+1
    # initialisation de la progress bar
    #setTxtProgressBar(pb,n)
    print(d)
    dd<-NULL
    comptage<-1
    while(is.null(dd)&comptage<7)
    {
      print(paste("Tentative #",comptage))
      dd<-get_data_pub(polluant,d)
      if (is.null(dd)) Sys.sleep(3)
      comptage=comptage+1
    }

    if (!is.null(dd))
    {
      if (!is.null(aasqa))
      {
        dd<-subset(dd,organisme==aasqa)
      }
      if (!is.null(codesite))
      {
        dd<-subset(dd,code_site==codesite)
      }
      data<-rbind(data,dd)
    } else {print(paste0("la date ",d," n'a pas pu etre telechargee"))}
  } # fin de for d in s
  close(pb)
  return(data)


} # fin de la fonction



################################################################################################

################################################################################################





#' Connexion a l'API publique de GEOD'AIR pour le telechargement des donnees
#'
#'
#' @param polluant as text :selon les codes iso de geodair ("03" : NO2, "10": CO, "24" : PM10,...)
#' @param date_d as text : date de debut au format YYYY-MM-DD
#' @param date_f as text : date de debut au format YYYY-MM-DD
#' @param stat as text : "MoyH" : moyenne horaire, "MoyA": myenne annuelle , "MoyJ" moyenne jour,"MaxJH" max horaire journalier
#' @param user_pub : id (et pas mail) pour l'espace public de geodair. (Il semblerait que le mauvais nom de user n'impacte pas la requete).
#' @param pwd_pub : pwd pour l'espace public de geodair
#' @param api_key_pub : l'api de lespace public, qui est cree une fois pour toute ici : https://www.geodair.fr/donnees/api
#'
#' @description fait fonctionner l'API GEODAIR publique. la fonction ne permet de telecharger qu'une seule journee. Pour plusieures journées, utiliser data_geodair()
#'
#' @return un objet de classe S3 geodair
#' @import httr
#' @export
#' @encoding UTF-8
#'
#' @examples
#' \dontrun{
#'data<-NULL
#'data<-get_data_pub(polluant="24",date="2024-11-23")
#'}
#'
get_data_pub<-function(polluant,date,stat="MoyH",user_pub=NULL,pwd_pub=NULL,api_key_pub=NULL)
{
  require(httr)
  require(jsonlite)
  require(data.table)
  require(janitor)

  if (is.null(api_key_pub))     api_key_pub<-get_ids_geodair()$apikey_geodair
  if (is.null(user_pub))     user_pub<-get_ids_geodair()$user_geodair
  if (is.null(pwd_pub))     pwd_pub<-get_ids_geodair()$mdp_geodair



  # acces a la partie publique de l'API

  # etape 1 : creer la demande
  #https://www.geodair.fr/api-ext/MoyH/export-avance?polluant=03&date=2022-08-08
  base_pub <-paste0("https://www.geodair.fr/api-ext/",stat,"/export-avance?polluant=",polluant,"&date=",date)
  statu<-0
  n_tentative<-0
  print(paste0('Getting data for user :',user_pub))
  while(statu!=200&n_tentative<50)
  {
    get_nom_fich <- try(httr::GET(base_pub, authenticate(user_pub,pwd_pub),
                                 add_headers("apikey"= api_key_pub)))
    statu<-get_nom_fich$status_code # si statu=200 alors c'est ok
    n_tentative<-n_tentative+1
    if (grepl("maintenance",get_nom_fich)) {stop("Le site GEODAIR est en maintenance")}
    # proposition avec trycatch, marche mais n'apporte rien
    #tryCatch({
    #  get_nom_fich <- httr::GET(base_pub, authenticate(user_pub, pwd_pub), add_headers("apikey" = api_key_pub))
    #  statu <- get_nom_fich$status_code
    #}, error = function(e) {
    #  cat("Error in GET request:", conditionMessage(e), "\n")
    #  statu <- 0
    #})
    #n_tentative <- n_tentative + 1

  }
  nom_fich_pub<-NULL
  nom_fich_pub<-httr::content(get_nom_fich,"text", encoding = "UTF-8")
  print(nom_fich_pub)

  # etape 2 : recuperer le resultat
  base2_pub<-paste0( "https://www.geodair.fr/api-ext/download?id=",nom_fich_pub)
  Sys.sleep(0.5)
  statu2<-0
  n_tentative<-0
  while(statu2!=200&n_tentative<100)
  {
    n_tentative=0
    statu=0
    get_data <-NULL
    while(statu!=200&n_tentative<100)
    {


      get_data <-tryCatch(httr::GET(base2_pub, authenticate(user_pub,pwd_pub),
                         add_headers("apikey"= api_key_pub,.headers = c("Content-Type"="application/json"))),
               message = function(c) {
                 message("x est numerique et j'ai pu modifier mon message")
                 suppressMessages(my_fun(val))
               },
               warning = function(c) {
                 message("x est une chaine de caracteres et j'ai pu modifier mon message")
                 paste0("ma nouvelle valeur : ", val)
               },
               error = function(c) {
                 return(NULL)
               },
               finally = print("tout s'est bien passé"))

      #get_data <- try(httr::GET(base2_pub, authenticate(user_pub,pwd_pub),
       #                         add_headers("apikey"= api_key_pub,.headers = c("Content-Type"="application/json"))))
      # suggestion : utiliser tryCatch à la place
      if (!is.null(get_data)) statu<-get_data$status_code
      n_tentative<-n_tentative+1

      #print(statu)
      #print(paste0("Tentative numero :",n_tentative))
    }
    if (n_tentative==100) {stop("Echec, code statu :",statu)}

    # Lecture du contenu sous la forme d'un dataframe en cas de succes
    res<-NULL
    res<-httr::content(get_data, "text", encoding = "UTF-8")

    str(get_data$headers)

    if (grepl("Dat",substr(res,1,4))) {statu2<-200} else {
      res_res<-parse_json(res)
      statu2<-res_res$status
      #print(paste("erreur :",statu2))
      stop(statu2)
    }
  }
  f <- data.table::fread(text =res , encoding = "UTF-8")
  f <- janitor::clean_names(f)
  f<-data.frame(f)
  # Nettoyage des noms de colonnes via le pkg {janitor}

  f$date_de_debut <- strptime(f$date_de_debut,"%Y/%m/%d %H:%M:%S",tz="GMT")
  f$date_de_fin<- strptime(f$date_de_fin,"%Y/%m/%d %H:%M:%S",tz="GMT")
  return(f)
}















################################################################################################

################################################################################################


#' pour_memoire_get_token - dont work
#'
#' @return
#' @export
#'
#' @examples
pour_memoire_get_token<-function()
{
  base_t<-"https://GeodairClient:f3b1fe41730f2355afd30af7cf10d7d2@www.geodair.fr/priv/api/oauth/token"
  #base_t<-"https://www.geodair.fr/priv/api/oauth/token"

  get_token <-httr::POST(base_t, body=list(username = user, password = pwd, grant_type = "password"), encode = "form")
  obj[["access_token"]] <- content(get_token)$access_token


  # Connexion a l'API Geodair pour generation d'un token
  conn <- httr::POST(url = base_t, body =list(username = user,
                                              password = pwd,
                                              grant_type = "password"), encode = "form")


  # Connexion a l'API Geodair pour generation d'un token
  conn <- httr::POST(url = base_t, body = obj$parametres, encode = "form")

}
