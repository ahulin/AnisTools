


#' get_api_ggmap
#'
#' @return
#' @export
#'
#' @examples
#'  \dontrun{
#' get_api_ggmap()
#' }
get_api_ggmap<-function()
{
  library(rjson)

  # emplacement du fichier json access
  home<-file.path(Sys.getenv("HOME"),"R","access")
  access_file<-file.path(home,"access.json")

  if (!file.exists(access_file))    create_access_api_ggmap()

  existing_access_data<- rjson::fromJSON(file = access_file)
  if (!is.element("api_ggmap",names(existing_access_data)))  create_access_api_ggmap()

  return(existing_access_data$api_ggmap)

}

##########################################################################
##########################################################################



#' get_ids_geodair
#'
#' @return
#' @export
#'
#' @examples
#'  \dontrun{
#' get_ids_geodair()
#' }
get_ids_geodair<-function()
{
  library(rjson)

  # emplacement du fichier json access
  home<-file.path(Sys.getenv("HOME"),"R","access")
  access_file<-file.path(home,"access.json")

  if (!file.exists(access_file))  {  create_access_geodair() }

  existing_access_data<- rjson::fromJSON(file = access_file)
  if (!is.element("identifiants_geodair",names(existing_access_data)))  create_access_geodair()

  return(existing_access_data$identifiants_geodair)

}


##########################################################################
##########################################################################



#' create_access_geodair
#'
#' @return
#' @export
#'
#' @examples
#'  \dontrun{
#' create_access_geodair()
#' }
create_access_geodair<-function()
{
  #api_key<-h7tx3m3FDqYsJVU8jr4nBHSZyZSP9gqa
  #id<-"ahulin"
  library(rjson)

  # emplacement du fichier json access
  home<-file.path(Sys.getenv("HOME"),"R","access")
  if (!dir.exists(home)) {dir.create(home,recursive=TRUE)}
  access_file<-file.path(home,"access.json")

  if (file.exists(access_file))
  {
    existing_access_data<- rjson::fromJSON(file = access_file)
  } else
  {
    existing_access_data=NULL

  }

  user_geodair<-""
  mdp_geodair<-""
  apikey_geodair<-""


  # 1. renseigner les identifiants geodair
  reponse<-"Y" # par defaut les valeurs sont a modifier, sauf si l'utilisateur ne le souhaite pas a la question ci dessous
  # demande a l'utilisateur si il souhaite modifier les valeurs
  if (!is.null(existing_access_data$identifiants_geodair))
  {
    print(existing_access_data$identifiants_geodair)
    reponse<-readline(prompt=paste0("les identifiants GEODAIR existent deja. Do you want to overwrite them ? (y/n) : ",collapse=" "))
  }

  if (toupper(reponse)=="Y") # nb : Y par defaut si les valeurs n'existent pas deja
  {
    # demande a l'utilisateur les valeurs ? entrer

    while(user_geodair=="" )  user_geodair<- readline(prompt="Enter user for geodair : ")
    while(mdp_geodair=="" )  mdp_geodair<- readline(prompt="Enter password for geodair  : ")
    while(apikey_geodair=="" )  apikey_geodair<- readline(prompt="Enter api_key for geodair  : ")
  } else
  {
    user_geodair<-existing_access_data$identifiants_geodair$user_geodair
    mdp_geodair<-existing_access_data$identifiants_geodair$mdp_geodair
    apikey_geodair<-existing_access_data$identifiants_geodair$apikey_geodair
  }
  identifiants_geodair=list(user_geodair=user_geodair,mdp_geodair=mdp_geodair,apikey_geodair=apikey_geodair)



  if (is.null(existing_access_data))
  {
    access_data<-list(identifiants_geodair=identifiants_geodair)

  } else {
    access_data<-existing_access_data

    access_data$identifiants_geodair<-identifiants_geodair
  }


  jsonData <- toJSON(access_data)



  #write JSON object to file
  write(jsonData, access_file)

  if (file.exists(access_file)) print(paste0("Le fichier d'acces a corectement ete cree/modife : ",access_file))
}



##########################################################################
##########################################################################


create_access_bdd<-function(nom_connection) ### en cours de construction
{
  library(rjson)

  # emplacement du fichier json access
  home<-file.path(Sys.getenv("HOME"),"R","access")
  if (!dir.exists(home)) {dir.create(home,recursive=TRUE)}
  access_file<-file.path(home,"access_bdd.json")


  if (file.exists(access_file))
  {
    existing_access_data<- rjson::fromJSON(file = access_file)
  } else
  {
    existing_access_data=NULL
  }

  reponse<-"n"
  n_con<-NULL
  for (n in 1:length(existing_access_data))
  {
    nom_con_n<-existing_access_data[[n]]$nom_connection
    if (nom_con_n==nom_connection)
    {
      print(unlist(existing_access_data[[n]]))
      print("La connection '",nom_connection,"' existe deja, la remplacer?")
      reponse<-readline(prompt=paste0("*** La connection '",nom_connection,"' existe deja, la remplacer? (y/n) : ",collapse=" "))
      if (reponse=="n") { print("La connection n'a pas ete modifiee");return(NULL)}
    }
    if (reponse=="y") {n_con<-n;break}
  }
  if (reponse=="n")
  {
    print(paste0("Liste des connections existantes :"))
    for (n in 1:length(existing_access_data)) print(existing_access_data[[n]]$nom_connection)
    reponse<-readline(prompt=paste0("*** La connection  '",nom_connection,"' n'existe pas encore, la creer?. (y/n) : ",collapse=" "))
    if (reponse=="n") { print("La connection n'a pas ete cree.");return(NULL)}

  }
  if (reponse=="y")
  {
    # creation/modification de la connection dans le json
    if (is.null(n_con)) n_con<-length(existing_access_data)+1
    existing_access_data[[4]]<-"Nouvelle connection"
    existing_access_data[[n_con]]$nom_connection<-nom_connection
    ip<-readline(prompt=paste0("Adresse IP:"))
    existing_access_data[[n_con]]$ip<-ip
    id<-readline(prompt=paste0("Identifiant utilisateur:"))
    existing_access_data[[n_con]]$user<-id
    mdp<-readline(prompt=paste0("Mot de passe:"))
    existing_access_data[[n_con]]$mdp<-mdp
    bdd<-readline(prompt=paste0("Nom de la base de donnee (Touche Entree pour laisser NULL):"))
    existing_access_data[[n_con]]$bdd<-bdd
    port<-readline(prompt=paste0("Port: (5432 par défaut) (Touche Entree pour laisser NULL):"))
    existing_access_data[[n_con]]$port<-port
    schema<-readline(prompt=paste0("Schema: (Touche Entree pour laisser NULL):"))
    existing_access_data[[n_con]]$Schema<-Schema
  }
}



##########################################################################
##########################################################################



create_access_api_ggmap<-function()
{
  #ggmap::register_stadiamaps("f047d97f-ff34-486f-b051-bbfe18871a51",write = TRUE)
  library(rjson)

  # emplacement du fichier json access
  home<-file.path(Sys.getenv("HOME"),"R","access")
  if (!dir.exists(home)) {dir.create(home,recursive=TRUE)}
  access_file<-file.path(home,"access.json")

  if (file.exists(access_file))
  {
    existing_access_data<- rjson::fromJSON(file = access_file)
  } else
  {
    existing_access_data=NULL

  }

  api_ggmap<-""



  # 1. renseigner la cle API de ggmap geree par stadia (https://docs.stadiamaps.com/guides/migrating-from-stamen-map-tiles/)
  reponse<-"Y" # par defaut les valeurs sont a modifier, sauf si l'utilisateur ne le souhaite pas a la question ci dessous
  # demande a l'utilisateur si il souhaite modifier les valeurs
  if (!is.null(existing_access_data$api_ggmap))
  {
    print(existing_access_data$api_ggmap)
    reponse<-readline(prompt=paste0("La cle API de ggmap-stadia existe déja. Do you want to overwrite them ? (y/n) : ",collapse=" "))
  }

  if (toupper(reponse)=="Y") # nb : Y par defaut si les valeurs n'existent pas deja
  {
    # demande a l'utilisateur les valeurs ? entrer

    while(api_ggmap=="" )  api_ggmap<- readline(prompt="Enter API KEY for ggmap : ")
  } else
  {
    api_ggmap<-existing_access_data$api_ggmap
  }

  if (is.null(existing_access_data))
  {
    access_data<-list(api_ggmap=api_ggmap)

  } else {
    access_data<-existing_access_data

    access_data$api_ggmap<-api_ggmap
  }


  jsonData <- toJSON(access_data)



  #write JSON object to file
  write(jsonData, access_file)

  if (file.exists(access_file)) print(paste0("Le fichier d'acces a corectement ete cree/modife : ",access_file))
}

