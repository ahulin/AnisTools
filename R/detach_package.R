#' detach_package
#'
#' @param pkg nom du package
#' @param character.only
#'
#' @description permet de detacher un package proprement et sans erreurs
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' detach_package("vegan", TRUE)
#' }
detach_package <- function(pkg, character.only = FALSE)
{
  if(!character.only)
  {
    pkg <- deparse(substitute(pkg))
  }
  search_item <- paste("package", pkg, sep = ":")
  while(search_item %in% search())
  {
    detach(search_item, unload = TRUE, character.only = TRUE)
  }
}
