#' à la hausse //  à la baisse // inchangé
#' @param x1 Le niveau le plus récent
#' @param x2 Le niveau le plus ancien
#' @param seuil La limite pour l'égalite (par défaut 0.1, c'est-à-dire 0.1%)
#'
#' @seealso comparaison
#'
#' @return à la hausse // inchangé // à la baisse
#'
#' @examples
#' alahausse(1.004,1)   # à la hausse
#' alahausse(0.996,1)   # à la baisse
#' alahausse(1,1.0004)  # inchangé
#'
#' @export
alahausse  = function(x1,x2,seuil=0.1){
  return(comparaison(x1,x2,"\u00e0 la hausse","inchang\u00e9","\u00e0 la baisse",seuil))
}

#quelques rappels
#stringi::stri_escape_unicode("?")
#\\u00e9
#stringi::stri_escape_unicode("?")
#\\u00e8
#stringi::stri_escape_unicode("?")
#\\u00e0"

