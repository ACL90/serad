#' Formatage Dares des niveaux, exprimés en nombres
#'
#' @param y Le niveau à formater.
#' @param detail Par défaut -2 pour arrondir à la centaine.
#'
#' @seealso arrondi_tot
#'
#' @return Le niveau formaté
#'
#' @importFrom dplyr last
#'
#' @examples
#' format_niv(365484)  # 365 500
#'
#' @export
format_niv =function(y,detail = -2){
  w = format(arrondi_tot(last(y),detail),big.mark="\ua0", scientific=FALSE)
  return(gsub("-","\u2212",w))
}

