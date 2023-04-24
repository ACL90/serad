#' Formatage Dares des niveaux, exprimés en nombres
#'
#' @param y Le niveau à formater.
#' @param detail Par défaut -2 pour arrondir à la centaine.
#'
#' @seealso arrondi_tot
#'
#' @return Le niveau formaté
#'
# @importFrom dplyr last
#'
#' @examples
#' format_niv(365484)  # 365 500
#'
#' @details Pour changer le comportement par défaut d'arrondi,
#' modifier getOption("serad")$arrondi_niv
#'
#' @export
format_niv =function(y,detail){

  if(missing(detail)) {
    serad0 = getOption("serad")
    detail = serad0$arrondi_niv #-2 pour arrondir à la centaine
  }


  w = format(arrondi_tot(y,detail),big.mark="\ua0", scientific=FALSE)
  #w = format(arrondi_tot(last(y),detail),big.mark="\ua0", scientific=FALSE)

  return(gsub("-","\u2212",w))
}

#usethis::use_test()
