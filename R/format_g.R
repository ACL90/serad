#' Formatage Dares des variations, exprimées en pourcentage ('g')
#' @description
#' signe = 0 permet de retirer le signe, que ce soit un + ou un - (par exemple pour dire: diminue de 5 \%).
#'
#' @details
#' Le symbole 'moins' n'est pas correctement rendu en Word lorsque utilisé avec cat. (on obtient le tiret du milieu)
#'
#' @param y La variation à formater.
#' @param signe 1 si on veut le signe, 0 sinon. Par défaut signe = 1.
#'
#' @seealso g
#'
#' @return La variation mise en forme.
#'
#' @examples
#' format_g(5.3654,0)  # "5,4 %"
#' format_g(5.3654,1)  # "+5,4 %"
#' format_g(-5.3654,0) # "5,4 %"
#' format_g(-5.3654,1) # "−5,4 %"
#' format_g(-5.3654)   # "−5,4 %"
#' @export
format_g = function(y,signe = 1){
  if(signe ==1) {
    w = gsub("\\.", ",",sprintf("%+3.1f\ua0%%",y))  #il y a un +
    return(gsub("-","\U2212",w))
  }
  else {
    w =  gsub("\\.", ",",sprintf("%3.1f\ua0%%",y))  #pas de + ici
    return(gsub('-+',"",w))
  }
}

# NOTE sur format_g : peut s'utiliser typiquement en combinaison avec g(x1,x2)
#https://stackoverflow.com/questions/23586741/change-decimal-character-in-sprintf/73694284#73694284
# \ua0 permet de faire l'espace ins?cable, (non-breaking space)
# \U2212 pour avoir le symbole 'moins' en typographie
