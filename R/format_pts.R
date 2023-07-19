#' Formatage des variations, exprimées en points
#' @description
#' signe = 0 permet de retirer le signe, que ce soit un + ou un - (par exemple pour dire: diminue de 5 points).
#'
#' @details
#' Le symbole 'moins' n'est pas correctement rendu en Word lorsque utilisé avec cat. (on obtient le tiret du milieu)
#'
#' @param y La variation à formater.
#' @param signe 1 si on veut le signe, 0 sinon. Par défaut signe = 1.
#' @param detail Combien de chiffre après la virgule. Par défaut 1.
#' @param  abrev 1 si on préfère l'abréviation. Par défaut abrev = 0.
#'
#' @seealso format_g
#'
#' @return La variation mise en forme.
#'
#' @details Pour changer le comportement par défaut d'arrondi, modifier getOption("serad")$arrondi_pourcent
#'
#' @examples
#' format_pts(5.3654,signe =0)  # "5,4 points"
#' format_pts(1.3654,signe =0)  # "1,4 point"
#' format_pts(5.3654,abrev = 1) # "+5,4 pts"
#' format_pts(5.3654,1)  # "+5,4 points"
#' format_pts(-5.3654,0) # "5,4 points"
#' format_pts(-5.3654,1) # "−5,4 points"
#' format_pts(-5.3654)   # "−5,4 points"
#' format_pts(-5.3654,detail = 2)   # "−5,37 points"
#' format_pts(0.35)      # "+0.4 point"
#' @export
format_pts= function(y ,signe = 1, detail, abrev = 0){


  serad0 = getOption("serad")
  if(missing(detail)) {
      detail = serad0$arrondi_pourcent
  }
  moins = serad0$moins

  y0 = serad::arrondi_tot(y,detail)

  #####ICI ajouter le choix entre pt/point et pts/points
    ##rajouter une variable, et utiliser serad::s(x,"chat parle", "chats parlent")
  if(abrev==0) {
    post = serad::s(y0,"point", "points")
  }
  else {
    post = serad::s(y0,"pt", "pts")
  }

  formattage = paste0("%+3.",detail,"f\ua0")

  if(signe ==1) {
    w = gsub("\\.", ",",sprintf(formattage,y0))  #il y a un +
    w = paste0(w,post)
    return(gsub("-",moins,w))
  }
  else {
    w =  gsub("\\.", ",",sprintf(formattage,y0))  #pas de + ici
    w = gsub('\\+',"",w)
    w = paste0(w,post)
    return(gsub('-',"",w))
  }

}

#https://stackoverflow.com/questions/23586741/change-decimal-character-in-sprintf/73694284#73694284
# \ua0 permet de faire l'espace insécable, (non-breaking space)
# \U2212 dans moins pour avoir le symbole 'moins' en typographie

#usethis::use_test()
