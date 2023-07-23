#' Fournit dynamiquement le trimestre sous forme littéraire
#' @param trim Le trimestre sous la forme d'un chiffre : 1, 2, 3 ou 4.
#' @param annee Un nombre positif en 4 chiffres
#' @param type Par défaut: "lettres" (pour 1er trimestre XXXX). Sinon: "chiffres" (pour premier trimestre XXXX)
#' @param majuscule Par défaut 0. 1 pour avoir "Premier" plutôt que "premier"
#'
#' @return 1er trimestre XXXX (ou une variante)
#'
#' @examples
#' quelTrim(3,2023)                  #troisième trimestre 2023
#' quelTrim(3,2023,majuscule=1)      #Troisième trimestre 2023
#' quelTrim(3,2023,type="chiffres") #3e trimestre 2023
#' quelTrim(1,2023,type="chiffres") #1er trimestre 2023
#'
#' @seealso
#' nextTrim prevTrim
#'
#' @export
quelTrim = function(trim,annee,type="lettres",majuscule=0) {
  annee=as.numeric(annee)
  trim=as.numeric(trim)
    if(type=="lettres") {
      if(trim==1 && majuscule==1) {
        b="Premier"
      }    else if(trim==1 && majuscule==0) {
        b="premier"
      } else if(trim==2 && majuscule==1) {
        b="Deuxi\u00e8me"
      }    else if(trim==2 && majuscule==0) {
        b="deuxi\u00e8me"
      }    else if(trim==3 && majuscule==1) {
        b="Troisi\u00e8me"
      }    else if(trim==3 && majuscule==0) {
        b="troisi\u00e8me"
      }    else if(trim==4 && majuscule==1) {
        b="Quatri\u00e8me"
      }    else if(trim==4 && majuscule==0) {
        b="quatri\u00e8me"
      }    else b="Autres" #mettre un message d'erreur ici
    }
    if(type=="chiffres") {
      b = ifelse(trim==1,"1er",paste0(trim,"e"))
    }
    b = paste0(b, " ","trimestre"," ",annee)
    return(b)


}

#usethis::use_test()



###################
#nextTrim()
#' Fournit dynamiquement le trimestre suivant sous forme littéraire
#' @param trim Le trimestre sous la forme d'un chiffre : 1, 2, 3 ou 4.
#' @param annee Un nombre positif en 4 chiffres
#' @param type Par défaut: "lettres" (pour 1er trimestre XXXX). Sinon: "chiffres" (pour premier trimestre XXXX)
#' @param majuscule Par défaut 0. 1 pour avoir "Premier" plutôt que "premier"
#'
#' @return 1er trimestre XXXX (ou une variante)
#'
#' @examples
#' nextTrim(3,2023)                  #quatrième trimestre 2023
#' nextTrim(4,2023)                  #premier trimestre 2024
#'
#' @seealso
#' quelTrim prevTrim
#'
#' @export
nextTrim = function(trim,annee,type="lettres",majuscule=0) {
  annee=as.numeric(annee)
  trim=as.numeric(trim)
  if(trim==4) {
    b = quelTrim(1,annee+1,type,majuscule)
  }
  else {
    b= quelTrim(trim+1,annee,type,majuscule)
  }
  return(b)
}


###################
#nextTrim()
#' Fournit dynamiquement le trimestre suivant sous forme littéraire
#' @param trim Le trimestre sous la forme d'un chiffre : 1, 2, 3 ou 4.
#' @param annee Un nombre positif en 4 chiffres
#' @param type Par défaut: "lettres" (pour 1er trimestre XXXX). Sinon: "chiffres" (pour premier trimestre XXXX)
#' @param majuscule Par défaut 0. 1 pour avoir "Premier" plutôt que "premier"
#'
#' @return 1er trimestre XXXX (ou une variante)
#'
#' @examples
#' prevTrim(1,2023)                  #quatrième trimestre 2022
#'
#' @seealso
#' nextTrim quelTrim
#'
#' @export
prevTrim = function(trim,annee,type="lettres",majuscule=0) {
  annee=as.numeric(annee)
  trim=as.numeric(trim)
  if(trim==1) {
    b = quelTrim(4,annee-1,type,majuscule)
  }
  else {
    b= quelTrim(trim-1,annee,type,majuscule)
  }
  return(b)
}
