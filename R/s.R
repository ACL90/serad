#' Met au pluriel si la valeur absolue du nombre est supérieure ou égale a 2.
#' @param a Le nombre
#' @param sing Le mot à indiquer au singulier, rien par défaut.
#' @param plur Le mot à indiquer au pluriel, -s par défaut (d'ou le nom de la fonction)
#' @param seuil Pour gérer un arrondi à 2 depuis l'inférieur.
#'
#' @return Le mot bien accordé ; par défault '-s ou rien. TEST
#'
#' @examples
#' serad::s(-7.5)             #s
#' serad::s(-2)               #s
#' serad::s(1.4,"chat parle", "chats parlent") #chat parle
#' serad::s(-2,"chat parle", "chats parlent")  #chats parlent
#' serad::s(1.97)             #NA
#' serad::s(arrondi_tot(1.97))    #s
#' serad::s(1.97, seuil=1.95) #s
#'
#' @seealso
#' Le [site de l'Académie française](https://www.dictionnaire-academie.fr/article/QDL057)
#'
#' @export
s=function(a,sing="",plur="s",seuil=2){
  ifelse(abs(a)>=seuil,plur,sing)
}

#usethis::use_test()
