#' Met au pluriel si la valeur absolue du nombre est supérieure ou égale a 2.
#' @param a Le nombre
#' @param sing Le mot à indiquer au singulier, rien par défaut.
#' @param plur Le mot à indiquer au pluriel, -s par défaut (d'ou le nom de la fonction)
#' @param seuil Pour gérer le cas limite où un arrondi à 2 depuis l'inférieur.
#'
#' @return Le mot bien accordé ; par défault '-s ou rien.
#'
#' @examples
#' s(-7.5)
#' s(-2)
#' s(1.4,"chat parle", "chats parlent")
#' s(-2,"chat parle", "chats parlent")
#'
#' @export
s=function(a,sing="",plur="s",seuil=1.95){
  ifelse(abs(a)>=seuil,plur,sing)
}
