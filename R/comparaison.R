#' comparaison : fonction générique basée sur x1 et x2
#' @param x1 Le niveau le plus récent
#' @param x2 le niveau le plus ancien
#' @param hausse0 Mot si hausse
#' @param egalite0 Mot si egalite
#' @param baisse0 Mot si baisse
#' @param seuil La limite pour l'egalite (par defaut 0.1%)
#' @param param Un parametre supplementaire égal à 0 ou 1 (par exemple pour singulier)
#' @param hausse1 Formulation différente si hausse, dépendant de param
#' @param egalite1 Formulation différente si egalite, dépendant de param
#' @param baisse1 Formulation différente si baisse, dépendant de param
#'
#' @details Utilise comparaison0.R
#'
#' @seealso comparaison0 alahausse audessus davantage depasse g_nom_simple
#'
#' @return Un mot.
#'
#' @examples
#' comparaison(1.04,1,"augmente","reste stable","diminue")             #augmente
#' comparaison(0.9991,1,"augmente","reste stable","diminue")           #reste stable
#' comparaison(0.999,1,"augmente","reste stable","diminue")            #diminue
#' comparaison(0.9991,1,"augmente","reste stable","diminue",seuil = 0) #diminue
#' comparaison(1,1,"augmente","reste égal","diminue",seuil = 0)        #augmente
#'
#' @export
comparaison  = function(x1,x2,hausse0,egalite0,baisse0,seuil=0.1,
                        param=0,
                        hausse1=hausse0,egalite1=egalite0,baisse1=baisse0){
  return(comparaison0(g(x1,x2),
                      hausse0,egalite0,baisse0,
                      seuil,param,
                      hausse1,egalite1,baisse1))

}
