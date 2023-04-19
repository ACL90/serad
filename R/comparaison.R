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
#' @details Utilise comparaison_taux.R
#'
#' @seealso comparaison_taux alahausse audessus davantage depasse g_nom_simple
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
  return(comparaison_taux(g(x1,x2),
                      hausse0,egalite0,baisse0,
                      seuil,param,
                      hausse1,egalite1,baisse1))

}


###################
#audessus()
#' au-dessus ou en dessous
#' @param x1 Le niveau le plus récent
#' @param x2 Le niveau le plus ancien
#'
#' @seealso comparaison
#'
#' @return au-dessus // en dessous
#'
#' @examples
#' audessus(1.04,1)    # au-dessus
#' audessus(1.04,1.04) # au-dessus
#' audessus(0.96,1)    # en dessous
#'
#' @export
audessus = function(x1,x2){
  return(comparaison(x1,x2,"au-dessus","au-dessus","en dessous",0))
}

###################
#alahausse()
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



###################
#davantage()
#' davantage ou moins
#' @param x1 Le niveau le plus récent
#' @param x2 Le niveau le plus ancien
#'
#' @seealso comparaison
#'
#' @return davantage ou moins
#'
#' @examples
#' davantage(1.04,1)    # davantage
#' davantage(1.04,1.04) # davantage
#' davantage(0.96,1)    # moins
#'
#' @export
davantage = function(x1,x2){
  return(comparaison(x1,x2,"davantage","davantage","moins",0))
}


###################
#depasse()
#' verbe pour exprimer le sens de l'évolution, pouvant gérer si le nom est singulier ou pluriel
#' @param x1 Le niveau le plus récent
#' @param x2 Le niveau le plus ancien
#' @param sing 0 si le nom est un pluriel, 1 si un singulier. Par défaut un pluriel.
#'
#' @seealso comparaison
#'
#' @return 'excéde//-nt' ou 'est//sont au niveau de' ou 'est//sont en dessous de'
#'
#' @examples
#' depasse(1.04,1)          # excédent
#' depasse(1.04,1,sing=1)   # excéde
#' depasse(0.96,1)          # sont en dessous de
#' depasse(0.9991,1)        # sont au niveau de
#'
#' @export
depasse = function(x1,x2,sing=0){  #sing=1 pour singulier
  return(comparaison(x1,x2,param=sing,
                     hausse0="exc\u00e8dent",hausse1="exc\u00e8de",
                     egalite0="sont au niveau de",egalite1="est au niveau de",
                     baisse0="sont en dessous de",baisse1="est en dessous de"
  )
  )
}
#quelques rappels
#stringi::stri_escape_unicode("é")
#\\u00e9
#stringi::stri_escape_unicode("è")
#\\u00e8
#stringi::stri_escape_unicode("à")
#\\u00e0"


###################
#g_nom_simple()
#' Evolution décrite (simplement) de facon nominale, suivie d'une évolution
#' @param x1 Le niveau le plus récent
#' @param x2 Le niveau le plus ancien
#'
#' @return en hausse de//en baisse de + pourcentage
#'
#' @examples
#' g_nom_simple(3,1) # en hausse de 200,0 %
#' g_nom_simple(3,5) # en baisse de 40,0 %
#'
#' @export
g_nom_simple = function(x1,x2){
  #    z = case_when(g(x1,x2)>=0~"en hausse de",
  #                  g(x1,x2)<0~"en baisse de")
  z = comparaison(x1,x2,"en hausse de","en hausse","en baisse de")
  return(paste(z,format_g(g(x1,x2),signe=0)))
}







