#' comparaison_taux : fonction générique basée sur la variation
#' @param g La variation, exprimée en pourcentage (0.1 signifie 0.1%)
#' @param hausse0 Mot si hausse
#' @param egalite0 Mot si egalite
#' @param baisse0 Mot si baisse
#' @param seuil La limite pour l'egalite (par defaut 0.1%)
#' @param param Un parametre supplementaire égal à 0 ou 1 (par exemple pour singulier)
#' @param hausse1 Formulation différente dépendant de param ; par défaut hausse0
#' @param egalite1 Formulation différente dépendant de param ; par défaut egalite0
#' @param baisse1 Formulation différente dépendant de param ; par défaut baisse0
#'
#' @details Aussi fonction de base pour comparaison.R
#'
#' @seealso comparaison
#'
#' @return Un mot
#'
#' @examples
#' comparaison_taux(5,"augmente","reste stable","diminue")             #augmente
#' comparaison_taux(0.05,"augmente","reste stable","diminue")          #reste stable
#' comparaison_taux(0.05,"augmente","reste stable","diminue",seuil=0)  #augmente
#' comparaison_taux(0,"augmente","reste égal","diminue", seuil=0)      #augmente
#' comparaison_taux(0,"as","bs","cs",seuil=0,param=1,hausse1="a",egalite1="b",baisse1="c")      #a
#' comparaison_taux(0,"as","bs","cs",seuil=0,param=0,hausse1="a",egalite1="b",baisse1="c")      #as
#' comparaison_taux(0,"as","bs","cs",seuil=0,hausse1="a",egalite1="b",baisse1="c") #as
#'
#'
#' @export
comparaison_taux  = function(g,hausse0,egalite0,baisse0,
                         seuil=0.1,param=0,
                         hausse1=hausse0,egalite1=egalite0,baisse1=baisse0){
  # dplyr::case_when(((g>=seuil)&(param==0))~hausse0,
  #           ((g>=seuil)&(param==1))~hausse1,
  #           ((g<=-seuil)&(param==0))~baisse0,
  #           ((g<=-seuil)&(param==1))~baisse1,
  #           (((abs(g)<seuil)|(g=0))&(param==0))~egalite0,
  #           (((abs(g)<seuil)|(g=0))&(param==1))~egalite1    )

  ifelse((g>=seuil)&(param==0),hausse0,
    ifelse((g>=seuil)&(param==1),hausse1,
      ifelse((g<=-seuil)&(param==0),baisse0,
        ifelse((g<=-seuil)&(param==1),baisse1,
          ifelse(((abs(g)<seuil)|(g=0))&(param==0),egalite0,
            egalite1 #ifelse(((abs(g)<seuil)|(g=0))&(param==1),
          )
        )
      )
    )
  )

}
#usethis::use_test()
