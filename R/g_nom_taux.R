#' Evolution décrite de facon nominale, non suivie d'une évolution.
#' @param g L'évolution, en pourcentage.
#'
#' @return L'évolution, par exemple: "une forte hausse".
#'
#' @examples
#' g_nom_taux(4)       # une forte hausse
#' g_nom_taux(1)       # une hausse
#' g_nom_taux(0.4)     # une hausse modérée
#' g_nom_taux(0.1)     # une légère hausse
#' g_nom_taux(0)       # une stabilité
#' g_nom_taux(-0.3)    # une légère baisse
#' g_nom_taux(-1)      # une baisse modérée
#' g_nom_taux(-4)      # une baisse
#' g_nom_taux(-5)      # une forte baisse
#'
# @importFrom dplyr case_when
#'
#' @export
g_nom_taux = function(g){


  serad0 = getOption("serad")
  seuil = serad0$nomse

         # z = case_when(g>seuil$fortttt  ~serad0$nm$fortttt,
         #               g>seuil$forttt   ~serad0$nm$forttt,
         #               g>seuil$fortt    ~serad0$nm$fortt,
         #               g>seuil$fort     ~serad0$nm$fort,
         #               g>seuil$faible   ~serad0$nm$faible,
         #               g>seuil$faiblee  ~serad0$nm$faiblee,
         #               g>seuil$faibleee ~serad0$nm$faibleee,
         #               g>seuil$faibleeee~serad0$nm$faibleeee,
         #              g<=seuil$faibleeee~serad0$nm$faibleeeee)

         z =    ifelse(g>seuil$fortttt  ,serad0$nm$fortttt,
                ifelse(g>seuil$forttt   ,serad0$nm$forttt,
                ifelse(g>seuil$fortt    ,serad0$nm$fortt,
                ifelse(g>seuil$fort     ,serad0$nm$fort,
                ifelse(g>seuil$faible   ,serad0$nm$faible,
                ifelse( g>seuil$faiblee ,serad0$nm$faiblee,
                ifelse(g>seuil$faibleee ,serad0$nm$faibleee,
                ifelse(g>seuil$faibleeee,serad0$nm$faibleeee,
                                        serad0$nm$faibleeeee #g<=seuil$faibleeee~
                       ))))))))

  return(z)
}

#usethis::use_test()

#quelques rappels
#stringi::stri_escape_unicode("?")
#\\u00e9
#stringi::stri_escape_unicode("?")
#\\u00e8
#stringi::stri_escape_unicode("?")
#\\u00e0"
