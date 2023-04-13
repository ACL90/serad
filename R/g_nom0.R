#' Evolution décrite de facon nominale, non suivie d'une évolution.
#' @param g L'évolution, en pourcentage.
#'
#' @return L'évolution, par exemple: "une forte hausse".
#'
#' @examples
#' g_nom0(4)       # une forte hausse
#' g_nom0(1)       # une hausse
#' g_nom0(0.4)     # une hausse modérée
#' g_nom0(0.1)     # une légère hausse
#' g_nom0(0)       # une stabilité
#' g_nom0(-0.3)    # une légère baisse
#' g_nom0(-1)      # une baisse modérée
#' g_nom0(-4)      # une baisse
#' g_nom0(-5)      # une forte baisse
#'
#' @export
g_nom0 = function(g){
  dplyr::case_when(g>4-0.05~"une forte hausse",
            g>1-0.05~"une hausse",
            g>0.4-0.05~"une hausse mod\u00e9r\u00e9e",
            g>0.1-0.05~"une l\u00e9g\u00e8re hausse",
            g>0-0.05~"une stabilit\u00e9",
            g>-0.3-0.05~"une l\u00e9g\u00e8re baisse",
            g>-1-0.05~"une baisse mod\u00e9r\u00e9e",
            g>-4-0.05~"une baisse",
            g<=-4-0.05~"une forte baisse")
}

#TO-DO pour la suite: avoir tous les parametres en une seule matriceparametre avec deux colonnes: la fonction utilisee, le qualificatif texte, le seuil bas
#que cette matrice soit facile a appeler, et a changer de fa?on 'permanente' par l'utilisateur

#quelques rappels
#stringi::stri_escape_unicode("?")
#\\u00e9
#stringi::stri_escape_unicode("?")
#\\u00e8
#stringi::stri_escape_unicode("?")
#\\u00e0"
