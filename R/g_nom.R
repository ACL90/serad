#' Evolution décrite de facon nominale, non suivie d'une évolution.
#' @param x1 Le niveau le plus récent
#' @param x2 le niveau le plus ancien
#'
#' @return L'évolution, par exemple: "une forte hausse".
#'
#' @examples
#' g_nom(1.04,1)  # une forte hausse
#' g_nom(1.01,1)  # une hausse
#' g_nom(1.004,1) # une hausse modérée
#' g_nom(1.001,1) # une légère hausse
#' g_nom(1,1)     # une stabilité
#' g_nom(0.997,1) # une légère baisse
#' g_nom(0.99,1)  # une baisse modérée
#' g_nom(0.96,1)  # une baisse
#' g_nom(0.95,1)  # une forte baisse
#'
#' @export
g_nom = function(x1,x2){
  g = g(x1,x2)
  return(g_nom0(g))
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
