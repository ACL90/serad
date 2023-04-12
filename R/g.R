#' Calcul d'une variation
#' @param x1 Le niveau le plus récent
#' @param x2 Le niveau le plus ancien
#'
#' @seealso format_g
#'
#' @details Pas (encore) de gestion du cas où x2 est nul.
#'
#' @return La variation, exprimée en pourcentage (si x1=2.x1, cela retourne 100).
#'
#' @examples
#' g(2,1)  #100
#'
#' @export
g = function(x1,x2){100*(x1/x2-1)}


