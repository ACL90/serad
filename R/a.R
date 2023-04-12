#' Calcul d'une accélération
#' @param x1 Le niveau le plus récent
#' @param x2 Le niveau le niveau avant x1
#' @param x3 Le niveau le plus ancien
#'
#' @seealso g format_g
#'
#' @return L'accélération, exprimée en pourcentage.
#'
#' @details Pas (encore) de gestion du cas ou g(x2,x3) est nul, mais cela ne devrait pas se rencontrer en pratique
#'
#' @examples
#' a(4,2,1) #0
#' a(6,2,1) #100
#'
#' @export
a = function(x1,x2,x3){100*(g(x1,x2)/g(x2,x3)-1)}

