#' Calcul d'une accélération
#' @param x1 Le niveau le plus récent
#' @param x2 Le niveau le niveau avant x1
#' @param x3 Le niveau le plus ancien
#'
#' @seealso g format_g
#'
#' @return L'accélération, exprimée en pourcentage.
#'
#' @examples
#' a(4,2,1) #0
#' a(6,2,1) #100
#' a(2,1,1) #1e+12 et message d'avis
#'
#' @export
a = function(x1,x2,x3){
  serad::g(
    serad::g(x1,x2),
    serad::g(x2,x3)
  )
  }

