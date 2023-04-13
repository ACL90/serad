#' Calcul d'une variation
#' @param x1 Le niveau le plus récent
#' @param x2 Le niveau le plus ancien
#' @param eps Gestion du cas du denominateur nul
#'
#' @seealso format_g
#'
#' @return La variation, exprimée en pourcentage (si x1=2.x1, cela retourne 100).
#'
#' @examples
#' g(2,1)  #100
#'
#' @details Dans le cas improbable où x2 = 0, on considère qu'il vaut 0.00000001.
#' C'est une valeur paramétrable avec eps. L'idée est de toujours sortir quelque chose.
#'
#' @export
g = function(x1,x2,eps=0.00000001){
  if(x2==0){
    #ajout d un warning
    warning("division par 0 à l'appel de serad::g()")
    return(100*(x1/eps-1))
  }
  else return(100*(x1/x2-1))
}

