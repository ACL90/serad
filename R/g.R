#' Calcul d'une variation
#' @param x1 Le niveau le plus récent
#' @param x2 Le niveau le plus ancien
#'
#' @seealso format_g
#'
#' @return La variation, exprimée en pourcentage (si x1=2.x1, cela retourne 100).
#'
#' @examples
#' g(2,1)  #100
#' g(2,0)  #2e+10 et message d'avis
#'
#' @details Dans le cas improbable où x2 = 0, on considère qu'il vaut 0.00000001.
#' C'est une valeur paramétrable avec eps. L'idée est de toujours sortir quelque chose.
#' Un message d'avis apparait dans la console.
#' Pour un utilisateur avancé, il est possible de changer l'option par défaut de eps:
#' \code{library("serad")}\cr
#' \code{serad0 = getOption("serad")}\cr
#' \code{serad0$eps = 0 }\cr
#' \code{options(serad = serad0)}
#'
#' @export
g = function(x1,x2){

  serad0 = getOption("serad")
  eps = serad0$eps #eps Gestion du cas du denominateur nul
  if(any(x2==0)){
    #ajout d un warning
    warning("division par 0 dans serad::g()")
  }
  x2bis = x2 + (x2==0)*eps
  return(100*(x1/x2bis-1))
}

#Rappel
#stringi::stri_escape_unicode("à")  #\\u00e0
#stringi::stri_escape_unicode("ù") #\\u00f9

#usethis::use_test()
