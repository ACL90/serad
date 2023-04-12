#' Arrondit un nombre.
#'
#' @param x Le nombre à arrondir.
#' @param digit Un entier positif (pour arrondir après la virgule) ou négatif (pour arrondir à la centaine ou au millier par exemple). Par défaut digit = 1.
#'
#' @seealso format_niv
#'
#' @return Le nombre arrondi.
#'
#' @examples
#' arrondi_tot(1877.85,digit=0)  #1878
#' arrondi_tot(1877.85,digit=1)  #1877.9
#' arrondi_tot(1877.85,digit=2)  #1877.85
#' arrondi_tot(1877.85,digit=-1) #1880
#' arrondi_tot(1877.85,digit=-2) #1900
#'
#' @details Notoirement utile car round(0.35) donne...0.3.
#' Merci a Chloe Pariset et Gaetan Guillermin pour cette fonction.
#'
#' @export
arrondi_tot <- function(x,digit=1){
  x <- x*10^digit
  if (x < 0) {
    x <-ifelse(x-trunc(x)<=-0.5,trunc(x)-1,trunc(x))
  }
  else {
    x<-ifelse(x-trunc(x)>=0.5,trunc(x)+1,trunc(x))
  }
  x/10^digit
}
