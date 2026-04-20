#' Arrondi arithmetique
#'
#' Arrondit un nombre selon la regle arithmetique (0.5 vers le haut),
#' contrairement a \code{round()} qui utilise l'arrondi bancaire.
#'
#' @param x Nombre a arrondir.
#' @param digits Entier indiquant le nombre de decimales.
#'   Positif pour les decimales, negatif pour les dizaines, centaines, etc.
#'   Par defaut : 1.
#'
#' @return
#' Un nombre numerique arrondi.
#'
#' @seealso \code{\link{format_niv}}
#'
#' @examples
#' arrondi_tot(1877.85, digits = 0)   # 1878
#' arrondi_tot(1877.85, digits = 1)   # 1877.9
#' arrondi_tot(1877.85, digits = -1)  # 1880
#'
#' @export
arrondi_tot <- function(x, digits = 1) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- floor(z + 0.5)
  z <- z / 10^digits
  z * posneg
}
