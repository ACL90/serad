#' Formatage des variations en niveau
#'
#' Formate une difference numerique selon les regles d'arrondi
#' et d'affichage utilisees dans le package.
#'
#' @param y La difference a formater.
#' @param signe Indicateur logique : TRUE pour afficher le signe,
#'   FALSE sinon (par defaut : TRUE).
#' @param detail Precision d'arrondi. Par defaut, on utilise
#'   getOption("serad")$arrondi_niv.
#'
#' @return
#' Une chaine de caracteres correspondant a la variation formatee.
#'
#' @seealso \code{\link{arrondi_tot}}, \code{\link{format_niv}}
#'
#' @examples
#' format_delta(365484)           # "+365 500"
#' format_delta(365484, FALSE)    # "365 500"
#' format_delta(-365484)          # "-365 500"
#' format_delta(-365484, FALSE)   # "365 500"
#'
#' @export
format_delta <- function(y, signe = TRUE, detail = getOption("serad")$arrondi_niv) {

  z <- format_niv(y, detail)

  if (signe) {
    if (y >= 0) {
      return(paste0("+", z))
    } else {
      return(z)
    }
  } else {
    return(gsub("[-\u2212]", "", z)) # supprime tout signe négatif (classique ou unicode)
  }
}
