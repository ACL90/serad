#' Formatage des niveaux en nombres
#'
#' Formate un niveau numerique selon les regles d'arrondi
#' definies dans le package.
#'
#' @param y Le niveau a formater.
#' @param detail Precision d'arrondi. Par defaut, on utilise
#'   getOption("serad")$arrondi_niv.
#'
#' @return
#' Une chaine de caracteres correspondant au niveau formate.
#'
#' @seealso \code{\link{arrondi_tot}}
#'
#' @examples
#' format_niv(365484)  # "365 500"
#'
#' @export
format_niv <- function(y, detail = getOption("serad")$arrondi_niv) {

  y0 <- arrondi_tot(y, detail)

  w <- format(
    y0,
    big.mark = "\u00a0",   # espace insécable
    scientific = FALSE,
    trim = TRUE
  )

  gsub("-", "\u2212", w)
}
