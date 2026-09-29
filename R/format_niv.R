#' Formatage des niveaux en nombres
#'
#' Formate un niveau numérique selon les règles d'arrondi
#' définies dans le package. Peut afficher explicitement le signe
#' d'une variation en niveau.
#'
#' @param y Le niveau ou la variation à formater.
#' @param signe Indicateur logique : TRUE pour afficher le signe plus
#'   des valeurs positives, FALSE sinon (par défaut).
#' @param detail Précision d'arrondi. Par défaut, on utilise
#'   getOption("serad")$arrondi_niv.
#' @param lang Langue de sortie : "fr" ou "en".
#'
#' @return
#' Une chaîne de caractères correspondant à la valeur formatée.
#'
#' @seealso \code{\link{arrondi_tot}}
#'
#' @examples
#' format_niv(365484)                        # "365 500"
#' format_niv(365484, lang = "en")           # "365,500"
#' format_niv(365484 - 300000, signe = TRUE) # "+65 500"
#' format_niv(300000 - 365484, signe = TRUE) # "-65 500"
#'
#' @export
format_niv <- function(y,
                       signe = FALSE,
                       detail = getOption("serad")$arrondi_niv,
                       lang = get_serad_language()) {

  moins <- getOption("serad")$moins
  y0 <- serad::arrondi_tot(y, detail)

  mark <- if (lang == "fr") "\u00a0" else ","

  w <- format(
    y0,
    big.mark = mark,
    scientific = FALSE,
    trim = TRUE
  )

  w <- gsub("-", moins, w, fixed = TRUE)

  if (signe) {
    positifs <- !is.na(y0) & y0 >= 0
    w[positifs] <- paste0("+", w[positifs])
  }

  w
}
