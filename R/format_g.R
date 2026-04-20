#' Formatage des variations en pourcentage
#'
#' Formate une variation exprimee en pourcentage selon les regles
#' d'arrondi et d'affichage du package.
#'
#' @param y La variation a formater.
#' @param signe Indicateur logique : TRUE pour afficher le signe,
#'   FALSE pour le retirer (par defaut : TRUE).
#' @param detail Nombre de chiffres apres la virgule.
#'   Par defaut, on utilise getOption("serad")$arrondi_pourcent.
#'
#' @return
#' Une chaine de caracteres correspondant a la variation formatee
#' (ex. "+5,4 %", "-5,4 %").
#'
#' @seealso \code{\link{g}}, \code{\link{arrondi_tot}}
#'
#' @details
#' Le symbole "moins" peut etre personnalise via
#' getOption("serad")$moins.
#'
#' @examples
#' format_g(5.3654, signe = FALSE)  # "5,4 %"
#' format_g(5.3654)                 # "+5,4 %"
#' format_g(-5.3654, FALSE)         # "5,4 %"
#' format_g(-5.3654)                # "-5,4 %"
#' format_g(-5.3654, detail = 2)    # "-5,37 %"
#' format_g(0.35)                   # "+0,4 %"
#'
#' @export
format_g <- function(y, signe = TRUE, detail = getOption("serad")$arrondi_pourcent) {

  moins <- getOption("serad")$moins

  y0 <- serad::arrondi_tot(y, detail)

  # format avec signe explicite
  fmt <- paste0("%+.", detail, "f\u00a0%%")
  w   <- sprintf(fmt, y0)

  # virgule française
  w <- gsub("\\.", ",", w)

  if (!signe) {
    w <- gsub("\\+", "", w)
    w <- gsub("[-\u2212]", "", w)
  } else {
    w <- gsub("-", moins, w)
  }

  w
}
