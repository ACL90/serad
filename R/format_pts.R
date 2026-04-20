#' Formatage des variations en points
#'
#' Formate une variation exprimee en points selon les regles
#' d'arrondi et d'affichage du package.
#'
#' @param y La variation a formater.
#' @param signe Indicateur logique : TRUE pour afficher le signe,
#'   FALSE pour le retirer (par defaut : TRUE).
#' @param detail Nombre de chiffres apres la virgule.
#'   Par defaut, on utilise getOption("serad")$arrondi_pourcent.
#' @param abrev Indicateur logique : TRUE pour utiliser l'abreviation
#'   ("pt"/"pts"), FALSE pour afficher "point(s)" (par defaut : FALSE).
#'
#' @return
#' Une chaine de caracteres correspondant a la variation formatee
#' (ex. "+5,4 points", "+5,4 pts").
#'
#' @seealso \code{\link{format_g}}
#'
#' @details
#' Le symbole "moins" peut etre personnalise via
#' getOption("serad")$moins.
#'
#' @examples
#' format_pts(5.3654, signe = FALSE)   # "5,4 points"
#' format_pts(1.3654, signe = FALSE)   # "1,4 point"
#' format_pts(5.3654, abrev = TRUE)    # "+5,4 pts"
#' format_pts(-5.3654, FALSE)          # "5,4 points"
#' format_pts(-5.3654)                 # "-5,4 points"
#' format_pts(-5.3654, detail = 2)     # "-5,37 points"
#' format_pts(0.35)                    # "+0,4 point"
#'
#' @export
format_pts <- function(y,
                       signe  = TRUE,
                       detail = getOption("serad")$arrondi_pourcent,
                       abrev  = FALSE) {

  moins <- getOption("serad")$moins

  y0 <- serad::arrondi_tot(y, detail)

  # Singulier / pluriel
  post <- if (!abrev) {
    serad::s(y0, "point", "points")
  } else {
    serad::s(y0, "pt", "pts")
  }

  # Format numérique
  fmt <- paste0("%+.", detail, "f\u00a0")
  w   <- sprintf(fmt, y0)
  w   <- gsub("\\.", ",", w)

  # Gestion des signes
  if (!signe) {
    w <- gsub("\\+", "", w)
    w <- gsub("[-\u2212]", "", w)
  } else {
    w <- gsub("-", moins, w)
  }

  paste0(w, post)
}
