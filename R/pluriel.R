#' Accord au pluriel
#'
#' Retourne une forme singulière ou plurielle selon une valeur numérique
#' et la langue.
#'
#' En français, le pluriel s'applique à partir de 2.
#' En anglais, seul 1 (ou -1) est au singulier.
#'
#' @param x Valeur numérique.
#' @param sing Forme utilisée au singulier. Par défaut : chaîne vide.
#' @param plur Forme utilisée au pluriel. Par défaut : `"s"`.
#' @param lang Langue de sortie : `"fr"` ou `"en"`.
#'
#' @return
#' Une chaîne de caractères correspondant à la forme correctement accordée.
#' Renvoie `NA_character_` lorsque `x` vaut `NA`.
#'
#' @examples
#' pluriel(-7.5)
#' pluriel(-2)
#' pluriel(1.4, "chat parle", "chats parlent")
#' pluriel(-2, "chat parle", "chats parlent")
#' pluriel(1.97)
#' pluriel(1.5, "point", "points", lang = "fr")
#' pluriel(1.5, "point", "points", lang = "en")
#'
#' @export
pluriel <- function(x,
                    sing = "",
                    plur = "s",
                    lang = get_serad_language()) {

  if (!lang %in% c("fr", "en")) {
    stop("`lang` doit \u00eatre \u00e9gal \u00e0 \"fr\" ou \"en\".")
  }

  est_singulier <- if (lang == "en") {
    abs(x) == 1
  } else {
    abs(x) < 2
  }

  ifelse(
    is.na(x),
    NA_character_,
    ifelse(est_singulier, sing, plur)
  )
}
