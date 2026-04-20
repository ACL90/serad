#' Met au pluriel selon la valeur numerique
#'
#' Retourne la forme singuliere ou plurielle
#' selon la valeur absolue du nombre.
#'
#' @param a Valeur numerique.
#' @param sing Forme au singulier. Par defaut : chaine vide.
#' @param plur Forme au pluriel. Par defaut : "s".
#' @param seuil Seuil a partir duquel la forme plurielle
#'   est utilisee (par defaut : 2).
#'
#' @return
#' Une chaine de caracteres correspondant a la forme
#' correctement accordee. NA si l'entree est NA.
#'
#' @examples
#' serad::s(-7.5)
#' serad::s(-2)
#' serad::s(1.4, "chat parle", "chats parlent")
#' serad::s(-2, "chat parle", "chats parlent")
#' serad::s(1.97)
#' serad::s(arrondi_tot(1.97))
#' serad::s(1.97, seuil = 1.95)
#'
#' @seealso
#' \url{https://www.dictionnaire-academie.fr/article/QDL057}
#'
#' @export
s <- function(a, sing = "", plur = "s", seuil = 2) {

  if (is.na(a))
    return(NA_character_)

  if (abs(a) >= seuil) plur else sing
}

