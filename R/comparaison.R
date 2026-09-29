#' Comparaison qualitative entre deux niveaux
#'
#' Compare deux niveaux successifs et retourne une formulation
#' selon l'évolution observée.
#'
#' @param x1 Niveau le plus récent.
#' @param x2 Niveau le plus ancien.
#' @param hausse_defaut Formulation en cas de hausse.
#' @param egalite_defaut Formulation en cas de stabilité.
#' @param baisse_defaut Formulation en cas de baisse.
#' @param seuil Seuil d'égalité en valeur absolue. Par défaut : 0.1.
#' @param alt Indicateur logique permettant d'utiliser
#'   une formulation alternative.
#' @param hausse_alt Formulation alternative en cas de hausse.
#' @param egalite_alt Formulation alternative en cas de stabilité.
#' @param baisse_alt Formulation alternative en cas de baisse.
#'
#' @details
#' La comparaison repose sur le taux de variation calculé via \code{\link{g}}.
#' Des cas particuliers sont traités lorsque \code{x2} est nul ou négatif.
#'
#' Les exemples ci-dessous proposent trois formulations à copier-coller
#' et à adapter selon le contexte. Ces fonctions ne font pas partie
#' du package.
#'
#' @return
#' Une chaîne de caractères correspondant à la formulation retenue.
#'
#' @seealso \code{\link{comparaison_taux}}, \code{\link{g}}
#'
#' @examples
#' comparaison(1.04, 1, "augmente", "reste stable", "diminue")
#' comparaison(0.9991, 1, "augmente", "reste stable", "diminue")
#' comparaison(1, 1, "augmente", "reste égal", "diminue", seuil = 0)
#'
#' # Situer un niveau par rapport à un autre :
#' # « le niveau est au-dessus de celui de l'année précédente ».
#' formuler_position <- function(x1, x2, lang = get_serad_language()) {
#'   if (lang == "en") {
#'     comparaison(
#'       x1, x2,
#'       hausse_defaut = "above",
#'       egalite_defaut = "at the same level as",
#'       baisse_defaut = "below",
#'       seuil = 0
#'     )
#'   } else {
#'     comparaison(
#'       x1, x2,
#'       hausse_defaut = "au-dessus de",
#'       egalite_defaut = "au même niveau que",
#'       baisse_defaut = "en dessous de",
#'       seuil = 0
#'     )
#'   }
#' }
#'
#' formuler_position(104, 100)
#'
#' # Qualifier une tendance :
#' # « la tendance est à la hausse ».
#' formuler_tendance <- function(
    #'     x1, x2, seuil = 0.1, lang = get_serad_language()
#' ) {
#'   if (lang == "en") {
#'     comparaison(
#'       x1, x2,
#'       hausse_defaut = "upward",
#'       egalite_defaut = "stable",
#'       baisse_defaut = "downward",
#'       seuil = seuil
#'     )
#'   } else {
#'     comparaison(
#'       x1, x2,
#'       hausse_defaut = "à la hausse",
#'       egalite_defaut = "stable",
#'       baisse_defaut = "à la baisse",
#'       seuil = seuil
#'     )
#'   }
#' }
#'
#' formuler_tendance(1.04, 1)
#'
#' # Comparer des nombres d'éléments :
#' # « il y en a davantage ».
#' formuler_quantite <- function(x1, x2, lang = get_serad_language()) {
#'   if (lang == "en") {
#'     comparaison(
#'       x1, x2,
#'       hausse_defaut = "more",
#'       egalite_defaut = "as many",
#'       baisse_defaut = "fewer",
#'       seuil = 0
#'     )
#'   } else {
#'     comparaison(
#'       x1, x2,
#'       hausse_defaut = "davantage",
#'       egalite_defaut = "autant",
#'       baisse_defaut = "moins",
#'       seuil = 0
#'     )
#'   }
#' }
#'
#' formuler_quantite(104, 100)
#'
#' @export
comparaison <- function(x1, x2,
                        hausse_defaut,
                        egalite_defaut,
                        baisse_defaut,
                        seuil = 0.1,
                        alt = 0,
                        hausse_alt = hausse_defaut,
                        egalite_alt = egalite_defaut,
                        baisse_alt = baisse_defaut) {

  hausse  <- if (alt == 0) hausse_defaut  else hausse_alt
  egalite <- if (alt == 0) egalite_defaut else egalite_alt
  baisse  <- if (alt == 0) baisse_defaut  else baisse_alt

  if (x2 == 0) {
    if (x1 > abs(seuil)) {
      return(hausse)
    } else if (x1 < -abs(seuil)) {
      return(baisse)
    } else {
      return(egalite)
    }
  }

  if (x2 > 0) {
    return(
      comparaison_taux(
        g(x1, x2),
        hausse_defaut, egalite_defaut, baisse_defaut,
        seuil, alt,
        hausse_alt, egalite_alt, baisse_alt
      )
    )
  }

  if (x2 < 0 && x1 > 0) {
    return(
      comparaison_taux(
        g(-x2, -x1),
        hausse_defaut, egalite_defaut, baisse_defaut,
        seuil, alt,
        hausse_alt, egalite_alt, baisse_alt
      )
    )
  }

  if ((x1 - x2) <= abs(seuil)) {
    return(egalite)
  } else {
    return(hausse)
  }
}
