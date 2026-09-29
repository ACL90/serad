#' Évolution verbale
#'
#' @description
#' Décrit une évolution sous forme verbale à partir de deux niveaux,
#' sans tenir compte d'une éventuelle accélération, avec ou sans
#' la valeur de variation.
#'
#' @param x1 Le niveau le plus récent.
#' @param x2 Le niveau le plus ancien.
#' @param sing Indicateur logique : `TRUE` si le sujet est singulier
#'   (par défaut), `FALSE` sinon.
#' @param evolution Type d'évolution :
#'   `"pourcents"` (variation relative, par défaut) ou `"points"`.
#' @param avec_evolution Indicateur logique : `TRUE` pour ajouter la valeur
#'   de l'évolution après le verbe. Par défaut, `FALSE` retourne uniquement
#'   le verbe.
#' @param stable_sans_valeur Indicateur logique : `TRUE` (par défaut)
#'   pour ne pas ajouter la valeur lorsque la catégorie correspond
#'   à une stabilité. Si `FALSE`, la valeur est ajoutée après la
#'   formulation de stabilité, à condition que `avec_evolution = TRUE`.
#' @param lang Langue de sortie : `"fr"` ou `"en"`.
#'
#' @return
#' Une chaîne de caractères correspondant à la formulation verbale
#' retenue, avec ou sans la valeur de l'évolution, par exemple :
#' `"augmente"` ou `"augmente de 10,0 %"`.
#'
#' @details
#' La fonction calcule d'abord une évolution à partir de `x1`
#' et `x2` :
#' \itemize{
#'   \item si `evolution = "pourcents"`, elle utilise
#'   \code{\link{g}} ;
#'   \item si `evolution = "points"`, elle calcule `x1 - x2`.
#' }
#'
#' La valeur obtenue est ensuite transmise à
#' \code{\link{g_verbe_evo}}, qui détermine la formulation à partir
#' des conditions définies dans la table
#' `getOption("serad")$evo_simple`.
#'
#' Cette table doit notamment contenir les colonnes `condition`,
#' `verbe_sing` et `verbe_plur`.
#'
#' Les conditions doivent être disjointes : pour une valeur donnée,
#' une seule condition doit être vraie.
#'
#' @section Personnalisation:
#' Les formulations utilisées par cette fonction proviennent de la table
#' `getOption("serad")$evo_simple`.
#'
#' Pour modifier les conditions ou les libellés, voir
#' \code{\link{init_serad}}.
#'
#' @seealso
#' \code{\link{g_verbe_evo}},
#' \code{\link{g}},
#' \code{\link{init_serad}}
#'
#' @examples
#' g_verbe(1.1, 1)
#' g_verbe(1.1, 1, avec_evolution = TRUE)
#'
#' g_verbe(1.04, 1)
#' g_verbe(1.01, 1, sing = FALSE)
#'
#' g_verbe(0.999, 1)
#' g_verbe(
#'   0.999,
#'   1,
#'   avec_evolution = TRUE,
#'   stable_sans_valeur = FALSE
#' )
#'
#' g_verbe(0.96, 1)
#' g_verbe(0.79, 1)
#'
#' g_verbe(
#'   12,
#'   10,
#'   evolution = "points",
#'   avec_evolution = TRUE
#' )
#'
#' @export
g_verbe <- function(
    x1,
    x2,
    sing = TRUE,
    evolution = c("pourcents", "points"),
    avec_evolution = TRUE,
    stable_sans_valeur = TRUE,
    lang = get_serad_language()
) {

  evolution <- match.arg(evolution)

  valeur <- if (evolution == "pourcents") {
    serad::g(x1, x2)
  } else {
    x1 - x2
  }

  g_verbe_evo(
    g = valeur,
    sing = sing,
    evolution = evolution,
    avec_evolution = avec_evolution,
    stable_sans_valeur = stable_sans_valeur,
    lang = lang
  )
}
