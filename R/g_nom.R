#' Evolution nominale non suivie d'une valeur
#'
#' Decrit une evolution sous forme nominale a partir de deux niveaux,
#' sans ajouter la valeur de variation.
#'
#' @param x1 Le niveau le plus recent.
#' @param x2 Le niveau le plus ancien.
#' @param evolution Type d'evolution :
#'   "pourcents" (variation relative, par defaut) ou "points".
#' @param titre Indicateur logique : TRUE pour supprimer l'article
#'   initial et mettre une majuscule, notamment en debut de titre.
#'
#' @return
#' Une chaine de caracteres correspondant a la formulation nominale
#' retenue (par exemple : "une forte hausse").
#'
#' @details
#' La fonction calcule d'abord une evolution a partir de \code{x1}
#' et \code{x2} :
#' \itemize{
#'   \item si \code{evolution = "pourcents"}, elle utilise
#'   \code{\link{g}} ;
#'   \item si \code{evolution = "points"}, elle calcule \code{x1 - x2}.
#' }
#'
#' La valeur obtenue est ensuite transmise a \code{\link{g_nom_taux}},
#' qui determine la formulation a partir de la table
#' \code{getOption("serad")$evo_simple}.
#'
#' @seealso \code{\link{g_nom_taux}}, \code{\link{g}}
#'
#' @examples
#' g_nom(1.04, 1)
#' g_nom(1.01, 1)
#' g_nom(1.004, 1)
#' g_nom(1.001, 1)
#' g_nom(1, 1)
#' g_nom(0.997, 1)
#' g_nom(0.95, 1)
#' g_nom(0.95, 1, evolution = "points")
#'
#' @export
g_nom <- function(x1, x2,
                  evolution = c("pourcents", "points"),
                  titre = FALSE) {

  evolution <- match.arg(evolution)

  valeur <- switch(evolution,
                   pourcents = serad::g(x1, x2),
                   points    = x1 - x2
  )

  g_nom_taux(g = valeur, titre = titre)
}
