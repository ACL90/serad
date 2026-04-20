#' Evolution verbale ne tenant pas compte de l'acceleration et suivie de la valeur
#'
#' Decrit une evolution sous forme verbale a partir de deux niveaux,
#' sans tenir compte d'une eventuelle acceleration, et en ajoutant
#' la valeur de variation.
#'
#' @param x1 Le niveau le plus recent.
#' @param x2 Le niveau le plus ancien.
#' @param sing Indicateur logique : TRUE si le sujet est singulier
#'   (par defaut), FALSE sinon.
#' @param evolution Type d'evolution :
#'   "pourcents" (variation relative, par defaut) ou "points".
#' @param stable_sans_valeur Indicateur logique.
#'   TRUE (par defaut) : n'affiche pas la valeur en cas de stabilite.
#'   FALSE : affiche la valeur apres "a"
#'   (par exemple : "est stable a -0,1 %").
#'
#' @return
#' Une chaine de caracteres correspondant a la formulation verbale
#' retenue, par exemple : "bondit de 10,0 %".
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
#' La valeur obtenue est ensuite transmise a \code{\link{g_verbe_taux}},
#' qui determine la formulation a partir de la table
#' \code{getOption("serad")$evo_simple}.
#'
#' @seealso \code{\link{g_verbe_taux}}, \code{\link{g}}
#'
#' @examples
#' g_verbe(1.1, 1)
#' g_verbe(1.04, 1)
#' g_verbe(1.01, 1, sing = FALSE)
#' g_verbe(1.003, 1)
#' g_verbe(0.999, 1)
#' g_verbe(0.999, 1, stable_sans_valeur = FALSE)
#' g_verbe(0.96, 1)
#' g_verbe(0.79, 1)
#'
#' @export
g_verbe <- function(x1, x2,
                    sing = TRUE,
                    evolution = c("pourcents", "points"),
                    stable_sans_valeur = TRUE) {

  evolution <- match.arg(evolution)

  valeur <- if (evolution == "pourcents") {
    serad::g(x1, x2)
  } else {
    x1 - x2
  }

  g_verbe_taux(
    valeur,
    sing = sing,
    evolution = evolution,
    stable_sans_valeur = stable_sans_valeur
  )
}
