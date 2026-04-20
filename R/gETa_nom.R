#' Evolution nominale tenant compte de l'acceleration
#'
#' Decrit l'evolution sous forme nominale a partir de trois niveaux,
#' en tenant compte de l'acceleration entre deux variations successives.
#'
#' @param x1 Niveau le plus recent.
#' @param x2 Niveau precedent.
#' @param x3 Niveau le plus ancien.
#' @param titre Indicateur logique : TRUE pour supprimer l'article
#'   initial et mettre une majuscule, notamment en debut de titre.
#' @param alea Parametre numerique compris entre 0 et 1 controlant
#' l'utilisation de formulations alternatives. Si \code{alea = 0},
#' la formulation est deterministe. Si \code{alea = 1}, la formulation
#' alternative est toujours utilisee. Des valeurs intermediaires
#' permettent un tirage aleatoire.
#'
#' @return
#' Une chaine de caracteres correspondant a la formulation nominale
#' retenue.
#'
#' @details
#' La fonction calcule d'abord deux evolutions successives :
#' \code{g1 <- serad::g(x1, x2)} et \code{g2 <- serad::g(x2, x3)}.
#'
#' Ces deux variations sont ensuite transmises a
#' \code{\link{gETa_nom_taux}}, qui calcule leur acceleration a l'aide
#' de \code{\link{g}} et determine la formulation nominale appropriee
#' a partir de la table \code{getOption("serad")$evo_accel}.
#'
#' Une formulation alternative peut etre utilisee via la table
#' \code{getOption("serad")$evo_accel_alt}. Le choix entre la
#' formulation principale et la variante depend du parametre
#' \code{alea}.
#'
#' Si \code{titre = TRUE}, l'article initial est supprime et la
#' premiere lettre restante est mise en majuscule.
#'
#' @seealso \code{\link{gETa_nom_taux}}, \code{\link{g}}
#'
#' @examples
#' gETa_nom(1.00049, 1, 0.9996)
#' gETa_nom(1.1, 1, 0.99)
#' gETa_nom(1.003, 1, 0.99)
#' gETa_nom(0.96, 1, 1.01)
#' gETa_nom(0.8, 1, 0.99)
#' gETa_nom(1.1, 1, 0.99, alea = 0.5)
#'
#' @export
gETa_nom <- function(x1, x2, x3, titre = FALSE, alea = 0) {

  g1 <- serad::g(x1, x2)
  g2 <- serad::g(x2, x3)

  gETa_nom_taux(g1, g2, titre = titre, alea = alea)
}
