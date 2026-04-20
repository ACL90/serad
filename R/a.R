#' Calcul d'une acceleration
#'
#' Calcule l'acceleration entre trois niveaux successifs en comparant
#' les taux de variation consecutifs.
#'
#' L'acceleration correspond a la variation du taux entre
#' \code{(x1, x2)} et \code{(x2, x3)}.
#'
#' @param x1 Niveau le plus recent.
#' @param x2 Niveau precedent.
#' @param x3 Niveau le plus ancien.
#'
#' @return
#' Un nombre numerique correspondant a l'acceleration en pourcentage.
#'
#' @seealso \code{\link{g}}, \code{\link{format_g}}
#'
#' @examples
#' a(4, 2, 1)  # 0
#' a(6, 2, 1)  # 100
#'
#' \dontrun{
#' a(2, 1, 1)  # valeur tres elevee si taux precedent proche de zero
#' }
#'
#' @export
a <- function(x1, x2, x3) {

  g12 <- serad::g(x1, x2)
  g23 <- serad::g(x2, x3)

  serad::g(g12, g23)
}

