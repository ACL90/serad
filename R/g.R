#' Calcul d'une variation relative
#'
#' Calcule la variation relative entre \code{x1} et \code{x2},
#' exprimee en pourcentage.
#'
#' @param x1 Niveau le plus recent.
#' @param x2 Niveau le plus ancien.
#' @param eps Valeur utilisee a la place de \code{x2} lorsque
#'   \code{x2 = 0}, afin d'eviter une division par zero.
#'   Par defaut : \code{1e-8}.
#'
#' @return
#' Une valeur numerique correspondant a la variation en pourcentage
#' (par exemple, si x1 = 2 * x2, la fonction retourne 100).
#'
#' @seealso \code{\link{format_g}}
#'
#' @details
#' Si \code{x2 = 0}, la valeur epsilon definie par
#' getOption("serad")$eps est utilisee afin d'eviter une division
#' par zero. Un message d'avertissement est emis dans ce cas.
#'
#' Il est possible de modifier cette valeur :
#'
#' \code{
#' serad0 <- getOption("serad")
#' serad0$eps <- 0
#' options(serad = serad0)
#' }
#'
#' @examples
#' g(2, 1)  # 100
#' g(2, 0)  # valeur tres elevee et avertissement
#'
#' @export
g <- function(x1, x2, eps = 1e-8) {

  zero_denom <- (x2 == 0)

  if (any(zero_denom, na.rm = TRUE)) {
    warning("division par 0 dans serad::g()")
  }

  x2_adj <- ifelse(zero_denom, eps, x2)

  100 * (x1 / x2_adj - 1)
}
