#' Evolution nominale d'un taux
#'
#' Decrit une evolution exprimee en pourcentage sous forme nominale
#' (par exemple : "une forte hausse").
#'
#' @param g L'evolution en pourcentage.
#' @param titre Indicateur logique : TRUE pour supprimer l'article
#'   initial et mettre une majuscule, notamment en debut de titre.
#'
#' @return
#' Une chaine de caracteres decrivant l'evolution
#' (par exemple : "une forte hausse", "une stabilite").
#'
#' @details
#' La fonction selectionne d'abord, dans la table
#' \code{getOption("serad")$evo_simple}, la premiere ligne dont
#' le seuil est inferieur strictement a \code{g}.
#'
#' La fonction renvoie alors la colonne \code{nom} correspondante.
#' Si \code{titre = TRUE}, l'article initial est supprime et la
#' premiere lettre restante est mise en majuscule.
#'
#' @examples
#' g_nom_taux(4)
#' g_nom_taux(1)
#' g_nom_taux(0.4)
#' g_nom_taux(0.1)
#' g_nom_taux(0)
#' g_nom_taux(-0.3)
#' g_nom_taux(-1)
#' g_nom_taux(-4)
#' g_nom_taux(-5)
#'
#' @seealso \code{\link{g_verbe_taux}}
#'
#' @export
g_nom_taux <- function(g, titre = FALSE) {

  serad0 <- getOption("serad")
  tab    <- serad0$evo_simple

  if (!is.data.frame(tab)) {
    stop("serad$evo_simple doit etre une data.frame.")
  }

  cols_attendues <- c("seuil", "nom")
  if (!all(cols_attendues %in% names(tab))) {
    stop("serad$evo_simple doit contenir : seuil, nom.")
  }

  i <- which(g > tab$seuil)[1]
  if (is.na(i)) i <- nrow(tab)

  res <- as.character(tab$nom[i])

  if (titre) {
    res <- sub(
      "^(une|un|des|la|le|les|du|de la|de l'|d'|l')\\s*",
      "",
      res
    )

    res <- paste0(
      toupper(substr(res, 1, 1)),
      substr(res, 2, nchar(res))
    )
  }

  res
}
