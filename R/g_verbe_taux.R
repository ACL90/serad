#' Evolution verbale d'un taux
#'
#' Decrit une evolution sous forme verbale, sans tenir compte
#' d'une eventuelle acceleration, et suivie de l'expression
#' "de <variation>".
#'
#' @param g L'evolution.
#' @param sing Indicateur logique : TRUE si le sujet est singulier
#'   (par defaut), FALSE sinon.
#' @param evolution Type d'evolution :
#'   "pourcents" (variation relative) ou "points".
#' @param stable_sans_valeur Indicateur logique : TRUE (par defaut)
#'   pour ne rien ajouter apres une stabilite.
#'   Si FALSE, ajoute la valeur apres "a".
#'
#' @return
#' Une chaine de caracteres decrivant l'evolution,
#' par exemple "bondit de 10,0 %".
#'
#' @details
#' La fonction selectionne d'abord, dans la table
#' \code{getOption("serad")$evo_simple}, la premiere ligne dont
#' le seuil est inferieur strictement a \code{g}.
#'
#' Si \code{sing = TRUE}, la fonction renvoie la colonne
#' \code{verbe_sing}. Sinon, elle renvoie \code{verbe_plur}.
#'
#' La valeur numerique est ensuite formatee avec \code{\link{format_g}}
#' ou \code{\link{format_pts}} selon l'argument \code{evolution}.
#'
#' Si la formulation correspond a une stabilite et que
#' \code{stable_sans_valeur = TRUE}, seule la formulation verbale
#' est renvoyee. Sinon, la valeur formatee est ajoutee.
#'
#' @seealso \code{\link{g_verbe}}
#'
#' @examples
#' g_verbe_taux(10, TRUE)
#' g_verbe_taux(4, TRUE)
#' g_verbe_taux(1, FALSE)
#' g_verbe_taux(0.3)
#' g_verbe_taux(-0.1)
#' g_verbe_taux(-0.1, stable_sans_valeur = FALSE)
#' g_verbe_taux(-4)
#' g_verbe_taux(-21)
#'
#' @export
g_verbe_taux <- function(g,
                         sing = TRUE,
                         evolution = c("pourcents", "points"),
                         stable_sans_valeur = TRUE) {

  evolution <- match.arg(evolution)

  serad0 <- getOption("serad")
  tab <- serad0$evo_simple

  if (!is.data.frame(tab)) {
    stop("serad$evo_simple doit etre une data.frame.")
  }

  cols_attendues <- c("seuil", "verbe_sing", "verbe_plur")
  if (!all(cols_attendues %in% names(tab))) {
    stop("serad$evo_simple doit contenir : seuil, verbe_sing, verbe_plur.")
  }

  i <- which(g > tab$seuil)[1]
  if (is.na(i)) i <- nrow(tab)

  verbe <- if (sing) as.character(tab$verbe_sing[i]) else as.character(tab$verbe_plur[i])

  est_stable <- verbe %in% c("est stable", "sont stables")

  format_fun <- if (evolution == "pourcents") format_g else format_pts

  val <- if (g < 0 && est_stable) {
    format_fun(g, signe = TRUE)
  } else {
    format_fun(g, signe = FALSE)
  }

  if (est_stable && stable_sans_valeur) {
    return(verbe)
  }

  if (est_stable) {
    return(paste(verbe, "\u00e0", val))
  }

  paste(verbe, val)
}
