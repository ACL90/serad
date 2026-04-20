#' Evolution nominale tenant compte de l'acceleration
#'
#' Decrit l'evolution en tenant compte de l'acceleration
#' entre deux variations successives.
#'
#' @param g1 Derniere evolution, exprimee en pourcentage.
#' @param g2 Evolution precedente, exprimee en pourcentage.
#' @param titre Indicateur logique : TRUE pour supprimer l'article
#'   initial et mettre une majuscule, notamment en debut de titre.
#' @param alea Parametre numerique compris entre 0 et 1 controlant
#' l'utilisation de formulations alternatives. Si \code{alea = 0},
#' la formulation est deterministe. Si \code{alea = 1}, la formulation
#' alternative est toujours utilisee. Des valeurs intermediaires
#' permettent un tirage aleatoire.
#'
#' @return
#' Une chaine de caracteres correspondant a la formulation
#' nominale retenue (par exemple : "une acceleration",
#' "une stabilisation").
#'
#' @details
#' La fonction calcule d'abord l'acceleration entre \code{g1}
#' et \code{g2} a l'aide de \code{\link{g}}.
#'
#' Elle parcourt ensuite la table \code{getOption("serad")$evo_accel}
#' ligne par ligne. Chaque ligne contient un ensemble de conditions
#' sur \code{g1}, \code{g2} et l'acceleration. La premiere ligne dont
#' toutes les conditions sont verifiees determine la formulation
#' retenue.
#'
#' Une formulation alternative peut etre utilisee via la table
#' \code{getOption("serad")$evo_accel_alt}, qui contient une variante
#' pour chaque ligne de \code{evo_accel}. Le choix entre la formulation
#' principale et la variante depend du parametre \code{alea}.
#'
#' Si \code{titre = TRUE}, l'article initial est supprime et la
#' premiere lettre restante est mise en majuscule.
#'
#' @examples
#' gETa_nom_taux(0.049, 0.049)
#' gETa_nom_taux(10, 1)
#' gETa_nom_taux(-4, 1, titre = TRUE)
#' gETa_nom_taux(-21, 1)
#' gETa_nom_taux(10, 1, alea = 0.5)
#'
#' @seealso \code{\link{g}}, \code{\link{gETa_verbe_taux}}
#'
#' @importFrom stats runif
#' @export
gETa_nom_taux <- function(g1, g2, titre = FALSE, alea = 0) {

  serad <- getOption("serad")
  tab   <- serad$evo_accel
  tab_alt <- serad$evo_accel_alt
  seuil <- serad$seuil

  a <- serad::g(g1, g2)

  # fonction locale (évite dépendance au init)
  pick <- function(base, alt, alea) {
    if (alea == 0 || is.na(alt) || alt == "") return(base)
    if (runif(1) < alea) alt else base
  }

  env <- list2env(c(
    list(g1 = g1, g2 = g2, a = a),
    list(
      seuil_stable       = seuil$stable,
      seuil_g2_bas       = seuil$g2_bas,
      seuil_g2_haut      = seuil$g2_haut,
      seuil_g1_bas       = seuil$g1_bas,
      seuil_g1_tres_bas  = seuil$g1_tres_bas,
      seuil_accel_hausse = seuil$accel_hausse,
      seuil_accel_baisse = seuil$accel_baisse,
      seuil_accel_recul  = seuil$accel_recul
    )
  ))

  for (i in seq_len(nrow(tab))) {

    cond <- c(tab$cond_g1[i], tab$cond_g2[i], tab$cond_a[i])
    cond <- cond[cond != "TRUE"]

    results <- sapply(cond, function(x) eval(parse(text = x), envir = env))

    if (length(results) == 0 || all(results)) {

      res <- pick(
        base = as.character(tab$nom[i]),
        alt  = as.character(tab_alt$nom_alt[i]),
        alea = alea
      )

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

      return(res)
    }
  }

  NA_character_
}
