#' Évolution verbale d'un taux
#'
#' @description
#' Décrit une évolution sous forme verbale, sans tenir compte
#' d'une éventuelle accélération, avec ou sans la valeur formatée.
#'
#' @param g Valeur de l'évolution.
#' @param sing Indicateur logique : `TRUE` si le sujet est singulier
#'   (par défaut), `FALSE` sinon.
#' @param evolution Type d'évolution :
#'   `"pourcents"` (variation relative, par défaut) ou `"points"`.
#' @param avec_evolution Indicateur logique : `TRUE` pour ajouter la valeur
#'   de l'évolution après le verbe, `FALSE` pour retourner uniquement
#'   le verbe.
#' @param stable_sans_valeur Indicateur logique : `TRUE` (par défaut)
#'   pour ne pas ajouter la valeur lorsque la catégorie correspond
#'   à une stabilité. Si `FALSE`, la valeur est ajoutée après la
#'   formulation de stabilité, à condition que `avec_evolution = TRUE`.
#' @param lang Langue de sortie : `"fr"` ou `"en"`.
#'
#' @return
#' Une chaîne de caractères décrivant l'évolution, avec ou sans sa valeur.
#'
#' @details
#' La fonction sélectionne, dans la table
#' `getOption("serad")$evo_simple`, la ligne dont la condition est vérifiée
#' par la valeur de `g`.
#'
#' La table `evo_simple` doit contenir une colonne `condition`, composée de
#' chaînes de caractères évaluables par R, par exemple :
#' `"g >= -0.10 & g <= 0.10"`.
#'
#' Les conditions doivent être disjointes : pour une valeur donnée de `g`,
#' une seule condition doit être vraie.
#'
#' Si `sing = TRUE`, la fonction utilise la colonne `verbe_sing`.
#' Sinon, elle utilise la colonne `verbe_plur`.
#'
#' Si `avec_evolution = FALSE`, la préposition placée à la fin de la
#' formulation verbale (`"de"`, `"à"`, `"by"` ou `"at"`) est supprimée.
#'
#' Si `avec_evolution = TRUE`, la valeur est formatée avec
#' \code{\link{format_taux}} lorsque `evolution = "pourcents"` et avec
#' \code{\link{format_pts}} lorsque `evolution = "points"`.
#'
#' Lorsqu'une évolution appartient à la catégorie de stabilité et que
#' `stable_sans_valeur = TRUE`, seule la formulation verbale est retournée.
#'
#' @section Personnalisation:
#' Les formulations utilisées par cette fonction proviennent de la table
#' `getOption("serad")$evo_simple`.
#'
#' Pour modifier les conditions ou les libellés, voir
#' \code{\link{init_serad}}.
#'
#' @seealso
#' \code{\link{g_verbe}},
#' \code{\link{g_nom_evo}},
#' \code{\link{format_taux}},
#' \code{\link{format_pts}},
#' \code{\link{init_serad}}
#'
#' @examples
#' g_verbe_evo(10)
#' g_verbe_evo(10, avec_evolution = FALSE)
#'
#' g_verbe_evo(2, evolution = "points")
#' g_verbe_evo(2, evolution = "points", avec_evolution = FALSE)
#'
#' g_verbe_evo(0.1)
#' g_verbe_evo(-0.1)
#' g_verbe_evo(-0.1, stable_sans_valeur = FALSE)
#'
#' g_verbe_evo(10, sing = FALSE)
#' g_verbe_evo(10, lang = "en")
#'
#' @export
g_verbe_evo <- function(
    g,
    sing = TRUE,
    evolution = c("pourcents", "points"),
    avec_evolution = TRUE,
    stable_sans_valeur = TRUE,
    lang = get_serad_language()
) {

  evolution <- match.arg(evolution)

  if (!lang %in% c("fr", "en")) {
    stop("`lang` doit \u00eatre \u00e9gal \u00e0 \"fr\" ou \"en\".")
  }

  if (!is.logical(sing) ||
      length(sing) != 1 ||
      is.na(sing)) {
    stop("`sing` doit \u00eatre \u00e9gal \u00e0 TRUE ou FALSE.")
  }

  if (!is.logical(avec_evolution) ||
      length(avec_evolution) != 1 ||
      is.na(avec_evolution)) {
    stop("`avec_evolution` doit \u00eatre \u00e9gal \u00e0 TRUE ou FALSE.")
  }

  if (!is.logical(stable_sans_valeur) ||
      length(stable_sans_valeur) != 1 ||
      is.na(stable_sans_valeur)) {
    stop("`stable_sans_valeur` doit \u00eatre \u00e9gal \u00e0 TRUE ou FALSE.")
  }

  serad0 <- getOption("serad")

  if (is.null(serad0) || is.null(serad0$evo_simple)) {
    stop(
      paste0(
        "Les options serad ne sont pas initialis\u00e9es. ",
        "Utiliser init_serad_fr() ou init_serad_en()."
      )
    )
  }

  tab <- serad0$evo_simple

  # ---- vérification de la table ----
  if (!is.data.frame(tab)) {
    stop("serad$evo_simple doit \u00eatre une data.frame.")
  }

  cols_attendues <- c(
    "condition",
    "verbe_sing",
    "verbe_plur"
  )

  if (!all(cols_attendues %in% names(tab))) {
    stop(
      paste0(
        "serad$evo_simple doit contenir : ",
        "condition, verbe_sing, verbe_plur."
      )
    )
  }

  # ---- sélection de la catégorie ----
  test_conditions <- vapply(
    tab$condition,
    function(condition) {
      eval(
        parse(text = condition),
        envir = list(g = g)
      )
    },
    logical(1)
  )

  i <- which(test_conditions)

  if (length(i) == 0) {
    stop(
      "Aucune cat\u00e9gorie trouv\u00e9e pour g = ",
      g,
      call. = FALSE
    )
  }

  if (length(i) > 1) {
    stop(
      "Plusieurs cat\u00e9gories trouv\u00e9es pour g = ",
      g,
      ". Les conditions de serad$evo_simple ne sont pas disjointes.",
      call. = FALSE
    )
  }

  # ---- sélection du verbe ----
  verbe <- if (sing) {
    as.character(tab$verbe_sing[i])
  } else {
    as.character(tab$verbe_plur[i])
  }

  # ---- détection de la stabilité ----
  est_stable <- isTRUE(
    eval(
      parse(text = tab$condition[i]),
      envir = list(g = 0)
    )
  )

  # ---- rendu sans valeur ----
  sans_valeur <- !avec_evolution ||
    (est_stable && stable_sans_valeur)

  if (sans_valeur) {
    if (lang == "fr") {
      verbe <- sub(
        "\\s+(de|\u00e0)$",
        "",
        verbe,
        ignore.case = TRUE
      )
    } else {
      verbe <- sub(
        "\\s+(by|at)$",
        "",
        verbe,
        ignore.case = TRUE
      )
    }

    return(verbe)
  }

  # ---- formatage de la valeur ----
  format_fun <- switch(
    evolution,
    pourcents = format_taux,
    points = format_pts
  )

  val <- if (g < 0 && est_stable) {
    format_fun(
      g,
      signe = TRUE,
      lang = lang
    )
  } else {
    format_fun(
      g,
      signe = FALSE,
      lang = lang
    )
  }

  # ---- rendu avec valeur ----
  paste(verbe, val)
}
