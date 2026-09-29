#' Évolution nominale
#'
#' @description
#' Décrit une évolution sous forme nominale, avec ou sans la valeur
#' de l'évolution.
#'
#' @param g Valeur de l'évolution.
#' @param evolution Type d'évolution :
#'   `"pourcents"` (par défaut) ou `"points"`.
#' @param avec_evolution Indicateur logique : `TRUE` pour ajouter la valeur
#'   de l'évolution à la formulation nominale, `FALSE` pour retourner
#'   uniquement la formulation.
#' @param titre Indicateur logique : `TRUE` pour supprimer l'article
#'   initial et mettre une majuscule, notamment en début de titre.
#' @param lang Langue de sortie : `"fr"` ou `"en"`.
#'
#' @return
#' Une chaîne de caractères décrivant l'évolution, avec ou sans sa valeur
#' (par exemple : `"une forte hausse"`, `"une forte hausse de 4 %"` ou
#' `"une forte hausse de 2 points"`).
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
#' La fonction renvoie ensuite la colonne `nom` correspondante.
#'
#' Si `titre = TRUE`, l'article initial est supprimé et la première lettre
#' restante est mise en majuscule.
#'
#' Si `avec_evolution = TRUE`, la valeur absolue de `g` est ajoutée à la
#' formulation avec l'unité correspondant à `evolution`. La valeur absolue
#' est utilisée car le sens de l'évolution est déjà exprimé par le nom :
#' par exemple, `"une baisse de 4 %"` et non `"une baisse de -4 %"`.
#'
#' @section Personnalisation:
#' Les formulations utilisées par cette fonction proviennent de la table
#' `getOption("serad")$evo_simple`.
#'
#' Pour modifier les conditions ou les libellés, voir
#' \code{\link{init_serad}}.
#'
#' @examples
#' g_nom_evo(4)
#' g_nom_evo(4, avec_evolution = TRUE)
#'
#' g_nom_evo(1.5, evolution = "points")
#' g_nom_evo(1.5, evolution = "points", avec_evolution = TRUE)
#' g_nom_evo(-2, evolution = "points", avec_evolution = TRUE)
#'
#' g_nom_evo(4, avec_evolution = TRUE, titre = TRUE)
#' g_nom_evo(4, avec_evolution = TRUE, lang = "en")
#'
#' @seealso
#' \code{\link{g_nom}},
#' \code{\link{g_verbe_evo}},
#' \code{\link{pluriel}},
#' \code{\link{init_serad}}
#'
#' @export
g_nom_evo <- function(g,
                       evolution = c("pourcents", "points"),
                       avec_evolution = FALSE,
                       titre = FALSE,
                       lang = get_serad_language()) {

  evolution <- match.arg(evolution)

  if (!lang %in% c("fr", "en")) {
    stop("`lang` doit \u00eatre \u00e9gal \u00e0 \"fr\" ou \"en\".")
  }

  if (!is.logical(avec_evolution) ||
      length(avec_evolution) != 1 ||
      is.na(avec_evolution)) {
    stop("`avec_evolution` doit \u00eatre \u00e9gal \u00e0 TRUE ou FALSE.")
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

  if (!is.data.frame(tab)) {
    stop("serad$evo_simple doit \u00eatre une data.frame.")
  }

  cols_attendues <- c("condition", "nom")

  if (!all(cols_attendues %in% names(tab))) {
    stop("serad$evo_simple doit contenir : condition, nom.")
  }

  # ---- sélection ----
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

  # ---- formulation nominale ----
  res <- as.character(tab$nom[i])

  # ---- mise en forme pour un titre ----
  if (titre) {
    if (lang == "en") {
      res <- sub(
        "^(a|an|the)\\s+",
        "",
        res,
        ignore.case = TRUE
      )
    } else {
      res <- sub(
        "^(une|un|des|la|le|les|du|de la|de l'|d'|l')\\s*",
        "",
        res,
        ignore.case = TRUE
      )
    }

    res <- paste0(
      toupper(substr(res, 1, 1)),
      substr(res, 2, nchar(res))
    )
  }

  # ---- ajout de la valeur de l'évolution ----
  if (avec_evolution) {
    valeur_affichee <- format(
      abs(g),
      trim = TRUE,
      scientific = FALSE,
      decimal.mark = if (lang == "fr") "," else "."
    )

    unite <- switch(
      evolution,

      pourcents = if (lang == "fr") {
        "\u00a0%"
      } else {
        "%"
      },

      points = pluriel(
        abs(g),
        sing = " point",
        plur = " points",
        lang = lang
      )
    )

    res <- paste0(
      res,
      if (lang == "fr") " de " else " of ",
      valeur_affichee,
      unite
    )
  }

  res
}
