#' Produit le libellé d'une période
#'
#' Formate un mois ou un trimestre, éventuellement décalé dans le temps,
#' en français ou en anglais.
#'
#' @param numero Numéro du mois (1 à 12) ou du trimestre (1 à 4).
#' @param annee Année de la période.
#' @param periode Type de période : `"mois"` ou `"trimestre"`.
#' @param decalage Nombre de périodes à ajouter. Une valeur positive avance
#'   dans le temps et une valeur négative permet de revenir en arrière.
#' @param format Format du libellé : `"lettres"` ou `"chiffres"`.
#'   Pour les mois, cet argument ne modifie pas le résultat.
#' @param avec_annee Indicateur logique : `TRUE` pour afficher l'année,
#'   `FALSE` pour ne pas l'afficher.
#' @param lang Langue de sortie : `"fr"` ou `"en"`.
#'
#' @return
#' Une chaîne de caractères représentant la période demandée.
#'
#' @examples
#' libelle_periode(8, 2026, periode = "mois")
#' libelle_periode(8, 2026, periode = "mois", decalage = 23)
#' libelle_periode(4, 2026, periode = "trimestre")
#' libelle_periode(
#'   4,
#'   2026,
#'   periode = "trimestre",
#'   decalage = 1,
#'   format = "chiffres"
#' )
#' libelle_periode(
#'   1,
#'   2026,
#'   periode = "trimestre",
#'   avec_annee = FALSE,
#'   lang = "en"
#' )
#'
#' @export
libelle_periode <- function(numero,
                            annee,
                            periode = c("mois", "trimestre"),
                            decalage = 0L,
                            format = c("lettres", "chiffres"),
                            avec_annee = TRUE,
                            lang = get_serad_language()) {

  periode <- match.arg(periode)
  format <- match.arg(format)
  lang <- match.arg(lang, c("fr", "en"))

  if (!is.numeric(numero) ||
      length(numero) != 1L ||
      is.na(numero) ||
      !is.finite(numero) ||
      numero %% 1 != 0) {
    stop("`numero` doit \u00EAtre un entier.", call. = FALSE)
  }

  if (!is.numeric(annee) ||
      length(annee) != 1L ||
      is.na(annee) ||
      !is.finite(annee) ||
      annee %% 1 != 0) {
    stop("`annee` doit \u00EAtre un entier.", call. = FALSE)
  }

  if (!is.numeric(decalage) ||
      length(decalage) != 1L ||
      is.na(decalage) ||
      !is.finite(decalage) ||
      decalage %% 1 != 0) {
    stop("`decalage` doit \u00EAtre un entier.", call. = FALSE)
  }

  if (!is.logical(avec_annee) ||
      length(avec_annee) != 1L ||
      is.na(avec_annee)) {
    stop(
      "`avec_annee` doit \u00EAtre \u00E9gal \u00E0 TRUE ou FALSE.",
      call. = FALSE
    )
  }

  numero <- as.integer(numero)
  annee <- as.integer(annee)
  decalage <- as.integer(decalage)

  maximum <- if (periode == "mois") 12L else 4L

  if (!numero %in% seq_len(maximum)) {
    stop(
      "`numero` doit \u00EAtre compris entre 1 et ",
      maximum,
      " pour la p\u00E9riode s\u00E9lectionn\u00E9e.",
      call. = FALSE
    )
  }

  total <- numero - 1L + decalage

  numero <- total %% maximum + 1L
  annee <- annee + total %/% maximum

  if (periode == "mois") {

    libelle <- if (lang == "fr") {
      c(
        "janvier",
        "f\u00E9vrier",
        "mars",
        "avril",
        "mai",
        "juin",
        "juillet",
        "ao\u00FBt",
        "septembre",
        "octobre",
        "novembre",
        "d\u00E9cembre"
      )[numero]
    } else {
      c(
        "January",
        "February",
        "March",
        "April",
        "May",
        "June",
        "July",
        "August",
        "September",
        "October",
        "November",
        "December"
      )[numero]
    }

  } else if (format == "lettres") {

    ordinal <- if (lang == "fr") {
      c(
        "premier",
        "deuxi\u00E8me",
        "troisi\u00E8me",
        "quatri\u00E8me"
      )[numero]
    } else {
      c(
        "first",
        "second",
        "third",
        "fourth"
      )[numero]
    }

    nom_periode <- if (lang == "fr") {
      "trimestre"
    } else {
      "quarter"
    }

    libelle <- paste(ordinal, nom_periode)

  } else {

    suffixe <- if (lang == "fr") {
      if (numero == 1L) "er" else "e"
    } else {
      c("st", "nd", "rd", "th")[numero]
    }

    ordinal <- paste0(
      numero,
      "^",
      suffixe,
      "^"
    )

    nom_periode <- if (lang == "fr") {
      "trimestre"
    } else {
      "quarter"
    }

    libelle <- paste(ordinal, nom_periode)
  }

  if (avec_annee) {
    libelle <- paste(libelle, annee)
  }

  libelle
}
