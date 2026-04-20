#' Comparaison qualitative entre deux niveaux
#'
#' Compare deux niveaux successifs et retourne une formulation
#' selon l'evolution observee.
#'
#' @param x1 Niveau le plus recent.
#' @param x2 Niveau le plus ancien.
#' @param hausse_defaut Formulation en cas de hausse.
#' @param egalite_defaut Formulation en cas de stabilite.
#' @param baisse_defaut Formulation en cas de baisse.
#' @param seuil Seuil d'egalite en valeur absolue. Par defaut : 0.1.
#' @param alt Indicateur logique permettant d'utiliser
#'   une formulation alternative.
#' @param hausse_alt Formulation alternative en cas de hausse.
#' @param egalite_alt Formulation alternative en cas de stabilite.
#' @param baisse_alt Formulation alternative en cas de baisse.
#'
#' @details
#' La comparaison repose sur le taux de variation calcule via \code{\link{g}}.
#' Des cas particuliers sont traites lorsque \code{x2} est nul ou negatif.
#'
#' @return
#' Une chaine de caracteres correspondant a la formulation retenue.
#'
#' @seealso \code{\link{comparaison_taux}}, \code{\link{g}}
#'
#' @examples
#' comparaison(1.04, 1, "augmente", "reste stable", "diminue")
#' comparaison(0.9991, 1, "augmente", "reste stable", "diminue")
#' comparaison(1, 1, "augmente", "reste egal", "diminue", seuil = 0)
#'
#' @export
comparaison <- function(x1, x2,
                        hausse_defaut,
                        egalite_defaut,
                        baisse_defaut,
                        seuil = 0.1,
                        alt = 0,
                        hausse_alt  = hausse_defaut,
                        egalite_alt = egalite_defaut,
                        baisse_alt  = baisse_defaut) {

  # Choix des libelles selon alt
  hausse  <- if (alt == 0) hausse_defaut  else hausse_alt
  egalite <- if (alt == 0) egalite_defaut else egalite_alt
  baisse  <- if (alt == 0) baisse_defaut  else baisse_alt

  # ---- Cas 1 : x2 == 0 ----
  if (x2 == 0) {

    if (x1 > abs(seuil)) {
      return(hausse)

    } else if (x1 < -abs(seuil)) {
      return(baisse)

    } else {
      return(egalite)
    }
  }

  # ---- Cas 2 : x2 > 0 ----
  if (x2 > 0) {
    return(
      comparaison_taux(
        g(x1, x2),
        hausse_defaut, egalite_defaut, baisse_defaut,
        seuil, alt,
        hausse_alt, egalite_alt, baisse_alt
      )
    )
  }

  # ---- Cas 3 : x2 < 0 & x1 > 0 ----
  if (x2 < 0 && x1 > 0) {
    return(
      comparaison_taux(
        g(-x2, -x1),
        hausse_defaut, egalite_defaut, baisse_defaut,
        seuil, alt,
        hausse_alt, egalite_alt, baisse_alt
      )
    )
  }

  # ---- Cas 4 : x2 < 0 & x1 <= 0 ----
  if ((x1 - x2) <= abs(seuil)) {
    return(egalite)
  } else {
    return(hausse)
  }
}


#' Position relative entre deux niveaux
#'
#' Indique si \code{x1} est au-dessus ou en dessous de \code{x2}.
#'
#' @param x1 Niveau le plus recent.
#' @param x2 Niveau le plus ancien.
#'
#' @return
#' Une chaine de caracteres : "au-dessus" ou "en dessous".
#'
#' @seealso \code{\link{comparaison}}
#'
#' @examples
#' audessus(1.04, 1)     # "au-dessus"
#' audessus(1.04, 1.04)  # "au-dessus"
#' audessus(0.96, 1)     # "en dessous"
#'
#' @export
audessus <- function(x1, x2) {
  comparaison(
    x1, x2,
    hausse_defaut  = "au-dessus",
    egalite_defaut = "au-dessus",
    baisse_defaut  = "en dessous",
    seuil = 0
  )
}


#' Evolution a la hausse, a la baisse ou inchangee
#'
#' Indique si \code{x1} evolue a la hausse, a la baisse ou reste
#' inchange par rapport a \code{x2}.
#'
#' @param x1 Niveau le plus recent.
#' @param x2 Niveau le plus ancien.
#' @param seuil Seuil d'egalite en valeur absolue.
#'   Par defaut : 0.1 (soit 0.1 %).
#'
#' @return
#' Une chaine de caracteres : "a la hausse", "a la baisse"
#' ou "inchange".
#'
#' @seealso \code{\link{comparaison}}
#'
#' @examples
#' alahausse(1.004, 1)    # "à la hausse"
#' alahausse(0.996, 1)    # "à la baisse"
#' alahausse(1, 1.0004)   # "inchangé"
#'
#' @export
alahausse <- function(x1, x2, seuil = 0.1) {
  comparaison(
    x1, x2,
    hausse_defaut  = "\u00e0 la hausse",
    egalite_defaut = "inchang\u00e9",
    baisse_defaut  = "\u00e0 la baisse",
    seuil    = seuil
  )
}

#' Davantage ou moins
#'
#' Indique si \code{x1} est davantage ou moins eleve que \code{x2}.
#'
#' @param x1 Niveau le plus recent.
#' @param x2 Niveau le plus ancien.
#'
#' @return
#' Une chaine de caracteres : "davantage" ou "moins".
#'
#' @seealso \code{\link{comparaison}}
#'
#' @examples
#' davantage(1.04, 1)     # "davantage"
#' davantage(1.04, 1.04)  # "davantage"
#' davantage(0.96, 1)     # "moins"
#'
#' @export
davantage <- function(x1, x2) {
  comparaison(
    x1, x2,
    hausse_defaut  = "davantage",
    egalite_defaut = "davantage",
    baisse_defaut  = "moins",
    seuil = 0
  )
}

#' Verbe pour exprimer le sens de l'evolution
#'
#' Indique si \code{x1} excede, est au niveau de ou est en dessous de
#' \code{x2}, en tenant compte du nombre grammatical.
#'
#' @param x1 Le niveau le plus recent.
#' @param x2 Le niveau le plus ancien.
#' @param sing Indicateur logique : FALSE si le sujet est pluriel,
#'   TRUE s'il est singulier (par defaut : pluriel).
#'
#' @return
#' Une chaine de caracteres :
#' "excedent"/"excede",
#' "sont au niveau de"/"est au niveau de",
#' ou "sont en dessous de"/"est en dessous de".
#'
#' @seealso \code{\link{comparaison}}
#'
#' @examples
#' depasse(1.04, 1)               # "excèdent"
#' depasse(1.04, 1, sing = TRUE)  # "excède"
#' depasse(0.96, 1)               # "sont en dessous de"
#' depasse(0.9991, 1)             # "sont au niveau de"
#'
#' @export
depasse <- function(x1, x2, sing = FALSE) {
  comparaison(
    x1, x2,
    hausse_defaut  = "exc\u00e8dent",
    egalite_defaut = "sont au niveau de",
    baisse_defaut  = "sont en dessous de",
    alt = as.integer(sing),
    hausse_alt  = "exc\u00e8de",
    egalite_alt = "est au niveau de",
    baisse_alt  = "est en dessous de"
  )
}


#' Evolution nominale simple
#'
#' Decrit l'evolution entre \code{x1} et \code{x2}
#' sous forme nominale suivie du pourcentage correspondant.
#'
#' @param x1 Le niveau le plus recent.
#' @param x2 Le niveau le plus ancien.
#'
#' @return
#' Une chaine de caracteres de type
#' "en hausse de" ou "en baisse de"
#' suivie du pourcentage formate.
#'
#' @examples
#' g_nom_simple(3, 1)  # "en hausse de 200,0 %"
#' g_nom_simple(3, 5)  # "en baisse de 40,0 %"
#'
#' @export
g_nom_simple <- function(x1, x2) {

  variation <- g(x1, x2)

  z <- comparaison(
    x1, x2,
    hausse_defaut  = "en hausse de",
    egalite_defaut = "en hausse",
    baisse_defaut  = "en baisse de"
  )

  paste(z, format_g(variation, signe = 0))
}







