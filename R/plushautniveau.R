#' Indique si le niveau est le plus haut ou le plus bas de la serie
#'
#' Determine si la derniere valeur d'une serie correspond
#' au plus haut ou au plus bas niveau observe depuis
#' un certain nombre de periodes.
#'
#' @param df Data frame en entree.
#' @param temps Ordre temporel : "croissant" (par defaut)
#'   si les valeurs vont du passe vers le present,
#'   "decroissant" sinon.
#' @param voc_haut Formulation utilisee pour un plus haut niveau.
#' @param voc_bas Formulation utilisee pour un plus bas niveau.
#' @param vart Variable de temps du data frame.
#'   Par defaut : premiere colonne.
#' @param vary Variable numerique du data frame.
#'   Par defaut : seconde colonne.
#' @param nbperiode Nombre minimal de periodes depuis
#'   lesquelles le niveau doit etre extremum pour
#'   retourner une phrase (par defaut : 3).
#'
#' @return
#' Une chaine de caracteres indiquant le plus haut
#' ou le plus bas niveau depuis une date donnee.
#' Retourne une chaine vide si rien de notable.
#'
#' @examples
#' col0 = c("Y1T1", "Y1T2", "Y1trim3", "Y1T4","Y2T1","Y2-T2")
#' col1 = c(12, 11, 7, 6, 9, 10)
#' col2 = c(12, 11, 7, 6, 9, 14)
#' col3 = c(12, 11, 3, 6, 9, 4)
#' col4 = c(12, 11, 7, 6, 9, 4)
#' col5 = c(12, 11, 7, 3, 9, 4)
#' df1 = data.frame(col0, col1, col2, col3, col4, col5)
#'
#' plushautniveau(df1)
#' # "C'est le plus haut niveau depuis Y1T2."
#'
#' plushautniveau(df1, nbperiode = 5)
#' # ""
#'
#' plushautniveau(df1, vary = "col2")
#' # "C'est le plus haut niveau depuis le debut de la serie."
#'
#' plushautniveau(df1, vary = "col3")
#' # "C'est le plus bas niveau depuis Y1trim3."
#'
#' plushautniveau(df1, vary = "col4")
#' # "C'est le plus bas niveau depuis le debut de la serie."
#'
#' @export
plushautniveau <- function(df,
                           temps = "croissant",
                           voc_haut = "C'est le plus haut niveau depuis",
                           voc_bas  = "C'est le plus bas niveau depuis",
                           vart,
                           vary,
                           nbperiode = 3) {

  # ---- Vérifications ----
  if (!is.data.frame(df) || ncol(df) < 2)
    stop("df doit \u00eatre un dataframe avec au moins 2 colonnes")

  if (!temps %in% c("croissant", "decroissant"))
    stop("temps doit \u00eatre 'croissant' ou 'decroissant'")

  if (!is.numeric(nbperiode) || length(nbperiode) != 1 ||
      nbperiode < 2 ||
      abs(nbperiode - round(nbperiode)) > .Machine$double.eps) {
    stop("nbperiode doit etre un entier >= 2")
  }

  if (!missing(vart) && !(vart %in% names(df)))
    stop("vart doit \u00eatre un nom de variable du dataframe")

  if (!missing(vary) && !(vary %in% names(df)))
    stop("vary doit \u00eatre un nom de variable du dataframe")

  # ---- Extraction des variables ----
  xt <- if (missing(vart)) df[[1]] else df[[vart]]
  xy <- if (missing(vary)) df[[2]] else df[[vary]]

  if (!is.numeric(xy))
    stop("La s\u00e9rie d'int\u00e9r\u00eat doit \u00eatre num\u00e9rique")

  if (temps == "decroissant") {
    xt <- rev(xt)
    xy <- rev(xy)
  }

  if (length(xy) < 2)
    return("")

  last_val  <- utils::tail(xy, 1)
  prev_vals <- utils::head(xy, -1)

  if (all(is.na(prev_vals)))
    return("")

  # =========================
  # CAS PLUS HAUT
  # =========================
  if (all(last_val >= utils::tail(prev_vals, nbperiode - 1))) {

    idx_candidates <- which(prev_vals > last_val)

    if (length(idx_candidates) == 0) {
      idx <- 0
    } else {
      idx <- max(idx_candidates)
    }

    nb <- length(prev_vals) - idx

    if (nb < nbperiode - 1)
      return("")

    if (idx == 0)
      return(paste0(voc_haut, " le d\u00e9but de la s\u00e9rie."))
    else
      return(paste0(voc_haut, " ", xt[idx], "."))

  }

  # =========================
  # CAS PLUS BAS
  # =========================
  if (all(last_val <= utils::tail(prev_vals, nbperiode - 1))) {

    idx_candidates <- which(prev_vals < last_val)

    if (length(idx_candidates) == 0) {
      idx <- 0
    } else {
      idx <- max(idx_candidates)
    }

    nb <- length(prev_vals) - idx

    if (nb < nbperiode - 1)
      return("")

    if (idx == 0)
      return(paste0(voc_bas, " le d\u00e9but de la s\u00e9rie."))
    else
      return(paste0(voc_bas, " ", xt[idx], "."))

  }

  return("")
}


#pendant la phase de test
# temps = "croissant"
# voc_haut = "C'est le plus haut niveau depuis"
# voc_bas  = "C'est le plus bas niveau depuis"
# nbperiode = 3
# df0=data.frame(col0,col5,col2,col3)
# xy = df0[, 2]
# xt = df0[, 1]

#usethis::use_test()
