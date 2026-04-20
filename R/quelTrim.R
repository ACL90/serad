#' Fournit dynamiquement le trimestre en toutes lettres ou en chiffres
#'
#' Retourne une formulation litteraire du trimestre
#' pour une annee donnee.
#'
#' @param trim Trimestre sous forme numerique : 1, 2, 3 ou 4.
#' @param annee Annee sur 4 chiffres.
#' @param type "lettres" (par defaut) ou "chiffres".
#' @param majuscule Indicateur logique : TRUE pour une majuscule initiale
#'   (ex. "Premier"), FALSE sinon.
#' @param exposant Indicateur logique : TRUE pour afficher
#'   les exposants Markdown (ex. "1^er^"),
#'   FALSE pour afficher "1er".
#' @param mois Si renseigne (entier entre 1 et 12),
#'   remplace trim en deduisant le trimestre correspondant.
#'
#' @return
#' Une chaine de caracteres du type
#' "troisieme trimestre 2023"
#' ou "3^e^ trimestre 2023".
#'
#' @examples
#' quelTrim(3, 2023)
#' quelTrim(3, 2023, majuscule = TRUE)
#' quelTrim(3, 2023, type = "chiffres")
#' quelTrim(1, 2023, type = "chiffres")
#' quelTrim(999, 2023, type = "chiffres", mois = 8)
#' quelTrim(3, 2023, type = "chiffres", exposant = FALSE)
#'
#' @seealso \code{\link{nextTrim}}, \code{\link{prevTrim}}
#'
#' @export
quelTrim <- function(trim,
                     annee,
                     type = c("lettres", "chiffres"),
                     majuscule = FALSE,
                     exposant = TRUE,
                     mois = NULL) {

  type <- match.arg(type)

  annee <- as.numeric(annee)
  if (is.na(annee)) stop("annee doit \u00eatre num\u00e9rique")

  # Si mois fourni → déduction du trimestre
  if (!is.null(mois)) {
    mois <- as.numeric(mois)
    if (is.na(mois) || !(mois %in% 1:12))
      stop("mois doit \u00eatre entre 1 et 12")
    trim <- ceiling(mois / 3)
  } else {
    trim <- as.numeric(trim)
    if (is.na(trim) || !(trim %in% 1:4))
      stop("trim doit \u00eatre entre 1 et 4")
  }

  # ---- Version lettres ----
  if (type == "lettres") {

    noms <- c("premier", "deuxi\u00e8me", "troisi\u00e8me", "quatri\u00e8me")
    b <- noms[trim]

    if (majuscule)
      b <- paste0(toupper(substr(b, 1, 1)), substr(b, 2, nchar(b)))

  } else {

    # ---- Version chiffres ----
    if (exposant) {
      b <- if (trim == 1) "1^er^" else paste0(trim, "^e^")
    } else {
      b <- if (trim == 1) "1er" else paste0(trim, "e")
    }
  }

  paste(b, "trimestre", annee)
}

#' Fournit dynamiquement le trimestre suivant
#'
#' Retourne la formulation du trimestre situe k periodes
#' apres le trimestre indique.
#'
#' @param trim Trimestre sous forme numerique : 1, 2, 3 ou 4.
#' @param annee Annee sur 4 chiffres.
#' @param type "lettres" (par defaut) ou "chiffres".
#' @param majuscule Indicateur logique : TRUE pour une majuscule initiale,
#'   FALSE sinon.
#' @param exposant Indicateur logique : TRUE pour afficher
#'   les exposants Markdown (ex. "1^er^"),
#'   FALSE pour afficher "1er".
#' @param k Decalage en nombre de trimestres.
#'   Par defaut : 1 (trimestre suivant).
#'
#' @return
#' Une chaine de caracteres correspondant au trimestre
#' suivant (par exemple : "quatrieme trimestre 2023").
#'
#' @examples
#' nextTrim(3, 2023) # "quatrième trimestre 2023"
#' nextTrim(4, 2023) # "premier trimestre 2024"
#'
#' @seealso \code{\link{quelTrim}}, \code{\link{prevTrim}}
#'
#' @export
nextTrim <- function(trim,
                     annee,
                     type = c("lettres", "chiffres"),
                     majuscule = FALSE,
                     exposant = TRUE,
                     k = 1) {

  type <- match.arg(type)

  trim  <- as.numeric(trim)
  annee <- as.numeric(annee)

  if (is.na(trim) || !(trim %in% 1:4))
    stop("trim doit \u00eatre entre 1 et 4")

  if (is.na(annee))
    stop("annee doit \u00eatre num\u00e9rique")

  if (!is.numeric(k))
    stop("k doit \u00eatre num\u00e9rique")

  total <- trim - 1 + k

  new_trim  <- (total %% 4) + 1
  new_annee <- annee + (total %/% 4)

  quelTrim(new_trim,
           new_annee,
           type = type,
           majuscule = majuscule,
           exposant = exposant)
}


#' Fournit dynamiquement le trimestre precedent
#'
#' Retourne la formulation du trimestre situe k periodes
#' avant le trimestre indique.
#'
#' @param trim Trimestre sous forme numerique : 1, 2, 3 ou 4.
#' @param annee Annee sur 4 chiffres.
#' @param type "lettres" (par defaut) ou "chiffres".
#' @param majuscule Indicateur logique : TRUE pour une majuscule initiale,
#'   FALSE sinon.
#' @param exposant Indicateur logique : TRUE pour afficher
#'   les exposants Markdown (ex. "1^er^"),
#'   FALSE pour afficher "1er".
#' @param k Decalage en nombre de trimestres.
#'   Par defaut : 1 (trimestre precedent).
#'
#' @return
#' Une chaine de caracteres correspondant au trimestre
#' precedent (par exemple : "quatrieme trimestre 2022").
#'
#' @examples
#' prevTrim(1, 2023)
#' prevTrim(1, 2023, type = "chiffres", exposant = FALSE)
#'
#' @seealso \code{\link{nextTrim}}, \code{\link{quelTrim}}
#'
#' @export
prevTrim <- function(trim,
                     annee,
                     type = c("lettres", "chiffres"),
                     majuscule = FALSE,
                     exposant = TRUE,
                     k = 1) {

  type <- match.arg(type)

  nextTrim(trim,
           annee,
           type = type,
           majuscule = majuscule,
           exposant = exposant,
           k = -k)
}

#' Fournit dynamiquement le mois en toutes lettres
#'
#' Retourne une formulation litteraire du mois,
#' avec ou sans annee.
#'
#' @param mois Mois sous forme numerique : 1 a 12.
#' @param annee Annee sur 4 chiffres.
#' @param type "Annee" (par defaut, ex. "janvier 2023")
#'   ou "autres" (ex. "janvier").
#' @param majuscule Indicateur logique : TRUE pour une majuscule
#'   initiale (ex. "Janvier"), FALSE sinon.
#'
#' @return
#' Une chaine de caracteres correspondant au mois,
#' avec ou sans annee.
#'
#' @examples
#' quelMois(3, 2023)
#' quelMois(3, 2023, majuscule = TRUE)
#' quelMois(3, 2023, type = "autres")
#'
#' @seealso \code{\link{quelTrim}}, \code{\link{nextMois}}, \code{\link{prevMois}}
#'
#' @export
quelMois <- function(mois,
                     annee,
                     type = c("Annee", "autres"),
                     majuscule = FALSE) {

  type <- match.arg(type)

  mois  <- as.numeric(mois)
  annee <- as.numeric(annee)

  if (is.na(mois) || !(mois %in% 1:12))
    stop("mois doit \u00eatre entre 1 et 12")

  noms <- c(
    "janvier", "f\u00e9vrier", "mars", "avril", "mai", "juin",
    "juillet", "ao\u00dbt", "septembre", "octobre", "novembre", "d\u00e9cembre"
  )

  b <- noms[mois]

  if (majuscule)
    b <- paste0(toupper(substr(b, 1, 1)), substr(b, 2, nchar(b)))

  if (type == "Annee") {
    if (is.na(annee))
      stop("annee doit \u00eatre num\u00e9rique")
    b <- paste(b, annee)
  }

  b
}

#' Fournit dynamiquement le mois suivant
#'
#' Retourne la formulation du mois situe k periodes
#' apres le mois indique.
#'
#' @param mois Mois sous forme numerique : 1 a 12.
#' @param annee Annee sur 4 chiffres.
#' @param type "Annee" (par defaut, ex. "fevrier 2023")
#'   ou "autres" (ex. "fevrier").
#' @param majuscule Indicateur logique : TRUE pour une majuscule
#'   initiale (ex. "Fevrier"), FALSE sinon.
#' @param k Decalage en nombre de mois.
#'   Par defaut : 1 (mois suivant).
#'
#' @return
#' Une chaine de caracteres correspondant au mois
#' suivant (par exemple : "avril 2023").
#'
#' @examples
#' nextMois(3, 2023)
#' nextMois(12, 2023)
#' nextMois(12, 2023, k = 2)
#'
#' @seealso \code{\link{quelMois}}, \code{\link{prevMois}}, \code{\link{nextTrim}}
#'
#' @export
nextMois <- function(mois,
                     annee,
                     type = c("Annee", "autres"),
                     majuscule = FALSE,
                     k = 1) {

  type <- match.arg(type)

  mois  <- as.numeric(mois)
  annee <- as.numeric(annee)

  if (is.na(mois) || !(mois %in% 1:12))
    stop("mois doit \u00eatre entre 1 et 12")

  if (is.na(annee))
    stop("annee doit \u00eatre num\u00e9rique")

  total <- mois - 1 + k

  new_mois  <- (total %% 12) + 1
  new_annee <- annee + (total %/% 12)

  quelMois(new_mois,
           new_annee,
           type = type,
           majuscule = majuscule)
}


#' Fournit dynamiquement le mois precedent
#'
#' Retourne la formulation du mois situe k periodes
#' avant le mois indique.
#'
#' @param mois Mois sous forme numerique : 1 a 12.
#' @param annee Annee sur 4 chiffres.
#' @param type "Annee" (par defaut, ex. "decembre 2023")
#'   ou "autres" (ex. "decembre").
#' @param majuscule Indicateur logique : TRUE pour une majuscule
#'   initiale (ex. "Decembre"), FALSE sinon.
#' @param k Decalage en nombre de mois.
#'   Par defaut : 1 (mois precedent).
#'
#' @return
#' Une chaine de caracteres correspondant au mois
#' precedent (par exemple : "decembre 2022").
#'
#' @examples
#' prevMois(1, 2023)         # "décembre 2022"
#' prevMois(12, 2023, k = 2) # "octobre 2023"
#'
#' @seealso \code{\link{prevTrim}}, \code{\link{nextMois}}, \code{\link{quelMois}}
#'
#' @export
prevMois <- function(mois,
                     annee,
                     type = c("Annee", "autres"),
                     majuscule = FALSE,
                     k = 1) {

  type <- match.arg(type)

  nextMois(mois,
           annee,
           type = type,
           majuscule = majuscule,
           k = -k)
}

#' Recupere le numero du mois a partir d'un texte
#'
#' Extrait le numero du mois (1 a 12) a partir
#' d'une chaine de caracteres contenant un mois
#' en format abrege ou litteraire.
#'
#' @param mois Chaine de caracteres contenant un mois
#'   (ex. "En Juil 98").
#'
#' @return
#' Un entier entre 1 et 12 si un mois est detecte,
#' 0 sinon. NA si l'entree est NA.
#'
#' @examples
#' whichMois("En Juil 98")
#'
#' @seealso \code{\link{quelMois}}
#'
#' @export
whichMois <- function(mois) {

  if (is.na(mois))
    return(NA_integer_)

  # Normalisation
  mois_clean <- tolower(mois)

  # Suppression des accents
  mois_clean <- iconv(mois_clean, from = "UTF-8", to = "ASCII//TRANSLIT")

  # Dictionnaire
  dict <- list(
    "1"  = c("jan"),
    "2"  = c("fev", "feb"),
    "3"  = c("mar"),
    "4"  = c("avr", "apr"),
    "5"  = c("mai", "may"),
    "6"  = c("juin", "jun"),
    "7"  = c("juil", "jul"),
    "8"  = c("aou", "aug"),
    "9"  = c("sep"),
    "10" = c("oct"),
    "11" = c("nov"),
    "12" = c("dec")
  )

  for (i in seq_along(dict)) {
    if (any(sapply(dict[[i]], grepl, mois_clean)))
      return(as.numeric(i))
  }

  return(0L)
}
