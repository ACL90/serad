#' Evoluation verbale ne tenant pas compte de l'acceleration et suivi de "de 'ga'"
#' @param g L'evolution
#' @param sing 1 si le sujet du verbe est singulier (défault), 0 sinon
#'
#' @seealso g_verbe
#'
#' @return l'evolution, par exemple: "bondit de 10,0 %"
#'
#' @examples
#' g_verbe0(10,1) # bondit de 10,0 %
#' g_verbe0(4,1)  # s'accroit de 4,0 %
#' g_verbe0(1,0)  # sont en hausse de 1,0 %
#' g_verbe0(0.3)  # augmente de 0,3 %
#' g_verbe0(0.1)  # s'accroit très légèrement de 0,1 %
#' g_verbe0(-0.1) # est stable à 0,1 %
#' g_verbe0(-0.3) # diminue légèrement de 0,3 %
#' g_verbe0(-1)   # recule légèrement de 1,0 %
#' g_verbe0(-4)   # baisse de 4,0 %
#' g_verbe0(-20)  # recule de 20,0 %
#' g_verbe0(-21)  # chute de 21,0 %
#'
#' @export
g_verbe0 = function(g,sing=1){  #sing pour singulier

  serad0 = getOption("serad")
  seuil = serad0$sve

  z = dplyr::case_when(g>seuil$forttttt~ifelse(sing,serad0$verbev$forttttt_sing,   serad0$verbev$forttttt_plur),
            g>seuil$fortttt            ~ifelse(sing,serad0$verbev$fortttt_sing,    serad0$verbev$fortttt_plur),
            g>seuil$forttt             ~ifelse(sing,serad0$verbev$forttt_sing,     serad0$verbev$forttt_plur),
            g>seuil$fortt              ~ifelse(sing,serad0$verbev$fortt_sing,      serad0$verbev$fortt_plur),
            g>seuil$fort               ~ifelse(sing,serad0$verbev$fort_sing,       serad0$verbev$fort_plur),
            g>seuil$faible             ~ifelse(sing,serad0$verbev$faible_sing,     serad0$verbev$faible_plur),
            g>seuil$faiblee            ~ifelse(sing,serad0$verbev$faiblee_sing,    serad0$verbev$faiblee_plur),
            g>seuil$faibleee           ~ifelse(sing,serad0$verbev$faibleee_sing,   serad0$verbev$faibleee_plur),
            g>seuil$faibleeee          ~ifelse(sing,serad0$verbev$faibleeee_sing,  serad0$verbev$faibleeee_plur),
            g>seuil$faibleeeee         ~ifelse(sing,serad0$verbev$faibleeeee_sing, serad0$verbev$faibleeeee_plur),
           g<=seuil$faibleeeee         ~ifelse(sing,serad0$verbev$faibleeeeee_sing,serad0$verbev$faibleeeeee_plur))
  return(paste(z,format_g(g,signe=0)))
}

#TO-DO pour la suite: avoir tous les parametres en une seule matriceparametre avec deux colonnes: la fonction utilisee, le qualificatif texte, le seuil bas
#que cette matrice soit facile a appeler, et a changer de fa?on 'permanente' par l'utilisateur

#quelques rappels
#stringi::stri_escape_unicode("é")
#\\u00e9
#stringi::stri_escape_unicode("è")
#\\u00e8
#stringi::stri_escape_unicode("à")
#\\u00e0"


# z = dplyr::case_when(g>10-0.05~ifelse(sing,"bondit de","bondissent de"),
#                      g>4-0.05~ifelse(sing,"s'accroit de","s'acroissent de"),
#                      g>1-0.05~ifelse(sing,"est en hausse de","sont en hausse de"),
#                      g>0.3-0.05~ifelse(sing,"augmente de","augmentent de"),
#                      g>0.1-0.05~ifelse(sing,"s'accroit tr\u00e8s l\u00e9g\u00e8rement de","s'acroissent tr\u00e8s l\u00e9g\u00e8rement de"),
#                      g>(-0.1-0.05)~ifelse(sing,"est stable \u00e0","sont stables \u00e0"),
#                      g>(-0.3-0.05)~ifelse(sing,"diminue l\u00e9g\u00e8rement de","diminuent l\u00e9g\u00e8rement de"),
#                      g>(-1-0.05)~ifelse(sing,"recule l\u00e9g\u00e8rement de","reculent l\u00e9g\u00e8rement de"),
#                      g>(-4-0.05)~ifelse(sing,"baisse de","baissent de"),
#                      g>(-20-0.05)~ifelse(sing,"recule de","reculent de"),
#                      g<=(-20-0.05)~ifelse(sing,"chute de","chutent de"))
# return(paste(z,format_g(g,signe=0)))
