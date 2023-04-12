#' verbe pour exprimer le sens de l'évolution, pouvant gérer si le nom est singulier ou pluriel
#' @param x1 Le niveau le plus récent
#' @param x2 Le niveau le plus ancien
#' @param sing 0 si le nom est un pluriel, 1 si un singulier. Par défaut un pluriel.
#'
#' @seealso comparaison
#'
#' @return 'excéde//-nt' ou 'est//sont au niveau de' ou 'est//sont en dessous de'
#'
#' @examples
#' depasse(1.04,1)          # excédent
#' depasse(1.04,1,sing=1)   # excéde
#' depasse(0.96,1)          # sont en dessous de
#' depasse(0.9991,1)        # sont au niveau de
#'
#' @export
depasse = function(x1,x2,sing=0){  #sing=1 pour singulier
  return(comparaison(x1,x2,param=sing,
                     hausse0="exc\u00e8dent",hausse1="exc\u00e8de",
                     egalite0="sont au niveau de",egalite1="est au niveau de",
                     baisse0="sont en dessous de",baisse1="est en dessous de"
                     )
  )
}


#quelques rappels
#stringi::stri_escape_unicode("é")
#\\u00e9
#stringi::stri_escape_unicode("è")
#\\u00e8
#stringi::stri_escape_unicode("à")
#\\u00e0"


