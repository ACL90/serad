#' Formatage Dares des variations (exprimées en nombre, avec signe)
#' @param y La différence à formater.
#' @param signe 1 si on veut le signe, 0 sinon. Par défaut signe = 1.
#' @param detail Par défaut -2 pour arrondir à la centaine.
#'
#' @seealso arrondi_tot
#'
#' @return Le niveau formaté.
#'
#' @examples
#' format_delta(365484)       #+365 500
#' format_delta(365484,0)     #365 500
#' format_delta(-365484)      #−365 500
#' format_delta(-365484,0,-1) #365 480
#'
#' @export
format_delta =function(y, signe =1, detail=-2){
  z = format_niv(y, detail)
  if(signe ==1) {
    return(ifelse(y>=0,
                  paste0("+",z),
                  z))
    }
  else{
    return(gsub('\u2212',"",z))
  }
  }

