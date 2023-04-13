#' Pour une évolution verbale tenant compte de l'acceleration et non suivi de g
#' @param x1 Le niveau le plus récent
#' @param x2 Le niveau précédent
#' @param x3 Le niveau le plus ancien
#' @param sing 1 si le sujet du verbe est singulier (défault), 0 sinon
#'
#' @seealso gETa_verbe0
#'
#' @return Le verbe.
#'
#' @examples
#' gETa_verbe(1.00049,1,0.9996) #reste stable
#' gETa_verbe(1.00049,1,0.981)  #se stabilise
#' gETa_verbe(1.1,1,0.99)       #accélère
#' gETa_verbe(1.04,1,0.99)      #accélère
#' gETa_verbe(1.01,1,0.99)      #poursuit sa progression
#' gETa_verbe(1.003,1,0.99,0)   #ralentissent
#' gETa_verbe(1.001,1,1.01)     #repart à la hausse
#' gETa_verbe(0.999,1,1.01)     #poursuit sa baisse
#' gETa_verbe(0.99,1,1.01)      #poursuit sa baisse // recule à nouveau
#' gETa_verbe(0.96,1,1.01)      #poursuit sa baisse // recule à nouveau
#' gETa_verbe(0.96,1,0.99)      #recule
#' gETa_verbe(0.8,1,0.99)       #se replie fortement
#' gETa_verbe(0.79,1,0.99)      #chute
#'
#' @export
gETa_verbe = function(x1,x2,x3,sing=1){  #sing=1 pour singulier
  g1= serad::g(x1,x2)
  g2= serad::g(x2,x3)
  return(gETa_verbe0(g1,g2,sing))
}
