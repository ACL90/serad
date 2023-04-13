#' Pour une évolution verbale tenant compte de l'acceleration et non suivi de g
#' @param g1 La dernière evolution, exprimée en pourcent
#' @param g2 L'évolution précédente, exprimée en pourcent
#'
#' @seealso gETa_verbe0
#'
#' @return Une modalité (un nombre). gETa_verbe0 indiquera concrètement le verbe
#'
#' @examples
#' gETa_verbe00(0.049,0.049) #1
#' gETa_verbe00(0.049,2)     #2
#' gETa_verbe00(10,1)        #5
#' gETa_verbe00(4,1)         #5
#' gETa_verbe00(1,1)         #7
#' gETa_verbe00(0.3,1)       #6
#' gETa_verbe00(0.1,-1)      #3
#' gETa_verbe00(-0.1,-1)     #11
#' gETa_verbe00(-0.3,-1)     #11
#' gETa_verbe00(-1,-1)       #11
#' gETa_verbe00(-4,-1)       #10
#' gETa_verbe00(-4,1)        #8
#' gETa_verbe00(-20,1)       #13
#' gETa_verbe00(-21,1)       #12
#'
# pas utilisable par l'utilisateur donc pas le mot cle export
gETa_verbe00 = function(g1,g2){

  a = serad::g(g1,g2)
  return(
    dplyr::case_when((arrondi_tot(g1,1)==0) & (arrondi_tot(g2,1)==0) ~1,  #reste stable # round(g1,1)
            (arrondi_tot(g1,1)==0) & (arrondi_tot(g2,1)!=0) ~2,  #se stabilise
            g1>0 & g2<(-0.5)                ~3,  #repart à la hausse #se redresse
            g1>0 & g2<0                     ~4,  #augmente, est en hausse
            g1>0 & a>30                     ~5,  #accélère
            g1>0 & a<(-30)                  ~6,  #ralentit, se modère
            g1>0                            ~7,  #poursuit sa progression
            g1<(-20)                        ~12, #chute
            g1<(-10)                        ~13, #se replie fortement
            g1<0 & g2>(1-0.05)              ~8,  #recule #se replie
            g1<0 & g2>=0                    ~9,  #baisse, diminue
            g1<0 & g2<0 & a>30              ~10, #recule à nouveau, poursuit son recul
            g1<0 & g2<0 & a<=30             ~11  #poursuit sa baisse, continue ? baisser
  )
  )
}

