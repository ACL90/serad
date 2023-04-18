#' Pour une évolution verbale tenant compte de l'acceleration et non suivi de g
#' @param g1 La dernière evolution, exprimée en pourcent
#' @param g2 L'évolution précédente, exprimée en pourcent
#'
#' @seealso gETa_verbe0
#'
#' @return Une modalité (un nombre). gETa_verbe0 indiquera concrètement le verbe
#'
# @examples
# gETa_verbe00(0.049,0.049) #1
# gETa_verbe00(0.049,2)     #2
# gETa_verbe00(10,1)        #5
# gETa_verbe00(4,1)         #5
# gETa_verbe00(1,1)         #7
# gETa_verbe00(0.3,1)       #6
# gETa_verbe00(0.1,-1)      #3
# gETa_verbe00(-0.1,-1)     #11
# gETa_verbe00(-0.3,-1)     #11
# gETa_verbe00(-1,-1)       #11
# gETa_verbe00(-4,-1)       #10
# gETa_verbe00(-4,1)        #8
# gETa_verbe00(-20,1)       #13
# gETa_verbe00(-21,1)       #12
#'
# pas utilisable par l'utilisateur donc pas le mot cle export
gETa_verbe00 = function(g1,g2){

  a = serad::g(g1,g2)
  seuil = getOption("serad")$seuil

  return(
    dplyr::case_when((arrondi_tot(g1, seuil$stable)==0) & (arrondi_tot(g2, seuil$stable)==0) ~"A",  #reste stable # round(g1,1)
            (arrondi_tot(g1, seuil$stable)==0) & (arrondi_tot(g2, seuil$stable)!=0) ~"B",  #se stabilise
            g1>0 & g2<(seuil$g2bas)         ~"C",  #repart à la hausse #se redresse a1 et a2
            g1>0 & g2<0                     ~"D",  #augmente, est en hausse a3 a4
            g1>0 & a>(seuil$afort)          ~"E",  #accélère a5 a6
            g1>0 & a<(seuil$dfort)          ~"F",  #ralentit, se modère a7 a8
            g1>0                            ~"G",  #poursuit sa progression a9
            g1<(seuil$g1tresbas)            ~"L", #chute a10
            g1<(-10)                        ~"M", #se replie fortement a11
            g1<0 & g2>(1-0.05)              ~"H",  #recule #se replie a12 a13
            g1<0 & g2>=0                    ~"I",  #baisse, diminue a14 a15
            g1<0 & g2<0 & a>30              ~"J", #recule à nouveau, poursuit son recul a16 a17 a18
            g1<0 & g2<0 & a<=30             ~"K"  #poursuit sa baisse, continue ? baisser a19 a20 a21 #21 modalites
  )
  )
}


#
# a = serad::g(g1,g2)
# return(
#   dplyr::case_when((arrondi_tot(g1,1)==0) & (arrondi_tot(g2,1)==0) ~1,  #reste stable # round(g1,1)
#                    (arrondi_tot(g1,1)==0) & (arrondi_tot(g2,1)!=0) ~2,  #se stabilise
#                    g1>0 & g2<(-0.5)                ~3,  #repart à la hausse #se redresse a1 et a2
#                    g1>0 & g2<0                     ~4,  #augmente, est en hausse a3 a4
#                    g1>0 & a>30                     ~5,  #accélère a5 a6
#                    g1>0 & a<(-30)                  ~6,  #ralentit, se modère a7 a8
#                    g1>0                            ~7,  #poursuit sa progression a9
#                    g1<(-20)                        ~12, #chute a10
#                    g1<(-10)                        ~13, #se replie fortement a11
#                    g1<0 & g2>(1-0.05)              ~8,  #recule #se replie a12 a13
#                    g1<0 & g2>=0                    ~9,  #baisse, diminue a14 a15
#                    g1<0 & g2<0 & a>30              ~10, #recule à nouveau, poursuit son recul a16 a17 a18
#                    g1<0 & g2<0 & a<=30             ~11  #poursuit sa baisse, continue ? baisser a19 a20 a21 #21 modalites
#   )
# )
# }


