#' Pour une évolution verbale tenant compte de l'acceleration et non suivi de g
#' @param g1 La dernière evolution, exprimée en pourcent
#' @param g2 L'évolution précédente, exprimée en pourcent
#'
#' @seealso gETa_verbe_taux
#'
# @importFrom dplyr case_when
#'
#' @return Une modalité (un nombre). gETa_verbe0 indiquera concrètement le verbe
#'
# @examples
# gETa_verbe00(0.049,0.049) #A
# gETa_verbe00(0.049,2)     #B
# gETa_verbe00(10,1)        #E
# gETa_verbe00(4,1)         #E
# gETa_verbe00(1,1)         #G
# gETa_verbe00(0.3,1)       #F
# gETa_verbe00(0.1,-1)      #C
# gETa_verbe00(-0.1,-1)     #K
# gETa_verbe00(-0.3,-1)     #K
# gETa_verbe00(-1,-1)       #K
# gETa_verbe00(-4,-1)       #J
# gETa_verbe00(-4,1)        #H
# gETa_verbe00(-20,1)       #M
# gETa_verbe00(-21,1)       #L
#'
# pas utilisable par l'utilisateur donc pas le mot cle export
gETa_verbe00 = function(g1,g2){

  a = serad::g(g1,g2)
  seuil = getOption("serad")$seuil

  return(

    ifelse((arrondi_tot(g1, seuil$stable)==0) & (arrondi_tot(g2, seuil$stable)==0) ,"A",  #reste stable
    ifelse((arrondi_tot(g1, seuil$stable)==0) & (arrondi_tot(g2, seuil$stable)!=0) ,"B",  #se stabilise
    ifelse(g1>0 & g2<(seuil$g2bas)         ,"C",  #repart à la hausse #se redresse
    ifelse(g1>0 & g2<0                     ,"D",  #augmente, est en hausse
    ifelse(g1>0 & a>(seuil$afort)          ,"E",  #accélère
    ifelse(g1>0 & a<(seuil$dfort)          ,"F",  #ralentit, se modère
    ifelse(g1>0                            ,"G",  #poursuit sa progression
    ifelse(g1<(seuil$g1tresbas)            ,"L",  #chute
    ifelse(g1<(seuil$g1bas)                ,"M",  #se replie fortement
    ifelse(g1<0 & g2>(seuil$g2haut)        ,"H",  #recule #se replie
    ifelse(g1<0 & g2>=0                    ,"I",  #baisse, diminue
    ifelse(g1<0 & g2<0 & a>seuil$afort2    ,"J",  #recule à nouveau, poursuit son recul
              "K"   # g1<0 & g2<0 & a<=seuil$afort2 #poursuit sa baisse, continue à baisser
    ))))))))))))
  )

}

#usethis::use_test()

    # case_when((arrondi_tot(g1, seuil$stable)==0) & (arrondi_tot(g2, seuil$stable)==0) ~"A",  #reste stable
    #         (arrondi_tot(g1, seuil$stable)==0) & (arrondi_tot(g2, seuil$stable)!=0) ~"B",  #se stabilise
    #         g1>0 & g2<(seuil$g2bas)         ~"C",  #repart à la hausse #se redresse
    #         g1>0 & g2<0                     ~"D",  #augmente, est en hausse
    #         g1>0 & a>(seuil$afort)          ~"E",  #accélère
    #         g1>0 & a<(seuil$dfort)          ~"F",  #ralentit, se modère
    #         g1>0                            ~"G",  #poursuit sa progression
    #         g1<(seuil$g1tresbas)            ~"L",  #chute
    #         g1<(seuil$g1bas)                ~"M",  #se replie fortement
    #         g1<0 & g2>(seuil$g2haut)        ~"H",  #recule #se replie
    #         g1<0 & g2>=0                    ~"I",  #baisse, diminue
    #         g1<0 & g2<0 & a>seuil$afort2    ~"J",  #recule à nouveau, poursuit son recul
    #         g1<0 & g2<0 & a<=seuil$afort2   ~"K"   #poursuit sa baisse, continue à baisser



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


