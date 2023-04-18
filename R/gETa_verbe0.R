#' Pour une évolution verbale tenant compte de l'acceleration et non suivi de g
#' @param g1 La dernière evolution, exprimé en pourcent
#' @param g2 L'évolution précédente, exprimée en pourcent
#' @param sing 1 si le sujet du verbe est singulier (défault), 0 sinon
#'
#' @seealso gETa_verbe00
#'
#' @return Une modalité (un nombre). gETa_verbe0 indiquera concrètement le verbe
#'
#' @examples
#' gETa_verbe0(0.049,0.049) #reste stable
#' gETa_verbe0(0.049,2)     #se stabilise
#' gETa_verbe0(10,1)        #accélère
#' gETa_verbe0(4,1,0)       #accélèrent
#' gETa_verbe0(1,1)         #poursuit sa progression
#' gETa_verbe0(0.3,1)       #ralentit
#' gETa_verbe0(0.1,-1)      #repart à la hausse
#' gETa_verbe0(-0.1,-1)     #poursuit sa baisse
#' gETa_verbe0(-0.3,-1)     #poursuit sa baisse
#' gETa_verbe0(-1,-1)       #poursuit sa baisse
#' gETa_verbe0(-4,-1)       #recule à nouveau
#' gETa_verbe0(-4,1)        #recule
#' gETa_verbe0(-20,1)       #se replie fortement
#' gETa_verbe0(-21,1)       #chute
#'
#' @export
gETa_verbe0 = function(g1,g2,sing=1){  #sing=1 pour singulier


  serad = getOption("serad")
  verbes = serad$verbes
  aleaDummy = serad$aleaDummy

  rd = aleaDummy*runif(1) + (1-aleaDummy)*0.5

  return(dplyr::case_when(gETa_verbe00(g1,g2)=="A"    ~ifelse(sing,verbes$AAsing,verbes$AAplur),
                   gETa_verbe00(g1,g2)=="B"           ~ifelse(sing,verbes$BAsing,verbes$BAplur),
                   gETa_verbe00(g1,g2)=="C" & rd>=0.5 ~ifelse(sing,verbes$CAsing,verbes$CAplur),
                   gETa_verbe00(g1,g2)=="C" & rd<0.5  ~ifelse(sing,verbes$CBsing,verbes$CBplur),
                   gETa_verbe00(g1,g2)=="D" & rd>=0.5 ~ifelse(sing,verbes$DAsing,verbes$DAplur),
                   gETa_verbe00(g1,g2)=="D" & rd<0.5  ~ifelse(sing,verbes$DBsing,verbes$DBplur),
                   gETa_verbe00(g1,g2)=="E"           ~ifelse(sing,verbes$EAsing,verbes$EAplur),
                   gETa_verbe00(g1,g2)=="F" & rd>=0.5 ~ifelse(sing,verbes$FAsing,verbes$FAplur),
                   gETa_verbe00(g1,g2)=="F" & rd<0.5  ~ifelse(sing,verbes$FBsing,verbes$FBplur),
                   gETa_verbe00(g1,g2)=="G"           ~ifelse(sing,verbes$GAsing,verbes$GAplur),
                   gETa_verbe00(g1,g2)=="H" & rd>=0.5 ~ifelse(sing,verbes$HAsing,verbes$HAplur),
                   gETa_verbe00(g1,g2)=="H" & rd<0.5  ~ifelse(sing,verbes$HBsing,verbes$HBplur),
                   gETa_verbe00(g1,g2)=="I" & rd>=0.5 ~ifelse(sing,verbes$IAsing,verbes$IAplur),
                   gETa_verbe00(g1,g2)=="I" & rd<0.5  ~ifelse(sing,verbes$IBsing,verbes$IBplur),
                   gETa_verbe00(g1,g2)=="J" & rd>=0.5 ~ifelse(sing,verbes$JAsing,verbes$JAplur),
                   gETa_verbe00(g1,g2)=="J" & rd<0.5  ~ifelse(sing,verbes$JBsing,verbes$JBplur),
                   gETa_verbe00(g1,g2)=="K" & rd>=0.5 ~ifelse(sing,verbes$KAsing,verbes$KAplur),
                   gETa_verbe00(g1,g2)=="K" & rd<0.5  ~ifelse(sing,verbes$KBsing,verbes$KBplur),
                   gETa_verbe00(g1,g2)=="L"           ~ifelse(sing,verbes$LAsing,verbes$LAplur),
                   gETa_verbe00(g1,g2)=="M"           ~ifelse(sing,verbes$MAsing,verbes$MAplur)
  ))
  }


#quelques rappels
#stringi::stri_escape_unicode("é")
#\\u00e9
#stringi::stri_escape_unicode("è")
#\\u00e8
#stringi::stri_escape_unicode("à")
#\\u00e0"


# return(dplyr::case_when(gETa_verbe00(g1,g2)=="A"~ifelse(sing,"reste stable","restent stables"),
#                         gETa_verbe00(g1,g2)=="B"~ifelse(sing,"se stabilise","se stabilisent"),
#                         gETa_verbe00(g1,g2)=="C" & rd>=0.5~ifelse(sing,"repart \u00e0 la hausse","repartent \u00e0 la hausse"),
#                         gETa_verbe00(g1,g2)=="C" & rd<0.5~ifelse(sing,"se redresse","se redressent"),
#                         gETa_verbe00(g1,g2)=="D" & rd>=0.5~ifelse(sing,"augmente","augmentent"),
#                         gETa_verbe00(g1,g2)=="D" & rd<0.5~ifelse(sing,"est en hausse","sont en hausse"),
#                         gETa_verbe00(g1,g2)=="E" ~ifelse(sing,"acc\u00e9l\u00e8re","acc\u00e9l\u00e8rent"),
#                         gETa_verbe00(g1,g2)=="F" & rd>=0.5~ifelse(sing,"ralentit","ralentissent"),
#                         gETa_verbe00(g1,g2)=="F" & rd<0.5~ifelse(sing,"se mod\u00e8re","se mod\u00e8rent"),
#                         gETa_verbe00(g1,g2)=="G" ~ifelse(sing,"poursuit sa progression","poursuivent leur progression"),
#                         gETa_verbe00(g1,g2)=="H" & rd>=0.5~ifelse(sing,"recule","reculent"),
#                         gETa_verbe00(g1,g2)=="H" & rd<0.5~ifelse(sing,"se replie","se replient"),
#                         gETa_verbe00(g1,g2)=="I" & rd>=0.5~ifelse(sing,"baisse","baissent"),
#                         gETa_verbe00(g1,g2)=="I" & rd<0.5~ifelse(sing,"diminue","diminuent"),
#                         gETa_verbe00(g1,g2)=="J" & rd>=0.5~ifelse(sing,"recule \u00e0 nouveau","reculent \u00e0 nouveau"),
#                         gETa_verbe00(g1,g2)=="J" & rd<0.5~ifelse(sing,"poursuit son recul","poursuivent leur recul"),
#                         gETa_verbe00(g1,g2)=="K" & rd>=0.5~ifelse(sing,"poursuit sa baisse","poursuivent leur baisse"),
#                         gETa_verbe00(g1,g2)=="K" & rd<0.5~ifelse(sing,"continue \u00e0 baisser","continuent \u00e0 baisser"),
#                         gETa_verbe00(g1,g2)=="L" ~ifelse(sing,"chute","chutent"),
#                         gETa_verbe00(g1,g2)=="M" ~ifelse(sing,"se replie fortement","se replient fortement")  #40 modalites
# ))
# }

