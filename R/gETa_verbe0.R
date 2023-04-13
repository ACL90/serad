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
  #rd = runif(1)
  rd = 0.6
  return(dplyr::case_when(gETa_verbe00(g1,g2)==1~ifelse(sing,"reste stable","restent stables"),
                   gETa_verbe00(g1,g2)==2~ifelse(sing,"se stabilise","se stabilisent"),
                   gETa_verbe00(g1,g2)==3 & rd>=0.5~ifelse(sing,"repart \u00e0 la hausse","repartent \u00e0 la hausse"),
                   gETa_verbe00(g1,g2)==3 & rd<0.5~ifelse(sing,"se redresse","se redressent"),
                   gETa_verbe00(g1,g2)==4& rd>=0.5~ifelse(sing,"augmente","augmentent"),
                   gETa_verbe00(g1,g2)==4& rd<0.5~ifelse(sing,"est en hausse","sont en hausse"),
                   gETa_verbe00(g1,g2)==5~ifelse(sing,"acc\u00e9l\u00e8re","acc\u00e9l\u00e8rent"),
                   gETa_verbe00(g1,g2)==6& rd>=0.5~ifelse(sing,"ralentit","ralentissent"),
                   gETa_verbe00(g1,g2)==6& rd<0.5~ifelse(sing,"se mod\u00e8re","se mod\u00e8rent"),
                   gETa_verbe00(g1,g2)==7~ifelse(sing,"poursuit sa progression","poursuivent leur progression"),
                   gETa_verbe00(g1,g2)==8& rd>=0.5~ifelse(sing,"recule","reculent"),
                   gETa_verbe00(g1,g2)==8& rd<0.5~ifelse(sing,"se replie","se replient"),
                   gETa_verbe00(g1,g2)==9& rd>=0.5~ifelse(sing,"baisse","baissent"),
                   gETa_verbe00(g1,g2)==9& rd<0.5~ifelse(sing,"diminue","diminuent"),
                   gETa_verbe00(g1,g2)==10& rd>=0.5~ifelse(sing,"recule \u00e0 nouveau","reculent \u00e0 nouveau"),
                   gETa_verbe00(g1,g2)==10& rd<0.5~ifelse(sing,"poursuit son recul","poursuivent leur recul"),
                   gETa_verbe00(g1,g2)==11& rd>=0.5~ifelse(sing,"poursuit sa baisse","poursuivent leur baisse"),
                   gETa_verbe00(g1,g2)==11& rd<0.5~ifelse(sing,"continue \u00e0 baisser","continuent \u00e0 baisser"),
                   gETa_verbe00(g1,g2)==12~ifelse(sing,"chute","chutent"),
                   gETa_verbe00(g1,g2)==13~ifelse(sing,"se replie fortement","se replient fortement")
  ))
  }


#quelques rappels
#stringi::stri_escape_unicode("é")
#\\u00e9
#stringi::stri_escape_unicode("è")
#\\u00e8
#stringi::stri_escape_unicode("à")
#\\u00e0"
