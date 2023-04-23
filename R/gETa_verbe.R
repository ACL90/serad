#' Pour une évolution verbale tenant compte de l'acceleration et non suivie de g
#' @param x1 Le niveau le plus récent
#' @param x2 Le niveau précédent
#' @param x3 Le niveau le plus ancien
#' @param sing 1 si le sujet du verbe est singulier (défault), 0 sinon
#'
#' @seealso gETa_verbe_taux
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
#' gETa_verbe(0.999,1,1.01)     #continue à baisser
#' gETa_verbe(0.99,1,1.01)      #poursuit sa baisse // recule à nouveau
#' gETa_verbe(0.96,1,1.01)      #poursuit sa baisse // recule à nouveau
#' gETa_verbe(0.96,1,0.99)      #recule
#' gETa_verbe(0.8,1,0.99)       #se replie fortement
#' gETa_verbe(0.79,1,0.99)      #chute
#'
#'
#' @details Cette fonction est basée sur une partition donnée des situations possibles,
#' qui devrait suffir à l'essentiel des usages.
#'
#' Il est même possible pour un **utilisateur avancé** de paramétrer seuils et
#' sorties *dans le cadre de cette partition* - en revanche, si c'est la partition même qui n'est pas adaptée à l'usage, merci d'envoyer
#' un courriel à l'adresse de contact.
#'
#' *Pour paramétrer en pratique :*\cr
#' \code{library("serad")}\cr
#' \code{serad0 = getOption("serad")}\cr
#' \code{serad0$seuil$verbes$AAsing   = "demeure atone" }\cr
#' \code{serad0$seuil$verbes$AAplur = "demeurent atones" }\cr
#' \code{options(serad = serad0)}
#'
#' *Pour comprendre la partition utilisée:*\cr
#' Elle repose sur 13 cas, étiquettés de A à M.
#' Soient:\cr
#' - \code{g1 = serad::g(x1,x2)}\cr
#' - \code{g2 = g(x2,x32)}\cr
#' - \code{g1 = a(x1,x2,x3)}\cr
#' - \code{seuil = getOption("serad")$seuil}\cr
#'
#' *Nota Bene* : Seules les sorties au singulier sont présentées ci-dessous.\cr
#'
#' Sont d'abord traités séparemment les cas où g1 est proche de zéro :
#' -> **Si** arrondi_tot(g1, **seuil$stable**)==0 :
#'| **Cas** |**Condition supplémentaire**     |**Sortie**            | **Valeur standard**|
#'| ---     | ---------------------           |:---------------:     | ------------------:|
#'| A       | arrondi_tot(g2, seuil$stable)==0|seuil$verbes$AAsing   | reste stable       |
#'| B       | arrondi_tot(g2, seuil$stable)!=0|seuil$verbes$BAsing   | se stabilise       |
#'
#'
#' -> **Sinon, et si** g1>0 :
#'| **Cas** |**Condition supplémentaire**     |**Sortie**            | **Valeur standard**    |
#'| ---     | ---------------------           |:---------------:     | ----------------------:|
#'| C       | g2<(**seuil$g2bas**)            |seuil$verbes$CAsing   | repart à la hausse     |
#'| C       |       (idem)                    |seuil$verbes$CBsing   | se redresse            |
#'| D       | g2<0                            |seuil$verbes$DAsing   | augmente               |
#'| D       |       (idem)                    |seuil$verbes$DBsing   | est en hausse          |
#'| E       | a>(**seuil$afort**)             |seuil$verbes$EAsing   | accélère               |
#'| F       | a<(**seuil$dfort**)             |seuil$verbes$FAsing   | ralentit               |
#'| F       |      (idem)                     |seuil$verbes$FBsing   | se modère              |
#'| G       | (cas restants)                  |seuil$verbes$GAsing   | poursuit sa progression|
#'
#' -> **Sinon** :
#'| **Cas** |**Condition supplémentaire**     |**Sortie**            | **Valeur standard**    |
#'| ---     | ---------------------           |:---------------:     | ----------------------:|
#'| L       | g1<(**seuil$g1tresbas**)        |seuil$verbes$LAsing   | chute                  |
#'| M       | g1<(**seuil$g1bas**)            |seuil$verbes$MAsing   | se replie fortement    |
#'| H       | g2>(**seuil$g2haut**)           |seuil$verbes$HAsing   | recule                 |
#'| H       |       (idem)                    |seuil$verbes$HBsing   | se replie              |
#'| I       | g2>=0                           |seuil$verbes$IAsing   | baisse                 |
#'| I       |       (idem)                    |seuil$verbes$IBsing   | diminue                |
#'| J       | g2<0 & a>**seuil$afort2**       |seuil$verbes$JAsing   | recule à nouveau       |
#'| J       |       (idem)                    |seuil$verbes$JBsing   | poursuit son recul     |
#'| K       | g2<0 & a<=seuil$afort2          |seuil$verbes$KAsing   | poursuit sa baisse     |
#'| K       |       (idem)                    |seuil$verbes$KBsing   | continue à baisser     |
#'
#' @export
gETa_verbe = function(x1,x2,x3,sing=1){  #sing=1 pour singulier ; =0 pour pluriel
  g1= serad::g(x1,x2)
  g2= serad::g(x2,x3)
  return(gETa_verbe_taux(g1,g2,sing))
}

