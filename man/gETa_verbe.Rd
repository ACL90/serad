% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gETa_verbe.R
\name{gETa_verbe}
\alias{gETa_verbe}
\title{Pour une évolution verbale tenant compte de l'acceleration et non suivie de g}
\usage{
gETa_verbe(x1, x2, x3, sing = 1)
}
\arguments{
\item{x1}{Le niveau le plus récent}

\item{x2}{Le niveau précédent}

\item{x3}{Le niveau le plus ancien}

\item{sing}{1 si le sujet du verbe est singulier (défault), 0 sinon}
}
\value{
Le verbe.
}
\description{
Pour une évolution verbale tenant compte de l'acceleration et non suivie de g
}
\details{
Cette fonction est basée sur une partition donnée des situations possibles,
qui devrait suffir à l'essentiel des usages.

Il est même possible pour un \strong{utilisateur avancé} de paramétrer seuils et
sorties \emph{dans le cadre de cette partition} - en revanche, si c'est la partition même qui n'est pas adaptée à l'usage, merci d'envoyer
un courriel à l'adresse de contact.

\emph{Pour paramétrer en pratique :}\cr
\code{library("serad")}\cr
\code{serad0 = getOption("serad")}\cr
\code{serad0$seuil$verbes$AAsing   = "demeure atone" }\cr
\code{serad0$seuil$verbes$AAplur = "demeurent atones" }\cr
\code{options(serad = serad0)}

\emph{Pour comprendre la partition utilisée:}\cr
Elle repose sur 13 cas, étiquettés de A à M.
Soient:\cr
\itemize{
\item \code{g1 = serad::g(x1,x2)}\cr
\item \code{g2 = g(x2,x32)}\cr
\item \code{g1 = a(x1,x2,x3)}\cr
\item \code{seuil = getOption("serad")$seuil}\cr
}

\emph{Nota Bene} : Seules les sorties au singulier sont présentées ci-dessous.\cr

Sont d'abord traités séparemment les cas où g1 est proche de zéro :
-> \strong{Si} arrondi_tot(g1, \strong{seuil$stable})==0 :\tabular{llcr}{
   \strong{Cas} \tab \strong{Condition supplémentaire} \tab \strong{Sortie} \tab \strong{Valeur standard} \cr
   A \tab arrondi_tot(g2, seuil$stable)==0 \tab seuil$verbes$AAsing \tab reste stable \cr
   B \tab arrondi_tot(g2, seuil$stable)!=0 \tab seuil$verbes$BAsing \tab se stabilise \cr
}


-> \strong{Sinon, et si} g1>0 :\tabular{llcr}{
   \strong{Cas} \tab \strong{Condition supplémentaire} \tab \strong{Sortie} \tab \strong{Valeur standard} \cr
   C \tab g2<(\strong{seuil$g2bas}) \tab seuil$verbes$CAsing \tab repart à la hausse \cr
   C \tab (idem) \tab seuil$verbes$CBsing \tab se redresse \cr
   D \tab g2<0 \tab seuil$verbes$DAsing \tab augmente \cr
   D \tab (idem) \tab seuil$verbes$DBsing \tab est en hausse \cr
   E \tab a>(\strong{seuil$afort}) \tab seuil$verbes$EAsing \tab accélère \cr
   F \tab a<(\strong{seuil$dfort}) \tab seuil$verbes$FAsing \tab ralentit \cr
   F \tab (idem) \tab seuil$verbes$FBsing \tab se modère \cr
   G \tab (cas restants) \tab seuil$verbes$GAsing \tab poursuit sa progression \cr
}


-> \strong{Sinon} :\tabular{llcr}{
   \strong{Cas} \tab \strong{Condition supplémentaire} \tab \strong{Sortie} \tab \strong{Valeur standard} \cr
   L \tab g1<(\strong{seuil$g1tresbas}) \tab seuil$verbes$LAsing \tab chute \cr
   M \tab g1<(\strong{seuil$g1bas}) \tab seuil$verbes$MAsing \tab se replie fortement \cr
   H \tab g2>(\strong{seuil$g2haut}) \tab seuil$verbes$HAsing \tab recule \cr
   H \tab (idem) \tab seuil$verbes$HBsing \tab se replie \cr
   I \tab g2>=0 \tab seuil$verbes$IAsing \tab baisse \cr
   I \tab (idem) \tab seuil$verbes$IBsing \tab diminue \cr
   J \tab g2<0 & a>\strong{seuil$afort2} \tab seuil$verbes$JAsing \tab recule à nouveau \cr
   J \tab (idem) \tab seuil$verbes$JBsing \tab poursuit son recul \cr
   K \tab g2<0 & a<=seuil$afort2 \tab seuil$verbes$KAsing \tab poursuit sa baisse \cr
   K \tab (idem) \tab seuil$verbes$KBsing \tab continue à baisser \cr
}
}
\examples{
gETa_verbe(1.00049,1,0.9996) #reste stable
gETa_verbe(1.00049,1,0.981)  #se stabilise
gETa_verbe(1.1,1,0.99)       #accélère
gETa_verbe(1.04,1,0.99)      #accélère
gETa_verbe(1.01,1,0.99)      #poursuit sa progression
gETa_verbe(1.003,1,0.99,0)   #ralentissent // se modèrent
gETa_verbe(1.001,1,1.01)     #repart à la hausse // se redresse
gETa_verbe(0.999,1,1.01)     #poursuit sa baisse // continue à baisser
gETa_verbe(0.99,1,1.01)      #poursuit sa baisse // continue à baisser
gETa_verbe(0.96,1,1.01)      #poursuit son recul // recule à nouveau
gETa_verbe(0.96,1,0.99)      #recule // se replie
gETa_verbe(0.8,1,0.99)       #se replie fortement
gETa_verbe(0.79,1,0.99)      #chute


}
\seealso{
gETa_verbe_taux
}
