% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/comparaison_taux.R
\name{comparaison_taux}
\alias{comparaison_taux}
\title{comparaison_taux : fonction générique basée sur la variation}
\usage{
comparaison_taux(
  g,
  hausse0,
  egalite0,
  baisse0,
  seuil = 0.1,
  param = 0,
  hausse1 = hausse0,
  egalite1 = egalite0,
  baisse1 = baisse0
)
}
\arguments{
\item{g}{La variation, exprimée en pourcentage (0.1 signifie 0.1\%)}

\item{hausse0}{Mot si hausse}

\item{egalite0}{Mot si egalite}

\item{baisse0}{Mot si baisse}

\item{seuil}{La limite pour l'egalite (par defaut 0.1\%)}

\item{param}{Un parametre supplementaire égal à 0 ou 1 (par exemple pour singulier)}

\item{hausse1}{Formulation différente dépendant de param ; par défaut hausse0}

\item{egalite1}{Formulation différente dépendant de param ; par défaut egalite0}

\item{baisse1}{Formulation différente dépendant de param ; par défaut baisse0}
}
\value{
Un mot
}
\description{
comparaison_taux : fonction générique basée sur la variation
}
\details{
Aussi fonction de base pour comparaison.R
}
\examples{
comparaison_taux(5,"augmente","reste stable","diminue")             #augmente
comparaison_taux(0.05,"augmente","reste stable","diminue")          #reste stable
comparaison_taux(0.05,"augmente","reste stable","diminue",seuil=0)  #augmente
comparaison_taux(0,"augmente","reste égal","diminue", seuil=0)      #augmente
comparaison_taux(0,"as","bs","cs",seuil=0,param=1,hausse1="a",egalite1="b",baisse1="c")      #a
comparaison_taux(0,"as","bs","cs",seuil=0,param=0,hausse1="a",egalite1="b",baisse1="c")      #as
comparaison_taux(0,"as","bs","cs",seuil=0,hausse1="a",egalite1="b",baisse1="c") #as


}
\seealso{
comparaison
}
