% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s.R
\name{s}
\alias{s}
\title{Met au pluriel si la valeur absolue du nombre est supérieure ou égale a 2.}
\usage{
s(a, sing = "", plur = "s", seuil = 2)
}
\arguments{
\item{a}{Le nombre}

\item{sing}{Le mot à indiquer au singulier, rien par défaut.}

\item{plur}{Le mot à indiquer au pluriel, -s par défaut (d'ou le nom de la fonction)}

\item{seuil}{Pour gérer un arrondi à 2 depuis l'inférieur.}
}
\value{
Le mot bien accordé ; par défault '-s ou rien. TEST
}
\description{
Met au pluriel si la valeur absolue du nombre est supérieure ou égale a 2.
}
\examples{
serad::s(-7.5)             #s
serad::s(-2)               #s
serad::s(1.4,"chat parle", "chats parlent") #chat parle
serad::s(-2,"chat parle", "chats parlent")  #chats parlent
serad::s(1.97)             #NA
serad::s(arrondi_tot(1.97))    #s
serad::s(1.97, seuil=1.95) #s

}
\seealso{
Le \href{https://www.dictionnaire-academie.fr/article/QDL057}{site de l'Académie française}
}
