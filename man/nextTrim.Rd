% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quelTrim.R
\name{nextTrim}
\alias{nextTrim}
\title{Fournit dynamiquement le trimestre suivant sous forme littéraire}
\usage{
nextTrim(trim, annee, type = "lettres", majuscule = 0, exposant = 1, k = 1)
}
\arguments{
\item{trim}{Le trimestre sous la forme d'un chiffre : 1, 2, 3 ou 4.}

\item{annee}{Un nombre positif en 4 chiffres}

\item{type}{Par défaut: "lettres" (pour 1er trimestre XXXX). Sinon: "chiffres" (pour premier trimestre XXXX)}

\item{majuscule}{Par défaut 0. 1 pour avoir "Premier" plutôt que "premier"}

\item{exposant}{Par défaut 1. Permet d'avoir les exposants sous Markdown , e.g."^1er^". Si 0: "1er".}

\item{k}{Par défaut 1 pour le trimestre suivant. Un nombre entier relatif.}
}
\value{
1^er^ trimestre XXXX (ou une variante)
}
\description{
Fournit dynamiquement le trimestre suivant sous forme littéraire
}
\examples{
nextTrim(3,2023)                  #quatrième trimestre 2023
nextTrim(4,2023)                  #premier trimestre 2024

}
\seealso{
quelTrim prevTrim
}
