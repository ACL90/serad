#' Evolution décrite de facon nominale, non suivie d'une évolution.
#' @param x1 Le niveau le plus récent
#' @param x2 le niveau le plus ancien
#'
#' @return L'évolution, par exemple: "une forte hausse".
#'
#' @examples
#' g_nom(1.04,1)  # une forte hausse
#' g_nom(1.01,1)  # une hausse
#' g_nom(1.004,1) # une hausse modérée
#' g_nom(1.001,1) # une légère hausse
#' g_nom(1,1)     # une stabilité
#' g_nom(0.997,1) # une légère baisse
#' g_nom(0.99,1)  # une baisse modérée
#' g_nom(0.96,1)  # une baisse
#' g_nom(0.95,1)  # une forte baisse
#'
#'
#' @details Pour un **utilisateur avancé**, il est possible de paramétrer seuils et
#' sorties. Par exemple :
#'
#' \code{library("serad")}\cr
#' \code{serad0 = getOption("serad")}\cr
#' \code{serad0$nomse$fortttt   = 7.95 #à la place de : 3.95}\cr
#' \code{serad0$nm$fortttt = "un bond" #à la place de : "une forte hausse"}\cr
#' \code{options(serad = serad0)}
#'
#' Soit \code{g = serad::g(x1,x2)}. Si g>seuil, alors sortie où:
#' ```{r table2, echo=FALSE, message=FALSE, warnings=FALSE, results='asis'}
#'tabl <- "
#'| **Seuil**             | **Valeur standard** |**Sortie**              | **Valeur standard**             |
#'| --------------------- |:---------------:| ------------------:|----------------------------:|
#'| serad0$nomse$fortttt  | 3.95            |serad0$nm$fortttt   | une forte hausse            |
#'| serad0$nomse$forttt   | 0.95            |serad0$nm$forttt    | une hausse                  |
#'| serad0$nomse$fortt    | 0.35            |serad0$nm$fortt     | une hausse mod\u00e9r\u00e9e|
#'| serad0$nomse$fort     | 0.05            |serad0$nm$fort      | une l\u00e9g\u00e8re hausse |
#'| serad0$nomse$faible   | (-0.05)         |serad0$nm$faible    | une stabilit\u00e9          |
#'| serad0$nomse$faiblee  | (-0.35)         |serad0$nm$faiblee   | une l\u00e9g\u00e8re baisse |
#'| serad0$nomse$faibleee | (-1.05)         |serad0$nm$faibleee  | une baisse mod\u00e9r\u00e9e|
#'| serad0$nomse$faibleeee| (-4.05)         |serad0$nm$faibleeee | une baisse                  |
#'|Et en dessous:         |                 |serad0$nm$faibleeeee| une forte baisse            |
#'"
#'cat(tabl) # output the table in a format good for HTML/PDF/docx conversion
#'```
#'
#' @export
g_nom = function(x1,x2){
  g = g(x1,x2)
  return(g_nom0(g))
}

#quelques rappels
#stringi::stri_escape_unicode("?")
#\\u00e9
#stringi::stri_escape_unicode("?")
#\\u00e8
#stringi::stri_escape_unicode("?")
#\\u00e0"
