#' Evolution verbale ne tenant pas compte de l'acceleration et suivie de 'ga'
#'
#' @details
#' Fonction dérivant de g_verbe0 - avec x1 et x2 en paramètres, contre un seul paramètre g pour g_verbe0().
#'
#' @param x1 Le niveau le plus récent
#' @param x2 Le niveau le plus ancien
#' @param sing0 1 si le sujet du verbe est singulier (défault), 0 sinon
#'
#' @seealso g_verbe0
#'
#' @return L'évolution, par exemple: "bondit de 10,0 %"
#'
#' @examples
#' g_verbe(1.1,1,1)  # bondit de 10,0 %
#' g_verbe(1.04,1,1) # s'accroit de 4,0 %
#' g_verbe(1.01,1,0) # sont en hausse de 1,0 %
#' g_verbe(1.003,1)  # augmente de 0,3 %
#' g_verbe(1.001,1)  # s'accroit très légèrement de 1,0 %
#' g_verbe(0.999,1)  # est stable à 0,1 %
#' g_verbe(0.997,1)  # diminue légèrement de 0,3 %
#' g_verbe(0.99,1)   # recule légèrement de 1,0 %
#' g_verbe(0.96,1)   # baisse de 4,0 %
#' g_verbe(0.8,1)    # recule de 20,0 %
#' g_verbe(0.79,1)   # chute de 21,0 %
#'
#'
#' @details Pour un **utilisateur avancé**, il est possible de paramétrer seuils et
#' sorties. Par exemple :
#'
#' \code{library("serad")}\cr
#' \code{serad0 = getOption("serad")}\cr
#' \code{serad0$sve$forttttt   = 15.95 }\cr
#' \code{serad0$verbev$forttttt_sing = "bondit fortement" }\cr
#' \code{serad0$verbev$forttttt_plur = "bondissent fortement" }\cr
#' \code{options(serad = serad0)}
#'
#' Soit \code{g = serad::g(x1,x2)}. Si g>seuil, alors sortie (au singulier).\cr
#' *Nota Bene* : Bien penser à changer aussi les sorties au pluriel.
#' ```{r table2, echo=FALSE, message=FALSE, warnings=FALSE, results='asis'}
#'tabl <- "
#'| **Seuil**             | **Valeur standard** |**Sortie**                | **Valeur standard**             |
#'| --------------------- |:---------------:| ----------------------------:|----------------------------:|
#'| serad0$sve$forttttt   | (9.95)          |serad0$verbev$forttttt_sing   | bondit de            |
#'| serad0$sve$fortttt    | (3.95)          |serad0$verbev$forttttt_sing   | s'accroit de                  |
#'| serad0$sve$forttt     | (0.95)          |serad0$verbev$forttttt_sing   | est en hausse de|
#'| serad0$sve$fortt      | (0.25)          |serad0$verbev$forttttt_sing   | augmente de |
#'| serad0$sve$fort       | (0.05)          |serad0$verbev$forttttt_sing   | s'accroit tr\u00e8s l\u00e9g\u00e8rement de          |
#'| serad0$sve$faible     | (-0.15)         |serad0$verbev$faible_sing     | est stable \u00e0 |
#'| serad0$sve$faiblee    | (-0.35)         |serad0$verbev$faiblee_sing    | diminue l\u00e9g\u00e8rement de|
#'| serad0$sve$faibleee   | (-1.05)         |serad0$verbev$faibleee_sing   | recule l\u00e9g\u00e8rement de                  |
#'| serad0$sve$faibleeee  | (-4.05)         |serad0$verbev$faibleeee_sing  | baisse de                  |
#'| serad0$sve$faibleeeee | (-20.05)        |serad0$verbev$faibleeeee_sing | recule de                  |
#'|Et en dessous:         |                 |serad0$verbev$faibleeeeee_sing| chute de            |
#'"
#'cat(tabl)
#'```
#'
#' @export
g_verbe = function(x1,x2,sing0=1){g_verbe0(serad::g(x1,x2),sing0)} #sing=1 pour singulier

