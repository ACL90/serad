#' Evolution verbale ne tenant pas compte de l'acceleration et suivie de 'ga'
#'
#' @details
#' Fonction dérivant de g_verbe0 (avec x1/x2 en paramètre, contre g pour g_verbe0)
#'
#' @param x1 Le niveau le plus récent
#' @param x2 Le niveau le plus ancien
#' @param sing0 1 si le sujet du verbe est singulier (défault), 0 sinon
#'
#' @seealso g_verbe0
#'
#' @return l'evolution, par exemple: "bondit de 10,0 %"
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
#' @export
g_verbe = function(x1,x2,sing0=1){g_verbe0(g(x1,x2),sing0)} #sing=1 pour singulier

