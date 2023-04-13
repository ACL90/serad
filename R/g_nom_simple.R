#' Evolution décrite (simplement) de facon nominale, suivie d'une évolution
#' @param x1 Le niveau le plus récent
#' @param x2 Le niveau le plus ancien
#'
#' @return en hausse de//en baisse de + pourcentage
#'
#' @examples
#' g_nom_simple(3,1) # en hausse de 200,0 %
#' g_nom_simple(3,5) # en baisse de 40,0 %
#'
#' @export
   g_nom_simple = function(x1,x2){
#    z = case_when(g(x1,x2)>=0~"en hausse de",
#                  g(x1,x2)<0~"en baisse de")
     z = comparaison(x1,x2,"en hausse de","en hausse","en baisse de")
   return(paste(z,format_g(g(x1,x2),signe=0)))
 }


