#' au-dessus ou en dessous
#' @param x1 Le niveau le plus récent
#' @param x2 Le niveau le plus ancien
#'
#' @seealso comparaison
#'
#' @return au-dessus // en dessous
#'
#' @examples
#' audessus(1.04,1)    # au-dessus
#' audessus(1.04,1.04) # au-dessus
#' audessus(0.96,1)    # en dessous
#'
#' @export
audessus = function(x1,x2){
  return(comparaison(x1,x2,"au-dessus","au-dessus","en dessous",0))
}
