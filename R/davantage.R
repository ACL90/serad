#' davantage ou moins
#' @param x1 Le niveau le plus récent
#' @param x2 Le niveau le plus ancien
#'
#' @seealso comparaison
#'
#' @return davantage ou moins
#'
#' @examples
#' davantage(1.04,1)    # davantage
#' davantage(1.04,1.04) # davantage
#' davantage(0.96,1)    # moins
#'
#' @export
davantage = function(x1,x2){
  return(comparaison(x1,x2,"davantage","davantage","moins",0))
}
