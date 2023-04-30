#' Précise si le plus haut/niveau de la série et depuis quelle date
#' @description
#' Un dataframe en entrée.\cr
#' Si pas indiqué, la date est considérée comme étant la première colonne.
#' Pas besoin de la formater.\cr
#' Si pas indiqué, on suppose les valeurs les plus anciennes en haut
#' (temps croissant).\cr
#' Si pas indiqué, la série d'intérêt, numérique, est considérée comme étant la
#' seconde colonne.\cr
#' Sauf si le paramètre diffère, la fonction ne ressort rien si pas la plus
#' haute/basse valeur depuis au moins 3 périodes.
#'
#' @details
#' Fonction ecrite sur demande d'un utilisateur.
#'
#' @param df Le dataframe en entrée
#' @param temps "croissant" par défaut si du passé vers le présent. "décroissant" sinon.
#' @param voc_haut "C'est le plus haut niveau de la série depuis" par défaut
#' @param voc_bas "C'est le plus bas niveau de la série depuis" par défaut
#' @param vart La variable de temps du dataframe. La première valeur par défaut.
#' @param vary La variable numérique du dataframe. La seconde valeur par défaut.
#' @param nbperiode Le nombre de périodes dépassée à partir duquel c'est utile
#' de sortir une valeur. 3 par défaut.
#'
#' @return La phrase. Rien si rien de notable.
#'
#' @examples
#' col0 = c("Y1T1", "Y1T2", "Y1trim3", "Y1T4","Y2T1","Y2-T2")
#' col1 = c( 12,     11,      7,         6,     9,     10)
#' col2 = c( 12,     11,      7,         6,     9,     14)
#' col3 = c( 12,     11,      3,         6,     9,     4)
#' col4 = c( 12,     11,      7,         6,     9,     4)
#' col5 = c( 12,     11,      7,         3,     9,     4)
#' df1 = data.frame(col0,col1,col2,col3,col4,col5)
#' plushautniveau(df1) #"C'est le plus haut niveau depuis Y1T2."
#' #plushautniveau(df1,nbperiode = 5.000000000000001)  #erreur: nbperiode doit etre un nombre entier
#' plushautniveau(df1,nbperiode = 5)  #NA
#' plushautniveau(df1,vary="col2") #"C'est le plus haut niveau depuis le début de la série."
#' plushautniveau(df1,vary="col3") #"C'est le plus bas niveau depuis Y1trim3."
#' plushautniveau(df1,vary="col4") #"C'est le plus bas niveau depuis le début de la série."
#'
#' @export
plushautniveau = function(df,
                          temps = "croissant",
                          voc_haut = "C'est le plus haut niveau depuis",
                          voc_bas  = "C'est le plus bas niveau depuis",
                          vart,
                          vary,
                          nbperiode = 3){

  df0 = df


  #verification: un dataframe, en entree
  if(is.data.frame(df0)==FALSE){
    stop("plushautniveau() requiert un dataframe en entr\u00e9e")
  }

  #verification : le datafame en entree a au moins 2 colonnes
  if(ncol(df0)<=1){
    stop("le dataframe dans plushautniveau() requiert au moins 2 colonnes")
  }

  #verification que, si spécifié, temps comporte croissont ou decroissant
  if(!missing(temps) && !(temps%in%c("croissant","decroissant"))){
    stop("temps peut prendre 2 valeurs: croissant ou decroissant")
  }

  #definition de xt et xy ; on les ordonne par ordre croissant
  #xy
  if(missing(vary)){
      xy = df0[, 2]
    } else{
      xy = df0[, vary]
    }
  #xt
  if(missing(vart)){
    xt = df0[, 1]
  } else{
    xt = df0[, vart]
  }
  #on re-ordonne l'echelle du temps si besoin
  if(temps == "decroissant"){
      xy = rev(xy)
      xt = rev(xt)
    }

  #verification : xy est bien numerique
    if(!all(is.numeric(xy))){
      stop("La s\u00e9rie d interet (precis\u00e9e par vary ou 2eme colonne de df) doit
           etre entierement num\u00e9rique.")
    }

  #verification : si rempli, nbperiode bien un nombre entier
    if(!missing(nbperiode)&&!serad_is.wholenumber(nbperiode)){
      stop("nbperiode doit etre un nombre entier")
    }
  #verification : nbperiode bien supérieur ou égal à 2
    if(!missing(nbperiode)&&!serad_is.wholenumber(nbperiode)){
      stop("nbperiode doit etre superieur ou \u00e9gal a 2")
    }

  #verification : si rempli, temps est bien un string.
    #Idem voc_haut, voc_bas, vart,vary
    if(!missing(temps)&&!is.character(temps)){
      stop("temps est une chaine de caract\u00e8res : croissant ou decroissant")
    }
  if(!missing(voc_haut)&&!is.character(voc_haut)){
    stop("voc_haut doit etre une chaine de caract\u00e8res")
  }
  if(!missing(voc_bas)&&!is.character(voc_bas)){
    stop("voc_bas doit etre une chaine de caract\u00e8res")
  }
  if(!missing(vart)&&!is.character(vart)){
    stop("vart doit etre une chaine de caract\u00e8res")
  }
  if(!missing(vary)&&!is.character(vary)){
    stop("vary doit etre une chaine de caract\u00e8res")
  }


  #première variation : va-t-on viser le plus haut/plus bas
  xy1 = xy[1:length(xy)-1] - rep(1, length(xy)-1) * xy[length(xy)]
  d = xy1[length(xy1)]
  xy1bis = xy1[1:length(xy1)]

  y1bis = rev(d*xy1bis>=0)
  occ = which.min(y1bis)

  #nbperiode : sur un temps significatif
  if(!all(y1bis) && occ<nbperiode) {
    #paste("")
    return("")
  }

  if(d<=0){ #si plus haut
    if(all(y1bis)){
      #paste(voc_haut,"depuis le d\u00e9but de la s\u00e9rie")
      return(paste0(voc_haut," ","le d\u00e9but de la s\u00e9rie","."))
    } else{
      #paste(voc_haut,xt[length(xt)-occ])
      return(paste0(voc_haut," ",xt[length(xt)-occ],"."))
      }
    }   else{  #d>0 si plus bas
      if(all(y1bis)){
        #paste(voc_bas,"depuis le d\u00e9but de la s\u00e9rie")
        return(paste0(voc_bas," ","le d\u00e9but de la s\u00e9rie","."))
        } else{
          #paste(voc_bas,xt[length(xt)-occ])
          return(paste0(voc_bas," ",xt[length(xt)-occ],"."))
        }
      }

  }


#pendant la phase de test
# temps = "croissant"
# voc_haut = "C'est le plus haut niveau depuis"
# voc_bas  = "C'est le plus bas niveau depuis"
# nbperiode = 3
# df0=data.frame(col0,col5,col2,col3)
# xy = df0[, 2]
# xt = df0[, 1]

#usethis::use_test()


#voir ?is.integer, exemple a la fin
serad_is.wholenumber =  function(x, tol = .Machine$double.eps)  {
  return(abs(x - round(x)) < tol)
}
