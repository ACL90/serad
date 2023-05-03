#' Documente les contributions à une évolution.
#' @description
#' Un dataframe en entrée.\cr
#' Si pas indiqué, la date est considérée comme étant la première colonne.
#' Pas besoin de la formater.\cr
#' Si pas indiqué, on suppose les valeurs les plus anciennes en haut
#' (temps croissant).\cr
#' Toutes les autres colonnes sont considérées comme les composantes dont les
#' contributions doivent être analysées. Elles doivent être numériques.\cr
#'
#' @details
#' Fonction écrite sur demande d'une utilisatrice.
#'
#' @param df Le dataframe en entrée
#' @param temps "croissant" par défaut si du passé vers le présent. "décroissant" sinon.
#' @param vart La variable de temps du dataframe. La première valeur par défaut.
#' @param Tglissement Sur combien de périodes faut-il remonter pour calculer l'évolution à 1 par défaut.
#' @param seuilpc Un seuil en dessous duquel la contribution (en valeur absolue)
#' n'est pas prise en compte. 5% par défaut.
#'
#' @details Si aucune contribution n'est supérieure à seuilpc en valeur absolue,
#' contributions() retourne à minima la plus grande contribution de même sens
#' que la contribution totale.
#'
#' @return Un descriptif ordonné des différents glissements.
#'
#' @examples
#' col0 = c("Y1T1", "Y1T2", "Y1trim3", "Y1T4","Y2T1","Y2-T2")
#' col1 = c( 12,      6,      2,         86,     19,     10)
#' col2 = c(  4,      8,      7,         34,     87,     14)
#' col3 = c( 10,     20,      3,         66,     90,     54)
#' col4 = c( 29,     12,      4,         16,     40,     94)
#' col5 = c( 58,     76,      1,          3,     34,     19)
#' df1 = data.frame(col0,col1,col2,col3,col4,col5)
#' contributions(df1)
#' ###col2 contribue pour +92,4 % à l'évolution entre Y2T1 et Y2-T2 ;
#' ###col3 contribue pour +45,6 % à l'évolution entre Y2T1 et Y2-T2 ;
#' ###col4 contribue pour −68,4 % à l'évolution entre Y2T1 et Y2-T2 ;
#' contributions(df1, Tglissement = 4, seuilpc = 50)
#' ###col4 contribue pour +118,8 % à l'évolution entre Y1T2 et Y2-T2 ;
#' ###col5 contribue pour −82,6 % à l'évolution entre Y1T2 et Y2-T2 ;
#'
#' @export
contributions = function(df,
                          temps = "croissant",
                          vart,
                          Tglissement = 1,
                          seuilpc = 20){

  df0 = df

  #verification: un dataframe, en entree
  if(is.data.frame(df0)==FALSE){
    stop("contributions() requiert un dataframe en entr\u00e9e")
  }

  #verification : le datafame en entree a au moins 2 colonnes
  if(ncol(df0)<=1){
    stop("le dataframe dans contributions() requiert au moins 2 colonnes")
  }

  #verification que, si spécifié, temps comporte croissont ou decroissant
  if(!missing(temps) && !(temps%in%c("croissant","decroissant"))){
    stop("temps peut prendre 2 valeurs: croissant ou decroissant")
  }

  #definition de xt et dfy ; on les ordonne par ordre croissant
  #xt
  if(missing(vart)){
    xt  = df0[, 1]
    dfy = df0[, -1]
  } else{
    xt  = df0[, vart]
    dfy = df0[, -which(names(df0) == vart)]
  }
  #on re-ordonne l'echelle du temps si besoin
  if(temps == "decroissant"){
    xt = rev(xt)
    df0 = as.data.frame(lapply(df0, rev))
    dfy = as.data.frame(lapply(dfy, rev))
  }


  ########je me suis arrêté là
  #verification : xy est bien numerique
  if(!all(sapply(dfy, is.numeric))){
    stop("Au moins une des s\u00e9ries composantes n'est pas num\u00e9rique!")
  }

  #verification : si rempli, nbperiode bien un nombre entier
  if(!missing(Tglissement)&&!serad_is.wholenumber(Tglissement)){
    stop("Tglissement doit etre un nombre entier")
  }
  #verification : Tglissement bien supérieur ou égal à 1
  if(!missing(Tglissement)&&(Tglissement<1)){
    stop("Tglissement doit etre superieur ou \u00e9gal a 1")
  }

  #verification : si rempli, temps est bien un string.
  #Idem vart
  if(!missing(temps)&&!is.character(temps)){
    stop("temps est une chaine de caract\u00e8res : croissant ou decroissant")
  }
  if(!missing(vart)&&!(vart%in%colnames(df0))){
    stop("vart doit etre un nom de variable de df")
  }

  #on restreint xt et dfy aux deux periodes utilisees
  #utilisation de Tglissement
  xt  = xt[c((length(xt)-Tglissement),length(xt))]
  dfy = dfy[c((length(dfy)-Tglissement+1),length(dfy)+1), ]

  #calcul du glissement total avec serad::g
  #cela gère déjà le cas de l'évolution nulle (avertissement)
  gtot = serad::g(sum(dfy[2,]),sum(dfy[1,]))
  DELTA = sum(dfy[2,]) -  sum(dfy[1,])
  #-------> rajouter un epsilon si DELTA==0

  #calcul de la contribution pour chaque colonne : on obtien Adfy
  Adfy = 100*(dfy[2,] - dfy[1,])/DELTA

  #on ordonne et on garde bonne trace de l ordre
    #Adfy = sort(Adfy[1,],decreasing =TRUE) #plus simple mais generait message d
    #d erreur car dataframe
  Adfy = data.frame(t(Adfy),t(Adfy))
  colnames(Adfy)[1] <-c("serie")

    #intermede pour prendre en compte seuil
  df_filtered <- Adfy[abs(Adfy$serie) > seuilpc, ]
  if(nrow(df_filtered)==0){
    #on en garde au moins 1
    Adfy <- Adfy[1, ]
  } else{
    Adfy <-df_filtered
  }

  Adfy <- Adfy[order(-Adfy$serie), ]
  bon_ordre <- rownames(Adfy)
  b <- data.frame(Adfy[,1])

  #formattage avec format_g : on obtient Bdfy
  Bdfy = t(as.data.frame(lapply(b[,1], serad::format_g)))

  #puis boucle pour obtenir le texte
  A = ""
  for (i in 1:length(Bdfy)) {
    #cat devant ???
    A = paste0(A,
                   bon_ordre[i],
                   " contribue pour ",
                   Bdfy[i],
                   " \u00e0 l'\u00e9volution entre ",
                   xt[1],
                   " et ",
                   xt[2],
                   " ; ")
  }
  return(A)

  }

  # contributions(df0)
  # contributions(df0, Tglissement = 4)
  # col0 = c("Y1T1", "Y1T2", "Y1trim3", "Y1T4","Y2T1","Y2-T2")
  # col1 = c( 12,      6,      2,         86,     19,     10)
  # col2 = c(  4,      8,      7,         34,     87,     14)
  # col3 = c( 10,     20,      3,         66,     90,     54)
  # col4 = c( 29,     12,      4,         16,     40,     94)
  # col5 = c( 58,     76,      1,          3,     34,     19)
  # df1 = data.frame(col0,col1,col2,col3,col4,col5)
  # df0 = df1
  # vart = "col0"
  # xt = df0[, vart]
  # dfy = df0[, -1]
  # Tglissement = 1
  # seuilpc = 20

  #quelques rappels
  #stringi::stri_escape_unicode("é")
  #\\u00e9
  #stringi::stri_escape_unicode("è")
  #\\u00e8
  #stringi::stri_escape_unicode("à")
  #\\u00e0"

  #usethis::use_test()
