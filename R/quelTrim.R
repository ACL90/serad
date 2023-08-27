#' Fournit dynamiquement le trimestre sous forme littéraire
#' @param trim Le trimestre sous la forme d'un chiffre : 1, 2, 3 ou 4.
#' @param annee Un nombre positif en 4 chiffres
#' @param type Par défaut: "lettres" (pour 1er trimestre XXXX). Sinon: "chiffres" (pour premier trimestre XXXX)
#' @param majuscule Par défaut 0. 1 pour avoir "Premier" plutôt que "premier"
#'
#' @return 1er trimestre XXXX (ou une variante)
#'
#' @examples
#' quelTrim(3,2023)                  #troisième trimestre 2023
#' quelTrim(3,2023,majuscule=1)      #Troisième trimestre 2023
#' quelTrim(3,2023,type="chiffres") #3e trimestre 2023
#' quelTrim(1,2023,type="chiffres") #1er trimestre 2023
#'
#' @seealso
#' nextTrim prevTrim
#'
#' @export
quelTrim = function(trim,annee,type="lettres",majuscule=0) {
  annee=as.numeric(annee) #il faudrait une erreur si pas possible de transformer
  trim=as.numeric(trim) #il faudrait une erreur si pas possible de transformer
    if(type=="lettres") {
      if(trim==1 && majuscule==1) {
        b="Premier"
      }    else if(trim==1 && majuscule==0) {
        b="premier"
      } else if(trim==2 && majuscule==1) {
        b="Deuxi\u00e8me"
      }    else if(trim==2 && majuscule==0) {
        b="deuxi\u00e8me"
      }    else if(trim==3 && majuscule==1) {
        b="Troisi\u00e8me"
      }    else if(trim==3 && majuscule==0) {
        b="troisi\u00e8me"
      }    else if(trim==4 && majuscule==1) {
        b="Quatri\u00e8me"
      }    else if(trim==4 && majuscule==0) {
        b="quatri\u00e8me"
      }    else b="Autres" #mettre un message d'erreur ici
    }
    if(type=="chiffres") {
      b = ifelse(trim==1,"1er",paste0(trim,"e"))
    }
    b = paste0(b, " ","trimestre"," ",annee)
    return(b)


}

#usethis::use_test()



###################
#nextTrim()
#' Fournit dynamiquement le trimestre suivant sous forme littéraire
#' @param trim Le trimestre sous la forme d'un chiffre : 1, 2, 3 ou 4.
#' @param annee Un nombre positif en 4 chiffres
#' @param type Par défaut: "lettres" (pour 1er trimestre XXXX). Sinon: "chiffres" (pour premier trimestre XXXX)
#' @param majuscule Par défaut 0. 1 pour avoir "Premier" plutôt que "premier"
#'
#' @return 1er trimestre XXXX (ou une variante)
#'
#' @examples
#' nextTrim(3,2023)                  #quatrième trimestre 2023
#' nextTrim(4,2023)                  #premier trimestre 2024
#'
#' @seealso
#' quelTrim prevTrim
#'
#' @export
nextTrim = function(trim,annee,type="lettres",majuscule=0) {
  annee=as.numeric(annee) #il faudrait une erreur si pas possible de transformer
  trim=as.numeric(trim) #il faudrait une erreur si pas possible de transformer
  if(trim==4) {
    b = quelTrim(1,annee+1,type,majuscule)
  }
  else {
    b= quelTrim(trim+1,annee,type,majuscule)
  }
  return(b)
}


###################
#prevTrim()
#' Fournit dynamiquement le trimestre précédent sous forme littéraire
#' @param trim Le trimestre sous la forme d'un chiffre : 1, 2, 3 ou 4.
#' @param annee Un nombre positif en 4 chiffres
#' @param type Par défaut: "lettres" (pour 1er trimestre XXXX). Sinon: "chiffres" (pour premier trimestre XXXX)
#' @param majuscule Par défaut 0. 1 pour avoir "Premier" plutôt que "premier"
#'
#' @return 1er trimestre XXXX (ou une variante)
#'
#' @examples
#' prevTrim(1,2023)                  #quatrième trimestre 2022
#'
#' @seealso
#' nextTrim quelTrim
#'
#' @export
prevTrim = function(trim,annee,type="lettres",majuscule=0) {
  annee=as.numeric(annee) #il faudrait une erreur si pas possible de transformer
  trim=as.numeric(trim) #il faudrait une erreur si pas possible de transformer
  if(trim==1) {
    b = quelTrim(4,annee-1,type,majuscule)
  }
  else {
    b= quelTrim(trim-1,annee,type,majuscule)
  }
  return(b)
}




#############################################################
#' Fournit dynamiquement le mois sous forme littéraire
#' @param mois Le mois sous la forme d'un chiffre : 1...12
#' @param annee Un nombre positif en 4 chiffres
#' @param type Par défaut: "Annee" (pour janvier XXXX). Sinon: janvier
#' @param majuscule Par défaut 0. 1 pour avoir "Janvier" plutôt que "janvier"
#'
#' @return janvier XXXX (ou une variante)
#'
#' @examples
#' quelMois(3,2023)                  #mars 2023
#' quelMois(3,2023,majuscule=1)      #Mars 2023
#' quelMois(3,2023,type="autres")    #mars
#'
#' @seealso
#' quelTrim nextMois prevMois
#'
#' @export
quelMois = function(mois,annee,type="Annee",majuscule=0) {
  if(is.na(annee)){
    type="pas d annee" #il faudrait afficher un avertissement
  }
  annee=as.numeric(annee) #il faudrait une erreur si pas possible de transformer
  mois=as.numeric(mois) #il faudrait une erreur si pas possible de transformer

    if(mois==1 && majuscule==1) {
      b="Janvier"
    }    else if(mois==1 && majuscule==0) {
      b="janvier"
    } else if(mois==2 && majuscule==1) {
      b="F\u00e9vrier"
    }    else if(mois==2 && majuscule==0) {
      b="f\u00e9vrier"
    }    else if(mois==3 && majuscule==1) {
      b="Mars"
    }    else if(mois==3 && majuscule==0) {
      b="mars"
    }    else if(mois==4 && majuscule==1) {
      b="Avril"
    }    else if(mois==4 && majuscule==0) {
      b="avril"
    }    else if(mois==5 && majuscule==1) {
      b="Mai"
    }    else if(mois==5 && majuscule==0) {
      b="mai"
    }    else if(mois==6 && majuscule==1) {
      b="Juin"
    }    else if(mois==6 && majuscule==0) {
      b="juin"
    }    else if(mois==7 && majuscule==1) {
      b="Juillet"
    }    else if(mois==7 && majuscule==0) {
      b="juillet"
    }    else if(mois==8 && majuscule==1) {
      b="Ao\u00fbt"
    }    else if(mois==8 && majuscule==0) {
      b="ao\u00fbt"
    }    else if(mois==9 && majuscule==1) {
      b="Septembre"
    }    else if(mois==9 && majuscule==0) {
      b="septembre"
    }    else if(mois==10 && majuscule==1) {
      b="Octobre"
    }    else if(mois==10 && majuscule==0) {
      b="octobre"
    }    else if(mois==11 && majuscule==1) {
      b="Novembre"
    }    else if(mois==11 && majuscule==0) {
      b="novembre"
    }    else if(mois==12 && majuscule==1) {
      b="D\u00e9cembre"
    }    else if(mois==12 && majuscule==0) {
      b="d\u00e9cembre"
    }    else b="Autres" #mettre un message d'erreur ici

  if(type=="Annee") {
    b = paste0(b," ",annee)
  }
  return(b)
}

#usethis::use_test()
#quelques rappels
#stringi::stri_escape_unicode("é")
#\\u00e9
#stringi::stri_escape_unicode("é")
#\\u00fb

###################
#nextMois()
#' Fournit dynamiquement le trimestre suivant sous forme littéraire
#' @param mois Le mois sous la forme d'un chiffre : 1..12
#' @param annee Un nombre positif en 4 chiffres
#' @param type Par défaut: "Annee" (pour février XXXX). Sinon: février.
#' @param majuscule Par défaut 0. 1 pour avoir "Février" plutôt que "février"
#'
#' @return Février XXXX (ou une variante)
#'
#' @examples
#' nextMois(3,2023)                  #avril 2023
#' nextMois(12,2023)                 #janvier 2024
#'
#' @seealso
#' quelMois nextTrim prevMois
#'
#' @export
nextMois = function(mois,annee,type="Annee",majuscule=0) {
  annee=as.numeric(annee) #il faudrait une erreur si pas possible de transformer
  mois=as.numeric(mois) #il faudrait une erreur si pas possible de transformer
  if(mois==12) {
    b = quelMois(1,annee+1,type,majuscule)
  }
  else {
    b= quelMois(mois+1,annee,type,majuscule)
  }
  return(b)
}


###################
#prevMois()
#' Fournit dynamiquement le mois précédent sous forme littéraire
#' @param mois Le mois sous la forme d'un chiffre : 1..12
#' @param annee Un nombre positif en 4 chiffres
#' @param type Par défaut: "Annee" (pour décembre XXXX). Sinon: décembre.
#' @param majuscule Par défaut 0. 1 pour avoir "Décembre" plutôt que "décembre"
#'
#' @return décembre XXXX (ou une variante)
#'
#' @examples
#' prevMois(1,2023)                  #décembre 2022
#'
#' @seealso
#' prevTrim nextMois quelMois
#'
#' @export
prevMois = function(mois,annee,type="Annee",majuscule=0) {
  annee=as.numeric(annee) #il faudrait une erreur si pas possible de transformer
  mois=as.numeric(mois) #il faudrait une erreur si pas possible de transformer
  if(mois==1) {
    b = quelMois(12,annee-1,type,majuscule)
  }
  else {
    b= quelMois(mois-1,annee,type,majuscule)
  }
  return(b)
}




###################
#whichMois()
#' pour récupérer le mois dans un format littéraire
#' @param mois Le mois en lettres
#'
#' @return 1..12
#'
#' @examples
#' whichMois("En Juil 98")                  #7
#'
#' @seealso
#' quelMois
#'
#' @export
whichMois = function(mois) {

  list1 = c("Jan","jan")
  list2 = c("Fev","fev","Feb","feb","F\u00e9v","f\u00e9v")
  list3 = c("Mar","mar")
  list4 = c("Avr","avr","Apr","apr")
  list5 = c("Mai","mai","May","may")
  list6 = c("Juin","juin","Jun","jun")
  list7 = c("Juil","juil","Jul","jul")
  list8 = list("Aug","aug","Ao","ao")
  list9 = c("Sep","sep")
  list10 = c("Oct","oct")
  list11 = c("Nov","nov")
  list12 = c("Dec","dec","D\u00e9c","d\u00e9c")

  # grep("ao",'août') #1
  # grep('août',"ao") #0
  # mois="aoutrr 2099"
  # any(sapply(list8, grepl, mois))
  if(any(sapply(list1, grepl, mois))==1){
    return(1)
  } else if(any(sapply(list2, grepl, mois))==1){
    return(2)
  } else if(any(sapply(list3, grepl, mois))==1){
    return(3)
  } else if(any(sapply(list4, grepl, mois))==1){
    return(4)
  } else if(any(sapply(list5, grepl, mois))==1){
    return(5)
  } else if(any(sapply(list6, grepl, mois))==1){
    return(6)
  } else if(any(sapply(list7, grepl, mois))==1){
    return(7)
  } else if(any(sapply(list8, grepl, mois))==1){
    return(8)
  } else if(any(sapply(list9, grepl, mois))==1){
    return(9)
  } else if(any(sapply(list10, grepl, mois))==1){
    return(10)
  } else if(any(sapply(list11, grepl, mois))==1){
    return(11)
  } else if(any(sapply(list12, grepl, mois))==1){
    return(12)
  } else {
    return(0)  #ajouter message d erreur ?
  }

}
#whichMois("June dsfdgogzb")
