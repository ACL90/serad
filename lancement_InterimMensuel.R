#https://rmarkdown.rstudio.com/lesson-1.html
# install.packages("rmarkdown")
# install.packages("knitr")
# install.packages("sjmisc")

library(rmarkdown)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(stringr)
library(readxl)
library(sjmisc)
library(stringr)
library(rlang)

#################A modifier avant chaque lancement#####################
Fichier = "Fichier.xlsx"
jour_publication_precendente = "8 septembre 2022"
evol_prov_mois_dernier = -0.2 #%
final_texte = "Pour le mois de juillet 2022, il s'agit de la semaine du 25 au 29, et pour le mois d'août 2022, de celle du 22 au 26."


################Automatisé - attention en cas de restructuration - #####################

#####Fichiers sur secteurs#####
#Attention à ces deux lignes en cas de restructuration du fichier
Secteurs00=read_excel(Fichier, sheet ="intérimaires par secteur",skip = 7)
Secteurs0 =Secteurs00[,c(1,length(Secteurs00)-12,length(Secteurs00)-2,length(Secteurs00)-1,length(Secteurs00))]
#rm(Secteurs00)
colnames(Secteurs0) <- c("Secteur","x12","x3","x2","x1")
Secteurs0 = Secteurs0%>%
  mutate(sigle=substr(Secteur,1,2))%>%
  mutate(A17=ifelse(str_contains(c("AZ","C1","C2","C3","C4","C5",
                                   "DE","FZ","GZ","HZ","IZ","JZ",
                                   "KZ","LZ","MN","OQ","RU"),
                                 sigle),
                    1,0))%>%
  mutate(industrie = ifelse(str_contains(c("C1","C2","C3","C4","C5","DE",
                                           "CI","CJ","CK",
                                           "CB","CC","CE","CF","CG","CH","CM",
                                           "BZ","DZ","EZ"),
                                         sigle),
                            1,0))%>%
  mutate(tertiaire = ifelse(str_contains(c("GZ","HZ","IZ","JZ",
                                           "KZ","LZ","MN","OQ","RU",
                                           "45","46","47",
                                           "JA","JB","JC",
                                           "MA","MB","MC","NZ",
                                           "OZ","PZ","QA","QB","RZ","STU"),
                                         sigle),
                            1,0))%>%
  mutate(Secteur = str_sub(Secteur,6,-1))%>%
  mutate(Secteur = case_when(sigle=="C1" ~ "l'industrie agro-alimentaire", #Fabrication de denrées alimentaires, boissons et produits à base de tabac
                             sigle=="C2" ~ "la cokéfaction et le raffinage",
                             sigle=="C3" ~ "la fabrication de biens d'équipements et de machines", #Fabrication d'équipements électriques, électroniques, informatiques ; fabrication de machines
                             sigle=="C4" ~ "la fabrication de matériels de transport",
                             sigle=="C5" ~ "la fabrication d'autres produits industriels",
                             sigle=="DE" ~ "l'énergie, l'eau et les déchets",  #Industries extractives, énergie, eau, gestion des déchets et dépollution
                             sigle=="FZ" ~ "la construction",
                             sigle=="GZ" ~ "le commerce", # Commerce ; réparation d'automobiles et de motocycles
                             sigle=="HZ" ~ "le transport-entreposage",
                             sigle=="IZ" ~ "l'hébergement-restauration",
                             sigle=="JZ" ~ "l'information-communication",
                             sigle=="KZ" ~ "les activités financières et d'assurance",
                             sigle=="LZ" ~ "les ctivités immobilières",
                             sigle=="MN" ~ "les services aux entreprises",  #Activités scientifiques et techniques ; services administratifs et de soutien
                             sigle=="OQ" ~ "les services non marchands", #Administration publique, enseignement, santé humaine et action sociale
                             sigle=="RU" ~ "les services aux ménages",  #Autres activités de services
                               sigle=="Ag" ~ "l'agriculture",
                               sigle=="In" ~ "l'industrie",
                               sigle=="Co" ~ "la construction",
                               sigle=="Te" ~ "le tertiaire",
                               sigle=="En" ~ "Ensemble"),)%>%
  filter(!is.na(Secteur))


SecteursG=Secteurs0%>%
  filter(sigle%in%c("In","Co","Te"))%>%
  select(-sigle,-A17,-industrie,-tertiaire)
Secteurs0=Secteurs0%>%
  filter(!(sigle%in%c("Ag","In","Co","Te","En")))

#####Fichier pour le premier paragraphe avec plus d'historique#####
#attention à la ligne suivante en cas de restructuration du fichier
Nombre = read_excel(Fichier, sheet ="Nombre d'intérimaires",skip = 7)%>%
  rename(date='...1')%>%
  select(date,Niveau)%>%
  mutate(annee=as.numeric(substr(date,1,4)),
         mois_num = as.numeric(substr(date,6,7)))%>%
  mutate(mois=case_when(mois_num==1 ~ "janvier",
                        mois_num==2 ~ "février",
                        mois_num==3 ~ "mars",
                        mois_num==4 ~ "avril",
                        mois_num==5 ~ "mai",
                        mois_num==6 ~ "juin",
                        mois_num==7 ~ "juillet",
                        mois_num==8 ~ "août",
                        mois_num==9 ~ "septembre",
                        mois_num==10 ~"octobre",
                        mois_num==11 ~"novembre",
                        mois_num==12 ~"décembre"))


#####Fichier pour les régions#####
#attention à la ligne suivante en cas de restructuration du fichier
Regions=t(read_excel(Fichier, sheet ="Intérimaires par région ETU",skip = 7))
colnames(Regions) <- Regions[1,]
Regions = data.frame(Regions)[3:length(Regions[,1]),1:12]
Regions = data.frame(Regions)[-1,]%>%
  data.frame()%>%
  mutate(date = 1)
Regions$date <- seq(1, length(Regions$ILE.DE.FRANCE))
Regions = Regions %>%
  mutate(mois_num = ifelse(date%%12==0,12,date%%12),
         annee = date%/%12+2000)%>%
  mutate(mois=case_when(mois_num==1 ~ "janvier",
                        mois_num==2 ~ "février",
                        mois_num==3 ~ "mars",
                        mois_num==4 ~ "avril",
                        mois_num==5 ~ "mai",
                        mois_num==6 ~ "juin",
                        mois_num==7 ~ "juillet",
                        mois_num==8 ~ "août",
                        mois_num==9 ~ "septembre",
                        mois_num==10 ~"octobre",
                        mois_num==11 ~"novembre",
                        mois_num==12 ~"décembre"))%>%
  select(annee,mois,everything())

Region_nom=c("en Île-de-France","dans le Grand Est","dans les Hauts-de-France",
             "en Normandie","en Centre-Val de Loire","en Bourgogne-Franche-Comté",
             "dans les Pays de la Loire","en Bretagne","en Nouvelle-Aquitaine",
             "en Occitanie","en Auvergne-Rhône-Alpes",
             "en Provence-Alpes-Côte d'Azur")


################Lancement du Rmarkdown a proprement parler - #####################
options(encoding = 'UTF-8')
library("serad")
#render("InterimMensuel.Rmd", output_format = "word_document")
render("InterimMensuel.Rmd",
       word_document(reference_docx="word-template_adaptations.docx"))





################ Pendant le developpement du package #####################
#il faudra décommenter et tester avec TRUE
devtools::check(cran = FALSE)
#https://stackoverflow.com/questions/38312576/package-vignettes-not-available-in-r
#usethis::use_vignette("serad")
#https://community.rstudio.com/t/bypass-qpdf-checks/115691
#devtools::build_vignettes()  #https://stackoverflow.com/questions/38312576/package-vignettes-not-available-in-r
devtools::build()
devtools::document()
rmarkdown::render("vignettes/serad.Rmd")


#devtools::load_all()
#Le suivant nécessite de faire skip en ligne de commande
devtools::install(build_vignettes = T) #Il faudrait avoir TRUE
vignette("serad")
??serad
