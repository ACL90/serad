#
#
# .onLoad <- function(libname, pkgname) {
#
#   # listes
#   serad0 <- list()
#
#   serad0$sve    <- list()
#   serad0$verbev <- list()
#   serad0$seuil  <- list()
#   serad0$verbes <- list()
#   serad0$nomse  <- list()
#   serad0$nm     <- list()
#
#   # arrondis
#   serad0$arrondi_niv <- -2 # arrondi à la centaine
#   serad0$arrondi_pourcent <- 1 # arrondi à un chiffre après la virgule
#
#   # gestion du -
#   serad0$moins <- "\u2212"
#
#   # pour decider si on veut de l'alea dans les verbes : 0 (rd <- 0.5) ou 1 (rd aléatoire)
#   serad0$alea_ind <- 1
#
# # ###                      Evolution sans accélération                     -----
#
#   # seuils verbes
#     serad0$sve$forttttt    <- 9.95
#     serad0$sve$fortttt     <- 3.95
#     serad0$sve$forttt      <- 0.95
#     serad0$sve$fortt       <- 0.25
#     serad0$sve$fort        <- 0.05
#     serad0$sve$faible      <- -0.15
#     serad0$sve$faiblee     <- -0.35
#     serad0$sve$faibleee    <- -1.05
#     serad0$sve$faibleeee   <- -4.05
#     serad0$sve$faibleeeee  <- -20.05
#
#   # verbes
#     serad0$verbev$forttttt_sing    <- "bondit de"
#     serad0$verbev$fortttt_sing     <- "s'accroit de"
#     serad0$verbev$forttt_sing      <- "est en hausse de"
#     serad0$verbev$fortt_sing       <- "augmente de"
#     serad0$verbev$fort_sing        <- "s'accroit tr\u00e8s l\u00e9g\u00e8rement de"
#     serad0$verbev$faible_sing      <- "est stable"
#     serad0$verbev$faiblee_sing     <- "diminue l\u00e9g\u00e8rement de"
#     serad0$verbev$faibleee_sing    <- "recule l\u00e9g\u00e8rement de"
#     serad0$verbev$faibleeee_sing   <- "baisse de"
#     serad0$verbev$faibleeeee_sing  <- "recule de"
#     serad0$verbev$faibleeeeee_sing <- "chute de"
#
#
#     serad0$verbev$forttttt_plur       <- "bondissent de"
#     serad0$verbev$fortttt_plur        <- "s'accro\u00eessent de"
#     serad0$verbev$forttt_plur         <- "sont en hausse de"
#     serad0$verbev$fortt_plur          <- "augmentent de"
#     serad0$verbev$fort_plur           <- "s'accro\u00eessent tr\u00e8s l\u00e9g\u00e8rement de"
#     serad0$verbev$faible_plur         <- "sont stables"
#     serad0$verbev$faiblee_plur        <- "diminuent l\u00e9g\u00e8rement de"
#     serad0$verbev$faibleee_plur       <- "reculent l\u00e9g\u00e8rement de"
#     serad0$verbev$faibleeee_plur      <- "baissent de"
#     serad0$verbev$faibleeeee_plur     <- "reculent de"
#     serad0$verbev$faibleeeeee_plur    <- "chutent de"
#
#   # seuils noms
#     serad0$nomse$fortttt   <- 3.95
#     serad0$nomse$forttt    <- 0.95
#     serad0$nomse$fortt     <- 0.35
#     serad0$nomse$fort      <- 0.05
#     serad0$nomse$faible    <- -0.05
#     serad0$nomse$faiblee   <- -0.35
#     serad0$nomse$faibleee  <- -1.05
#     serad0$nomse$faibleeee <- -4.05
#
#   # noms
#     serad0$nm$fortttt    <- "une forte hausse"
#     serad0$nm$forttt     <- "une hausse"
#     serad0$nm$fortt      <- "une hausse mod\u00e9r\u00e9e"
#     serad0$nm$fort       <- "une l\u00e9g\u00e8re hausse"
#     serad0$nm$faible     <- "une stabilit\u00e9"
#     serad0$nm$faiblee    <- "une l\u00e9g\u00e8re baisse"
#     serad0$nm$faibleee   <- "une baisse mod\u00e9r\u00e9e"
#     serad0$nm$faibleeee  <- "une baisse"
#     serad0$nm$faibleeeee <- "une forte baisse"
#
# # ###                      Evolution avec accélération                     -----
#
#   # seuils
#     serad0$seuil$stable    <- 0.05
#     serad0$seuil$g2bas     <- -0.5
#     serad0$seuil$afort     <- 30
#     serad0$seuil$dfort     <- -30
#     serad0$seuil$g1tresbas <- -20
#     serad0$seuil$g1bas     <- -10
#     serad0$seuil$g2haut    <- 0.95
#     serad0$seuil$afort2    <- 30
#
#   # verbes
#     serad0$verbes$AAsing <- "reste stable"
#     serad0$verbes$BAsing <- "se stabilise"
#     serad0$verbes$CAsing <- "repart \u00e0 la hausse"
#     serad0$verbes$CBsing <- "se redresse"
#     serad0$verbes$DAsing <- "augmente"
#     serad0$verbes$DBsing <- "est en hausse"
#     serad0$verbes$EAsing <- "acc\u00e9l\u00e8re"
#     serad0$verbes$FAsing <- "ralentit"
#     serad0$verbes$FBsing <- "se mod\u00e8re"
#     serad0$verbes$GAsing <- "poursuit sa hausse"
#     serad0$verbes$HAsing <- "recule"
#     serad0$verbes$HBsing <- "se replie"
#     serad0$verbes$IAsing <- "baisse"
#     serad0$verbes$IBsing <- "diminue"
#     serad0$verbes$JAsing <- "recule \u00e0 nouveau"
#     serad0$verbes$JBsing <- "poursuit son recul"
#     serad0$verbes$KAsing <- "poursuit sa baisse"
#     serad0$verbes$KBsing <- "continue \u00e0 baisser"
#     serad0$verbes$LAsing <- "chute"
#     serad0$verbes$MAsing <- "se replie fortement"
#
#     serad0$verbes$AAplur <- "restent stables"
#     serad0$verbes$BAplur <- "se stabilisent"
#     serad0$verbes$CAplur <- "repartent \u00e0 la hausse"
#     serad0$verbes$CBplur <- "se redressent"
#     serad0$verbes$DAplur <- "augmentent"
#     serad0$verbes$DBplur <- "sont en hausse"
#     serad0$verbes$EAplur <- "acc\u00e9l\u00e8rent"
#     serad0$verbes$FAplur <- "ralentissent"
#     serad0$verbes$FBplur <- "se mod\u00e8rent"
#     serad0$verbes$GAplur <- "poursuivent leur hausse"
#     serad0$verbes$HAplur <- "reculent"
#     serad0$verbes$HBplur <- "se replient"
#     serad0$verbes$IAplur <- "baissent"
#     serad0$verbes$IBplur <- "diminuent"
#     serad0$verbes$JAplur <- "reculent \u00e0 nouveau"
#     serad0$verbes$JBplur <- "poursuivent leur recul"
#     serad0$verbes$KAplur <- "poursuivent leur baisse"
#     serad0$verbes$KBplur <- "continuent \u00e0 baisser"
#     serad0$verbes$LAplur <- "chutent"
#     serad0$verbes$MAplur <- "se replient fortement"
#
#   # noms
#     serad0$noms$AA	<- "une stabilit\u00e9"
#     serad0$noms$BA	<- "une stabilisation"
#     serad0$noms$CA	<- "un rebond"
#     serad0$noms$CB	<- "un rebond"
#     serad0$noms$DA	<- "une hausse"
#     serad0$noms$DB	<- "une hausse"
#     serad0$noms$EA	<- "une acc\u00e9l\u00e9ration"
#     serad0$noms$FA	<- "un ralentissement"
#     serad0$noms$FB	<- "un ralentissement"
#     serad0$noms$GA	<- "une poursuite de la hausse"
#     serad0$noms$HA	<- "un repli"
#     serad0$noms$HB	<- "un repli"
#     serad0$noms$IA	<- "une baisse"
#     serad0$noms$IB	<- "une baisse"
#     serad0$noms$JA	<- "un nouveau recul"
#     serad0$noms$JB	<- "un nouveau recul"
#     serad0$noms$KA	<- "une poursuite de la baisse"
#     serad0$noms$KB	<- "une poursuite de la baisse"
#     serad0$noms$LA	<- "une chute"
#     serad0$noms$MA	<- "une chute"
#
#   options(serad = serad0)
# }
