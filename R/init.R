
#https://stackoverflow.com/questions/30411691/auto-set-options-in-own-package
#https://github.com/eddelbuettel/rpushbullet/blob/master/R/init.R
.onLoad <- function(libname,pkgname) {


  #options(serad.a = 1)
  #options(serad = list())
  #serad0 = list(x=1,y="2")

  serad0 = list()

  #pour decider si on veut de l alea dans les verbes : 0 (rd=0.5) ou 1 (rd aléatoire)
  serad0$aleaDummy = 1

  #seuils dans g_verbe0
  #serad$seuilv
    serad0$seuilv$forttttt    = (10-0.05)
    serad0$seuilv$fortttt     = (4-0.05)
    serad0$seuilv$forttt      = (1-0.05)
    serad0$seuilv$fortt       = (0.3-0.05)
    serad0$seuilv$fort        = (0.1-0.05)
    serad0$seuilv$faible      = (-0.1-0.05)
    serad0$seuilv$faiblee     = (-0.3-0.05)
    serad0$seuilv$gaibleee    = (-1-0.05)
    serad0$seuilv$faibleeee   = (-4-0.05)
    serad0$seuilv$faibleeeee  = (-20-0.05)

  #verbes utilises dans g_verbe0
  #serad$verbe0
    serad0$verbev$forttttt_sing    = "bondit de"
    serad0$verbev$forttttt_plur    = "bondissent de"
    serad0$verbev$fortttt_sing    = "s'accroit de"
    serad0$verbev$fortttt_plur    = "s'acroissent de"
    serad0$verbev$forttt_sing    = "est en hausse de"
    serad0$verbev$forttt_plur    = "sont en hausse de"
    serad0$verbev$fortt_sing    = "augmente de"
    serad0$verbev$fortt_plur    = "augmentent de"
    serad0$verbev$fort_sing    = "s'accroit tr\u00e8s l\u00e9g\u00e8rement de"
    serad0$verbev$fort_plur    = "s'acroissent tr\u00e8s l\u00e9g\u00e8rement de"
    serad0$verbev$faible_sing    = "est stable \u00e0"
    serad0$verbev$faible_plur    = "sont stables \u00e0"
    serad0$verbev$faiblee_sing    = "diminue l\u00e9g\u00e8rement de"
    serad0$verbev$faiblee_plur    = "diminuent l\u00e9g\u00e8rement de"
    serad0$verbev$faibleee_sing    = "recule l\u00e9g\u00e8rement de"
    serad0$verbev$faibleee_plur    = "reculent l\u00e9g\u00e8rement de"
    serad0$verbev$faibleeee_sing    = "baisse de"
    serad0$verbev$faibleeee_plur    = "baissent de"
    serad0$verbev$faibleeeee_sing    = "recule de"
    serad0$verbev$faibleeeee_plur    = "reculent de"
    serad0$verbev$faibleeeeee_sing    = "chute de"
    serad0$verbev$faibleeeeee_plur    = "chutent de"



  #seuils dans gETa_verbe00
  #serad$seuil #heuristique
  serad0$seuil$stable    = 1
  serad0$seuil$g2bas     = (-0.5)
  serad0$seuil$afort     = 30
  serad0$seuil$dfort     = (-30)
  serad0$seuil$g1tresbas = (-20)
  serad0$seuil$g1bas     = (-10)
  serad0$seuil$g2haut    = (0.95)
  serad0$seuil$afort2    = 30

  #verbes utilises dans gETa_verbe0
  #serad$verbes
  serad0$verbes$AAsing = "reste stable"
  serad0$verbes$AAplur = "restent stables"
  serad0$verbes$BAsing = "se stabilise"
  serad0$verbes$BAplur = "se stabilisent"
  serad0$verbes$CAsing = "repart \u00e0 la hausse"
  serad0$verbes$CAplur = "repartent \u00e0 la hausse"
  serad0$verbes$CBsing = "se redresse"
  serad0$verbes$CBplur = "se redressent"
  serad0$verbes$DAsing = "augmente"
  serad0$verbes$DAplur = "augmentent"
  serad0$verbes$DBsing = "est en hausse"
  serad0$verbes$DBplur = "sont en hausse"
  serad0$verbes$EAsing = "acc\u00e9l\u00e8re"
  serad0$verbes$EAplur = "acc\u00e9l\u00e8rent"
  serad0$verbes$FAsing = "ralentit"
  serad0$verbes$FAplur = "ralentissent"
  serad0$verbes$FBsing = "se mod\u00e8re"
  serad0$verbes$FBplur = "se mod\u00e8rent"
  serad0$verbes$GAsing = "poursuit sa progression"
  serad0$verbes$GAplur = "poursuivent leur progression"
  serad0$verbes$HAsing = "recule"
  serad0$verbes$HAplur = "reculent"
  serad0$verbes$HBsing = "se replie"
  serad0$verbes$HBplur = "se replient"
  serad0$verbes$IAsing = "baisse"
  serad0$verbes$IAplur = "baissent"
  serad0$verbes$IBsing = "diminue"
  serad0$verbes$IBplur = "diminuent"
  serad0$verbes$JAsing = "recule \u00e0 nouveau"
  serad0$verbes$JAplur = "reculent \u00e0 nouveau"
  serad0$verbes$JBsing = "poursuit son recul"
  serad0$verbes$JBplur = "poursuivent leur recul"
  serad0$verbes$KAsing = "poursuit sa baisse"
  serad0$verbes$KAplur = "poursuivent leur baisse"
  serad0$verbes$KBsing = "continue \u00e0 baisser"
  serad0$verbes$KBplur = "continuent \u00e0 baisser"
  serad0$verbes$LAsing = "chute"
  serad0$verbes$LAplur = "chutent"
  serad0$verbes$MAsing = "se replie fortement"
  serad0$verbes$MAplur = "se replient fortement"



  options(serad = serad0)

  #getOption("serad")

}