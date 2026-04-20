

.onLoad <- function(libname, pkgname) {

# ###                          Parametres globaux                          -----

  serad0 <- list()

  # arrondis
  serad0$arrondi_niv <- -2 # arrondi à la centaine
  serad0$arrondi_pourcent <- 1 # arrondi à un chiffre après la virgule

  # gestion du -
  serad0$moins <- "\u2212"

# ###                           Evolution simple                           -----

  # Evolutions
  evo_simple <- tibble::tribble(
    ~seuil, ~verbe_sing, ~verbe_plur, ~nom,

    9.95,   "bondit de",                                   "bondissent de",                                   "une forte hausse",
    3.95,   "s'accroit de",                                "s'accroissent de",                                "une forte hausse",
    0.95,   "est en hausse de",                            "sont en hausse de",                               "une hausse",
    0.25,   "augmente de",                                 "augmentent de",                                   "une hausse mod\u00e9r\u00e9e",
    0.05,   "s'accroit tr\u00e8s l\u00e9g\u00e8rement de", "s'accroissent tr\u00e8s l\u00e9g\u00e8rement de", "une l\u00e9g\u00e8re hausse",
    -0.15,  "est stable",                                  "sont stables",                                    "une stabilit\u00e9",
    -0.35,  "diminue l\u00e9g\u00e8rement de",             "diminuent l\u00e9g\u00e8rement de",               "une l\u00e9g\u00e8re baisse",
    -1.05,  "recule l\u00e9g\u00e8rement de",              "reculent l\u00e9g\u00e8rement de",                "une baisse mod\u00e9r\u00e9e",
    -4.05,  "baisse de",                                   "baissent de",                                     "une baisse",
    -20.05, "recule de",                                   "reculent de",                                     "une forte baisse",
    -Inf,   "chute de",                                    "chutent de",                                      "une forte baisse"
  )

  serad0$evo_simple <- evo_simple

# ###                      Evolution avec accélération                     -----

  # Seuils
  serad0$seuil <- list(
    stable = 0.05,
    g2_bas = -0.5,
    g2_haut = 0.95,
    g1_bas = -10,
    g1_tres_bas = -20,
    accel_hausse = 30,
    accel_baisse = -30,
    accel_recul = 30
  )

  # Evolutions
  evo_accel <- tibble::tribble(
    ~cond_g1, ~cond_g2, ~cond_a, ~verbe_sing, ~verbe_plur, ~nom,

    # Hausse forte
    "g1 >= seuil_stable", "g2 >= seuil_stable", "a > seuil_accel_hausse",
    "acc\u00e9l\u00e8re", "acc\u00e9l\u00e8rent", "une acc\u00e9l\u00e9ration",

    "g1 >= seuil_stable", "g2 >= seuil_stable", "a < seuil_accel_baisse",
    "ralentit", "ralentissent", "un ralentissement",

    "g1 >= seuil_stable", "g2 >= seuil_stable", "a >= seuil_accel_baisse & a <= seuil_accel_hausse",
    "poursuit sa hausse", "poursuivent leur hausse", "une poursuite de la hausse",

    "g1 >= seuil_stable", "g2 >= seuil_g2_bas & g2 < seuil_stable", "TRUE",
    "augmente", "augmentent", "une hausse",

    "g1 >= seuil_stable", "g2 < seuil_g2_bas", "TRUE",
    "repart \u00e0 la hausse", "repartent \u00e0 la hausse", "un rebond",

    # Stabilité
    "abs(g1) < seuil_stable", "abs(g2) >= seuil_stable", "TRUE",
    "se stabilise", "se stabilisent", "une stabilisation",

    "abs(g1) < seuil_stable", "abs(g2) < seuil_stable", "TRUE",
    "reste stable", "restent stables", "une stabilit\u00e9",

    # Baisse
    "g1 >= seuil_g1_bas & g1 < -seuil_stable", "g2 > seuil_g2_haut", "TRUE",
    "recule", "reculent", "un recul",

    "g1 >= seuil_g1_bas & g1 < -seuil_stable", "g2 >= -seuil_stable & g2 <= seuil_g2_haut", "TRUE",
    "baisse", "baissent", "une baisse",

    "g1 >= seuil_g1_bas & g1 < -seuil_stable", "g2 < -seuil_stable", "a > seuil_accel_recul",
    "recule \u00e0 nouveau", "reculent \u00e0 nouveau", "un nouveau recul",

    "g1 >= seuil_g1_bas & g1 < -seuil_stable", "g2 < -seuil_stable", "a <= seuil_accel_recul",
    "poursuit sa baisse", "poursuivent leur baisse", "une poursuite de la baisse",

    # Baisse forte
    "g1 >= seuil_g1_tres_bas & g1 < seuil_g1_bas", "TRUE", "TRUE",
    "baisse fortement", "baissent fortement", "une forte baisse",

    "g1 < seuil_g1_tres_bas", "TRUE", "TRUE",
    "chute", "chutent", "une chute"
  )

  evo_accel_alt <- tibble::tribble(
    ~verbe_sing_alt, ~verbe_plur_alt, ~nom_alt,

    "augmente plus vite", "augmentent plus vite", "un regain de dynamisme",
    "se mod\u00e8re", "se mod\u00e8rent", "un essoufflement",
    "continue d'augmenter", "continuent d'augmenter", "le prolongement de la hausse",
    "progresse", "progressent", "une progression",
    "se redresse", "se redressent", "un redressement",
    "se fige", "se figent", "une stabilisation",
    "demeure stable", "demeurent stables", "une stabilit\u00e9",
    "se replie", "se replient", "un repli",
    "diminue", "diminuent", "une diminution",
    "repart \u00e0 la baisse", "repartent \u00e0 la baisse", "un nouveau recul",
    "poursuit sa baisse", "poursuivent leur baisse", "une poursuite de la baisse",
    "se replie fortement", "se replient fortement", "un fort repli",
    "s'effondre", "s'effondrent", "un effondrement"
  )

  serad0$evo_accel <- evo_accel
  serad0$evo_accel_alt <- evo_accel_alt

# ###                      Enregistrement des options                      -----

  options(serad = serad0)
}
