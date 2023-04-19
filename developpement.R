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



################ Pendant le developpement du package #####################
#il faudra décommenter et tester avec TRUE
devtools::check(cran = FALSE)
#https://stackoverflow.com/questions/38312576/package-vignettes-not-available-in-r
#usethis::use_vignette("serad")
#https://community.rstudio.com/t/bypass-qpdf-checks/115691
#devtools::build_vignettes()  #https://stackoverflow.com/questions/38312576/package-vignettes-not-available-in-r
devtools::build(binary = TRUE)
devtools::build()
devtools::document()
#rmarkdown::render("vignettes/serad.Rmd")
#https://stackoverflow.com/questions/10373098/error-in-fetchkey-internal-error-3-in-r-decompress1


#devtools::load_all()
#Le suivant nécessite de faire skip en ligne de commande
devtools::install(build_vignettes = T)
#Si: ans runHook(".onLoad", env, package.lib, package) :internal error -3 in R_decompress1
#Alors relancer R
vignette("serad")
?g_nom #Si: Internal Server Error, relancer R.
?g_verbe
library("serad")
?gETa_verbe
