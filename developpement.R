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
#https://iqss.github.io/dss-rbuild/package-development.html
#https://r-pkgs.org/testing-basics.html
devtools::test()
#il faudra décommenter et tester avec TRUE
devtools::check(cran = TRUE)
#https://stackoverflow.com/questions/38312576/package-vignettes-not-available-in-r
#usethis::use_vignette("serad")
#https://community.rstudio.com/t/bypass-qpdf-checks/115691
#devtools::build_vignettes()  #https://stackoverflow.com/questions/38312576/package-vignettes-not-available-in-r
#devtools::build(binary = TRUE)
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
?plushautniveau


################ !!!! sticker !!!! #####################
#?sticker
#https://shixiangwang.github.io/home/en/post/2019-06-20-how-i-create-ucscxenatools-logo/
library(hexSticker)
library(ggplot2)
p <- ggplot(aes(x = mpg, y = wt), data = mtcars[mtcars$mpg==21,]) + geom_point()
p <- p + theme_void() + theme_transparent()
outfile <- tempfile(fileext=".png")
file = "C:/Users/alexandre.cazenave-l/Documents/GitHub/Serad/man/figures/logo.png"
#file = "logo.png"
sticker(p,
        package="serad ",
        u_color = "white", u_size = 1,
        h_fill="aquamarine3", h_color="white",
        p_size = 30,p_family="mono", #I would prefer : Futura Std Extra Bold"
        #http://www.sthda.com/french/wiki/couleurs-dans-r
        p_color = "blue4",
        filename=file)

#tentative pour obtenir la police du logo de la Dares
# "Futura Std Extra Bold"
# library(extrafont) https://github.com/wch/extrafont
# font_import()

library(usethis)
use_logo(img = file)
