# ======= dotation globale ========
# objectif : 
library("doBy", lib.loc="C:/R-3.5.0/library")
library("gridExtra", lib.loc="C:/R-3.5.0/library")
library(forcats) 

# 00 charge lib -----------------------------------------------------------
library(sf)
library(spatstat)
library(sp)
library(maptools)
library(raster)
library(cartography)
library(SpatialPosition)
library(ggplot2)
library(ggplotgui)
library(doBy)
library(gghighlight)
library(dplyr)
library(rgdal)
library(remotes)
library(readr)
library("gghighlight", lib.loc="C:/R-3.5.0/library")
library(ggridges)
library("viridis", lib.loc="C:/R-3.5.0/library")
library(GGally)
library(RColorBrewer)
library(colorRamps)
library(ggrepel)
# devtools::install_github('thomasp85/gganimate')
library(gganimate)
library(ggpubr)
library(reshape2)

# ==== travail sur la table uniquement ====
setwd("G:/00_data_ref/data_gouv_fr/dotation")
# http://www.milanor.net/blog/aggregation-dplyr-summarise-summarise_each/
# 01 travail table --------------------------------------------------------
df14 <- read_csv("2014.csv")
df14$annee ="2014"
df15 <- read_csv("2015.csv")
df15$annee ="2015"
df16 <- read_csv("2016.csv")
df16$annee ="2016"
df17 <- read_csv("2017.csv")
df17$annee ="2017"
View(head(df16))
dftot <- rbind(df14,df15,df16,df17)
dim(dftot)
View(dftot)

summary(dftot$dotation_forfaitaire)
df0 <- select(dftot , depcom,commune,annee, dotation_forfaitaire)
df0$dep <- substr(df0$depcom,1,2)

df21 <- subset(df0, dep=='21')
dim(df21)

# histogramme de distribution
# hist(df0$dotation_forfaitaire)
#ggplot(df0, aes(dotation_forfaitaire)) + geom_histogram(bins=500) + coord_cartesian(xlim=c(0,median(dftot$dotation_forfaitaire)))
#ggplot(df0, aes(dotation_forfaitaire)) + geom_density() + coord_cartesian(xlim=c(0,5000000))
# binwidth= 70
max(df0$dotation_forfaitaire)

# https://stackoverflow.com/questions/47233841/r-cut-by-breaks-and-count-number-of-occurrences-by-group
summary(df0$dotation_forfaitaire)
df0$Class=cut(df0$dotation_forfaitaire,c(0,25000,50000,100000,Inf),labels=c('0-25000','25000-50000','50000-100000','>100000'))
library(dplyr)

library(tidyverse)
# df0 <- subset(df0, dep != "2A" & dep != "2B")
length(unique(df0$depcom))

nb_rod <- do.call(data.frame , aggregate(formula = depcom ~ Class +  annee  , data = df0, FUN = function(x) c(nb = length(x) ) ))
dim(nb_rod)
df_bfc <- subset(df0, dep == 21 | dep == 25 | dep == 39 | dep == 58 | dep == 70 | dep == 71 | dep == 89 | dep == 90)
nb_rod_bfc <- do.call(data.frame , aggregate(formula = depcom ~ Class +  annee  , data = df_bfc, FUN = function(x) c(nb = length(x) ) ))
nb_rod_bfc0 <- subset(nb_rod, dep == 21 | dep == 25 | dep == 39 | dep == 58 | dep == 70 | dep == 71 | dep == 89 | dep == 90)

df_bfc$Class=cut(df_bfc$dotation_forfaitaire,c(0,25000,50000,100000,Inf),labels=c('0-25000','25000-50000','50000-100000','>100000'))

nb_rod_bfc

ggplot(nb_rod, aes(x=Class, y=depcom)) + geom_bar(stat='identity', position = "dodge", alpha=0.60) + facet_wrap(.~annee, ncol=4) + ggplot(nb_rod_bfc, aes(x=Class, y=depcom)) + geom_bar(stat='identity', position = "dodge", alpha=0.20) + facet_wrap(.~annee, ncol=4)
ggplot(nb_rod, aes(x=Class, y=depcom)) + geom_bar(stat='identity', position = "dodge", alpha=0.60) + facet_wrap(.~annee, ncol=4) + geom_bar(data=nb_rod_bfc, aes(x=Class, y=depcom), fill="blue", stat='identity', position = "dodge", alpha=0.20) + facet_wrap(.~annee, ncol=4)


nb_rod_bfc_dep <- do.call(data.frame , aggregate(formula = depcom ~ dep + Class +  annee  , data = df_bfc, FUN = function(x) c(nb = length(x) ) ))
ggplot(nb_rod_bfc_dep, aes(x=Class, y=depcom, fill=dep)) + geom_bar(stat='identity', position = "dodge") + facet_wrap(.~annee, ncol=4)

# ==== representation a garde pour montrer l evolution et la baisse de la dotation
ggplot(nb_rod_bfc_dep, aes(x=Class, y=depcom, fill=annee)) + geom_bar(stat='identity', position = "dodge") + facet_wrap(dep~., ncol=4) + scale_fill_grey() + theme_bw()


ggplot(df0 , aes(y=dotation_forfaitaire, x=dep, fill=annee, color=annee)) + geom_point() + scale_fill_grey() + scale_color_grey() + theme_bw() 
# +  gghighlight(dotation_forfaitaire > 124700) 

ggplot(df_bfc , aes(y=dotation_forfaitaire, x=dep, fill=annee, color=annee, size= dotation_forfaitaire)) + geom_point() + scale_fill_grey() + scale_color_grey() + theme_bw() 

ggplot(df_bfc , aes(y=dotation_forfaitaire, x=dep, fill=annee, color=annee, size= dotation_forfaitaire)) + scale_color_brewer(palette="Reds") + geom_jitter() + theme_bw() 
display.brewer.all()
# + gghighlight(dep == 21 | dep == 25 | dep == 39 | dep == 58 | dep == 70 | dep == 71 | dep == 89 | dep == 90)

ggplot(subset(df0, dep!=75) , aes(x=dotation_forfaitaire, fill=dep)) + geom_bar(position = "dodge")

titi <- subset(df0, dep!=75)
summary(titi$dotation_forfaitaire, titi$dep)



ggplot(nb_rod, aes(x=Class, y=depcom,fill=dep)) + geom_bar(stat='identity', position = "fill") 

+ facet_wrap(.~dep, scales="free")
g

toto <- table(summary(df0$dotation_forfaitaire))

dfin <-dcast(df0, depcom+commune~annee)
head(dfin)

dfin$dep <- substr(dfin$depcom,1,2)
head(dfin)

dfin$d15_14 <- dfin$'2015' - dfin$'2014'
dfin$d16_15 <- dfin$'2016' - dfin$'2015'
dfin$d17_16 <- dfin$'2017' - dfin$'2016'

head(dfin)

dfin$ratio_15_14 <- (dfin$d15_14 / dfin$'2015')*100
dfin$ratio_16_15 <- (dfin$d16_15 / dfin$'2016')*100
dfin$ratio_17_16 <- (dfin$d17_16 / dfin$'2017')*100
head(dfin)


# https://rdrr.io/cran/dplyr/man/top_n.html

if (require("Lahman")) {
  # Find 10 players with most games
  # A little nicer with %>%
  tbl_df(Batting) %>%
    group_by(playerID) %>%
    tally(G) %>%
    top_n(10)
  # Find year with most games for each player
  tbl_df(Batting) %>% group_by(playerID) %>% top_n(1, G)
}

# trouver les 20 communes ayant le plus grosses dotations
# tbl_df(dfin) %>%
#   group_by(dep) %>%
#   tally(depcom) %>%
#   top_n(20)

# top_n(dfin, 10, '2014') juste la select des 10 premersi rows :(

top20 <- dfin %>%
  arrange(desc(dfin$'2014')) %>%
  slice(1:20)
  
# selection des 40 plus grosses valeurs quelques soit l annee
top20 <- df0 %>%
  arrange(desc(df0$'dotation_forfaitaire')) %>%
  slice(1:100)

# Trouver les 8 plus grosses pour chaque département
toptest <- tbl_df(df0) %>% group_by(dep) %>% top_n(8, dotation_forfaitaire)

ggplot(top20, aes(x=commune, y=dotation_forfaitaire,fill=annee)) + geom_bar(stat='identity', position = "dodge") + facet_wrap(.~dep, scales="free")
ggplot(toptest, aes(x=commune, y=dotation_forfaitaire,fill=annee)) + geom_bar(stat='identity', position = "dodge") + facet_wrap(.~dep, scales="free")


#

summary(df0$dotation_forfaitaire)
# summary par dep
# https://stackoverflow.com/questions/9847054/how-to-get-summary-statistics-by-group

tapply(df0$dotation_forfaitaire, df0$dep, summary)


library(purrr)
list_sumdep <- df0 %>% split(.$dep) %>% map(summary)

names(top20)

# dataviz
head(df0)
ggplot(df0, aes(x=dep, y=dotation_forfaitaire,fill=annee)) + geom_boxplot() + facet_wrap(.~dep, scales="free")

ggplot(dfin, aes(x=dep, y=d15_14))


