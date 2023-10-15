

    

  




#building to bootstrap options####
library(WRS2)
#first, just using trimmed means, which help
t1way(Sepal.Length~Species, iris)
contrasts <- lincon(Sepal.Length~Species, iris)
contrasts

#if we don't trim mean, we use Welch's approximation for ANOVA
t1way(Sepal.Length~Species, iris, tr = 0)
contrasts <- lincon(Sepal.Length~Species, iris, tr = 0)
contrasts

#to actually use the bootstrap version
t1waybt(Sepal.Length~Species, iris)
bootstrap_post_hoc <- mcppb20(Sepal.Length~Species, iris)
#use p.adjust to correct for FWER
p.adjust(as.numeric(bootstrap_post_hoc$comp[,6]), "holm")

#mcfarland example#### 

waterchem <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/McFarland_PLOSOne_2020_WaterChem.csv",
                      stringsAsFactors = T)
str(waterchem)

fit_waterchem <- lm(Total.Organic.Carbon~WaterSource, waterchem)
library(ggplot2)
library(ggfortify)
autoplot(fit_waterchem)
summary(waterchem)
Anova(fit_waterchem, type = "III")
library(multcomp)












