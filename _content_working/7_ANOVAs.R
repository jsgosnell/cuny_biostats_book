

    

  



#orthogonal
contr <- rbind("versicolor - virginica - setosa" = c(-1,.5,.5),
               "virginica - setosa" = c(0,-1,1))
summary(glht(iris_anova, linfct = mcp(Species = contr)))
summary(iris_anova) # this appears to be right


#graphical comparison####
#add comparison portion to fuction_output
function_output$comparison <- "NA"
#enter by hand for small groups by comparing function_output means with multcomp 
#output (usually tukey)
function_output
summary(compare_cont_tukey)
#all different here, so
function_output$comparison <- letters[1:3]
ggplot(function_output, aes(x=Species, y=Sepal.Length)) +
  geom_point(aes(fill=Species), size = 3) +
  geom_errorbar(aes(ymin=Sepal.Length-ci, ymax=Sepal.Length+ci), size=1.5) +
  ylab("Sepal Length (cm)")+ggtitle("Sepal Length of various iris species")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))+
  #offset from mean by hand
  geom_text(aes(x = Species, y = mean + .35, label = comparison), size = 28)

#another option for labelling if you are doing tukey is to automate (easy for more comparisons)
post_hoc_spray_cld <- cld(compare_cont_tukey)
#fortify to make a dataframe
post_hoc_spray_cld_fortify <- fortify(post_hoc_spray_cld)
#rename lhs to group and then merge (rename to whatever you used as groupvar in 
#summary SE)
names(post_hoc_spray_cld_fortify)[names(post_hoc_spray_cld_fortify) == "lhs"] <- "Species"
function_output <- merge(function_output, post_hoc_spray_cld_fortify)
#plot
ggplot(function_output, aes(x=Species, y=Sepal.Length)) +
  geom_point(aes(fill=Species), size = 3) +
  geom_errorbar(aes(ymin=Sepal.Length-ci, ymax=Sepal.Length+ci), size=1.5) +
  ylab("Sepal Length (cm)")+ggtitle("Sepal Length of various iris species")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))+
  #offset from mean by hand
  geom_text(aes(x = Species, y = Sepal.Length + .35, label = letters), size = 28)


#kruskal.wallis####
kruskal.test(Sepal.Length ~ Species, data = iris)
pairwise.wilcox.test(iris$Sepal.Length, 
                          iris$Species, 
                          p.adjust.method="holm")

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












