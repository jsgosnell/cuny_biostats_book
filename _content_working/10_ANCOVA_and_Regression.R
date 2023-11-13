#from lecture on ancova and multiple regression

#garlick example####

garlick <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/Garlick_et_al_data.csv",
                    stringsAsFactors = T)
str(garlick)

garlick_lm <- lm(root_focal_q1 ~ shoot_focal + soil_environment*neighbor, garlick)
library(car)
Anova(garlick_lm, type = "III")

coef(garlick_lm)
head(model.matrix(garlick_lm))





#TEAM example####
team <- read.csv("https://raw.githubusercontent.com/jsgosnell/CUNY-BioStats/master/datasets/team_data_no_spaces.csv")
names(team)
head(team)

#lets build a model looking at variation among continents####
#this is an ANOVA
team_model_1 <-lm(PlotCarbon.tonnes ~ Continent, team)
#plot to see if assumptions are met. remember what these are (from Lisa Manne)
# The Residuals vs. Fitted values plot should show no structure; it should not show a trend of residuals against fitted values. The variance should not increase or decrease as you move along the x-axis.  The residuals should be centered around 0.
# The Quantile-quantile plot tests whether your residuals are normally distributed.  It plots each data point vs. its position in a theoretical normal distribution.  If the residuals are normally distributed, the plot will look like a straight line.
# The scale-location plot is another plot of residuals vs. fitted values:  it shows the square root of the standardized residuals against the fitted values.  This plot may more clearly show if there is an issue with the variance increasing with the mean (in that case the scatter would increase as the fitted values increased).
# The residuals vs. leverage plot also includes Cook's distance (pale blue dashed line).  Cook's Distance compares the fitted response of the regression which uses every data point, against the fitted response of the regression where a particular data point has been dropped from the analysis (and then sums this difference across all data points). Very influential data points (on the parameter estimates) are identified, and are labeled in this plot.  If there are heavily influential data points, you might consider re-doing the regression model after removing them.
par(mfrow =c(2,2))
plot(team_model_1)
Anova(team_model_1, type = "III")
summary(team_model_1)

#where's the "almost" difference? use Tukey's HSD for all pairs
library(multcomp)
compare_cont_tukey <- glht(team_model_1, linfct = mcp(Continent = "Tukey"))
summary(compare_cont_tukey)

#we could do one by one,but we have a lot of variables
#instead, we can use top-down or bottom-up approaches to compare

#top-down approach####
#for top-down, start with a model containing all variables you may wish to include
#and interactions.  You should ensure these are not highly correlated before including
#pairs let you visualize this.let's try continent, shannon diversity, phylogenetic 
#diveristy, a few rao q measures, rainfall, and elevation
#
#for ease just pull those out (not required, just for viewing here)
team_potential <- team[,colnames(team) %in% c(#site specific
  "Continent",
  #diversity
  "shannon", "wd.RaoQ", "maxdbh.RaoQ", "CWM.wd", "CWM.maxdbh", "PD",
  #environmental
  "Precip_mean.mm", "Elevation",
  #outcome
  "PlotCarbon.tonnes")]

pairs(team_potential, lower.panel=panel.smooth, upper.panel=panel.cor)
#some significant relationships, and shannon and pd are highly correlated...may 
#not be the best idea, but let's continue on

#. means all other variables
team_model_full <- lm(PlotCarbon.tonnes ~ ., 
                      team_potential)
drop1(team_model_full)
#you can drop one at a time. remember goal is lowest AIC. this is still a step-down
#method

team_model_full_a <- update(team_model_full, .~. - PD)
drop1(team_model_full_a)

team_model_full_b <- update(team_model_full_a, .~. - CWM.wd)
drop1(team_model_full_b)
#... and so on until we see <none> as the lowest AIC
#can specify test as well. F is ok for linear model. then drop highest p-value and work
#down until all significant
drop1(team_model_full, test = "F")

#add1 uses same idea but for smallest model. build small model. 1 means intercept
team_model_empty <- lm(PlotCarbon.tonnes ~ 1, team_potential)
#then add whatever you may want to consider in the argument (just doing smaller
#set here for ease)
add1(team_model_empty, ~ shannon + wd.RaoQ )
# 
add1(team_model_empty, ~ shannon + wd.RaoQ, test = "F" )

#or R can do this for us. default is backwards
stepAIC(team_model_full)
#save as object to get output
stepAIC_final <- stepAIC(team_model_full)

#or we can go forward
stepAIC(team_model_empty, ~ shannon + wd.RaoQ, direction = "forward")

#remember to check assumptions for final model
plot(stepAIC_final)
#you can also check variance inflation factors (vif) for final model to see 
#if they are too big (>5 indicates high correlation among factors)
vif(stepAIC_final) #all good
Anova(stepAIC_final, type ="III")
summary(stepAIC_final)

#these are all step-wise methods. we can also use AIC to do a full model search
library(MuMIn)
?dredge
options(na.action = "na.fail")
auto <- dredge(team_model_full)
write.csv(auto, "dredge_output.csv", row.names = F)
#model.avg for output. can decide how far delta can go. look at dredge output
head(auto)
#can average top models
model.avg(auto, subset = delta < 4) #notes uses AICc, best for small samples or 
#where you have to estimate lots of parameters
#to get the top 1
top_model <- get.models(auto, subset = 1)[[1]]
#check
plot(top_model)
vif(top_model)

