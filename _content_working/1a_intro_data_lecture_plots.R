
#transformations####

sample_data <- data.frame(x = rnorm (1000, 1, 1))
sample_data$x_add <- sample_data$x+5

ggplot(sample_data) + 
  geom_histogram(aes(x =x, fill="x"), se = F) +
  geom_histogram(aes(x = x_add, fill="x+5"), se = F)+
  labs(fill="Data")+
  xlab("x") + 
  ylab("y") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=30),
        axis.text.x  = element_text(size=30), 
        legend.text =element_text(size=30),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

sample_data$x_multiply <- sample_data$x*5

ggplot(sample_data) + 
  geom_histogram(aes(x =x, fill="x"), se = F) +
  geom_histogram(aes(x = x_multiply, fill="x*5"), se = F)+
  labs(fill="Data")+
  xlab("x") + 
  ylab("y") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=30),
        axis.text.x  = element_text(size=30), 
        legend.text =element_text(size=30),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

#shift them left
summary(birds)
length(birds[birds < .9])
birds[birds < .9] <- birds[birds < .9] - .2
hist(birds, main="Weight of Westchester cardinals", xlab = "\n Weight (g)", 
     ylab = "Frequency (#)\n", col = "red", cex.lab=label_size, cex.axis=1.25, 
     cex.main=title_size, cex.sub=label_size)
abline(v=mean(birds), col="yellow", lwd = 4)
abline(v=median(birds), col="green", lwd = 4)
abline(v=(getmode(birds)), col="blue", lwd = 4)
legend(x=.75, y= 1000, legend = c("mean", "median", "mode"), fill=c("yellow","green", 
                                                                    
                                                                    "blue"), cex = 1.5,
       bty="n", x.intersp = .1, y.intersp = .5)

#categorical data####
head(iris)
iris$random <- runif(1:nrow(iris))
iris$LL <- 0
iris$LL[iris$random > .7] <- 1

ggplot(iris[iris$Species == "setosa", ], aes(LL)) + 
  geom_histogram(size=3) +
  xlab("Genotype score")+
  ylab("Frequency")+
  ggtitle("Genotype score in an iris species")+
  geom_vline(xintercept = mean(iris[iris$Species == "setosa", "LL"]), color = "blue") +
  annotate("text", label = "proportion", x = .25, y = 20 , size = 8, color = "blue") +
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))

