



#leverage and outliers
iris_new <- iris
iris_new$Source <- "original"
#make outlier
iris_outlier <- data.frame(Petal.Length = c(2.5,12, 12.1),
                           Sepal.Length = c(5.4,8.9, 3), 
                           Source = "new")
iris_merged <- merge(iris_new, iris_outlier, all = T)

ggplot(iris_merged, aes(x=Petal.Length, y=Sepal.Length, color=Source)) +
  geom_point(size = 3) +
  ylab("Sepal Length")+ggtitle("Sepal length increases with petal length")+
  theme(axis.title.x = element_text(face="bold", size=28), 
        axis.title.y = element_text(face="bold", size=28), 
        axis.text.y  = element_text(size=20),
        axis.text.x  = element_text(size=20), 
        legend.text =element_text(size=20),
        legend.title = element_text(size=20, face="bold"),
        plot.title = element_text(hjust = 0.5, face="bold", size=32))+
  xlab("Petal length (cm)") +
  ylab("Sepal length (cm)")



iris_regression_merged <- lm(Sepal.Length ~ Petal.Length, iris_merged)
plot(iris_regression_merged)
iris_merged[1,]

iris_regression_merged <- lm(Sepal.Length ~ Petal.Length, iris_merged[-1,])
plot(iris_regression_merged)
