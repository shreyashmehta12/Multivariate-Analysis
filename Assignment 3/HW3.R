library(readr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(grid)
library(cowplot) 

#The dataset contains a set of 150 records under 5 attributes – Petal Length, Petal Width, Sepal Length, Sepal width and Class(Species).

iris <- read_csv("D:/Rutgers MITA/Spring 2023/MVA/Assignment 3/Iris.csv")
str(iris)
attach(iris)
summary(iris)
cor(iris[-5])

plot(iris[,-5])

library(GGally)
ggpairs(iris[,c("SepalLengthCm", "SepalWidthCm", "PetalLengthCm","PetalWidthCm","Species")],aes(color = factor(Species)))

p1 <- ggplot(iris, aes(x=SepalLengthCm, y=SepalWidthCm, color=Species)) + geom_point()
p2 <- ggplot(iris, aes(x=PetalLengthCm, y=PetalWidthCm, color=Species)) + geom_point()
p3 <- ggplot(iris, aes(x=SepalLengthCm, y=PetalLengthCm, color=Species)) + geom_point()
p4 <- ggplot(iris, aes(x=SepalWidthCm, y=PetalWidthCm, color=Species)) + geom_point()
plot_grid(p1, p2, p3, p4, labels = "AUTO")
#In this particular case, it seems that petal length and petal width are most distinct for the three species.


iris_pca <- prcomp(iris[,-5],scale=TRUE)
iris_pca
summary(iris_pca)
plot(iris_pca)
iris_pca

(eigen_iris <- iris_pca$sdev^2)
names(eigen_iris) <- paste("PC",1:4,sep="")
eigen_iris


sumlabdas<-sum(eigen_iris)
sumlabdas

propvar<-eigen_iris/sumlabdas
propvar

cumvar<-cumsum(propvar)
cumvar


matlambdas <- rbind(eigen_iris,propvar,cumvar)
rownames(matlambdas) <- c("Eigenvalues","Prop. variance","Cum. prop. variance")
round(matlambdas,4)

summary(iris_pca)

iris_pca$rotation

iris_pca$x


var <- get_pca_var(iris_pca)
var

plot(eigen_iris, xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree diagram")
library(factoextra)
library(corrplot)
library(devtools)
library(FactoMineR)
library(ggfortify)
library(psych)
# Correlation
pairs.panels(iris[,-5],
             gap = 0,
             bg = c("red", "blue")[iris$Species],
             pch=21)

diag(cov(iris_pca$x))
xlim <- range(iris_pca$x[,1])
plot(iris_pca$x,xlim=xlim,ylim=xlim)

library(vctrs)
library(purrr)

#Correlation circle
head(var$coord, 4)
fviz_eig(iris_pca, addlabels = TRUE)

fviz_pca_var(iris_pca,col.var = "cos2",
             gradient.cols = c("#FFCC00", "#CC9933", "#660033", "#330033"),
             repel = TRUE)


biplot(iris_pca)

#Quality of representation
head(var$cos2, 4)
corrplot(var$cos2, is.corr=FALSE)
# Color by cos2 values: quality on the factor map
fviz_pca_var(iris_pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

#Contributions of variables to PCs
head(var$contrib, 4)
library("corrplot")
corrplot(var$contrib, is.corr=FALSE)   
fviz_pca_var(iris_pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
# Contributions of variables to PC1
fviz_contrib(iris_pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(iris_pca, choice = "var", axes = 2, top = 10)
fviz_contrib(iris_pca, choice = "var", axes = 1:2, top = 10)


#Graphs of individuals

ind <- get_pca_ind(iris_pca)
ind

# Coordinates of individuals
head(ind$coord)
# Quality of individuals
head(ind$cos2)
# Contributions of individuals
head(ind$contrib)


fviz_pca_ind(iris_pca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)


fviz_pca_ind(iris_pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = iris$Species, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
)
ind.p <- fviz_pca_ind(iris_pca, geom = "point", col.ind = iris$Species)
ggpubr::ggpar(ind.p,
              title = "Principal Component Analysis",
              subtitle = "Iris data set",
              xlab = "PC1", ylab = "PC2",
              legend.title = "Species", legend.position = "top",
              ggtheme = theme_gray(), palette = "jco"
)


#anova testing
iris_anova <- aov(SepalLengthCm+SepalWidthCm ~ Species, data = iris)
summary(iris_anova)

#T-test can't be applied as there are 3 factors, i.e., 3 classes of Species.

