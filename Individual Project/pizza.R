#Importing required libraries
library(magrittr)
library(factoextra)
library(NbClust)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(grid)
library(factoextra)
library(corrplot)
library(ggcorrplot)
library(psych)
library(MASS)
library(memisc)

#Loading the dataset
pizza_data <- read.csv("D:/Rutgers MITA/Spring 2023/MVA/Individual Project/Pizza.csv")
head(pizza_data)

str(pizza_data)

#Removing and id and brand columns as they are not numerical and numerical can only be used for analysis
pizza_data <- pizza_data[,3:9]
head(pizza_data)

dim(pizza_data)
#The dataset contains 300 observations and 7 variables

#Data Preparation
norm_data <- scale(pizza_data)
head(norm_data)

corr <- cor(norm_data, method = "pearson")
ggcorrplot(corr)

#PCA
pca_data <- prcomp(norm_data)
summary(pca_data)

fviz_eig(pca_data, choice = "variance", addlabels = TRUE, barfill = "steelblue", linecolor = "red")

pca_data$rotation[,1:2]

eigenvalues <- summary(pca_data)$importance[2,]
eigenvalues

#Correlation Circle
fviz_pca_var(pca_data, col.var = "black")

#Quality of representation
fviz_cos2(pca_data, choice = "var", axes = 1:2)
fviz_pca_var(pca_data, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

#Contributions of variables to PCs
pca1<- fviz_contrib(pca_data, choice = "var", axes = 1, top = 6, title = "PCA1")
pca2<- fviz_contrib(pca_data, choice = "var", axes = 2, top = 6, title = "PCA2")
grid.arrange(pca1, pca2,  ncol=2)

#Individual representation
ind <- get_pca_ind(pca_data)
ind
fviz_pca_ind(pca_data, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)



#Kmeans Clustering

pizza_dist <- dist(norm_data, method = "euclidean")
fviz_dist(pizza_dist, show_labels = F)

nb <- NbClust(norm_data, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "kmeans")
km_data <- kmeans(norm_data, centers = 3, nstart = 35)
#The plot generates various clustering indices (such as the silhouette index, Dunn index, and gap statistic) for different numbers of clusters, allowing to visually identify the optimal number of clusters based on the chosen index.
fviz_cluster(list(data=norm_data, clusters=km_data$cluster))
#K-mean clustering was performed with 3 clusters on the pizza_data dataset. The centroid of each cluster is represented by a larger point with a white border.


#EFA
fa.parallel(pizza_data)
fit.pc <- principal(pizza_data, nfactors=3, rotate="varimax")
fit.pc
fa.plot(fit.pc)
fa.diagram(fit.pc)


#LDA
pizza_data <- read.csv("D:/Rutgers MITA/Spring 2023/MVA/Individual Project/Pizza.csv")
lda<-lda(pizza_data$brand ~ ., data = pizza_data)
summary(lda)
lda
plot(lda)
