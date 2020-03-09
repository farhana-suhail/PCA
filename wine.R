#?gdata
#install.packages("gdata")
#library(gdata)
library(cluster)
install.packages("fpc")
library(fpc)
install.packages("NbClust")
library(NbClust)
library(ggplot2)



wine <- read.csv("G:/Data Science/ExcelR/LM Portal/Assignments/PCA/wine.csv",1)
View(wine[,-1])


# cor = TRUE use correlation matrix for getting PCA scores
help(princomp) #to understand the api for princomp
pca.wine <- princomp(wine[,-1], cor = TRUE, scores = TRUE,
                   covmat = NULL)
str(pca.wine)
summary(pca.wine)
#the first 7 variables contribte ~90% of the information required for the entire data. 
#Hence the 13 components can be reduced to 7 for furhter analysis with 90% information. 
#The standard deviation of the components is stored in a named element called "sdev" of the output variable made by "princomp":
 
pca.wine$sdev 

loadings(pca.wine)

plot(pca.wine)
biplot(pca.wine)
# Showing the increase of variance with considering principal components
# Which helps in choosing number of principal components
plot(cumsum(pca.wine$sdev*pca.wine$sdev)*100/(sum(pca.wine$sdev*pca.wine$sdev)),type="b")
#We can also extract the PC loadings
pca.wine$loadings

#Note:
#1.The function above provides the estimate loadings for the data for each component
#2.Note that some values are missing. These values are not zero but are close to zero and should be ignored as we only concern about the largest loadings in the absolute values.
#3.Take the first PC as an example, the three largest loadings are -0.423 for V8, -0.395 for V7, and -0.376 for V13.
#4.The first PC makes up 36.19% of the variability in the original data

#We can also extract the PC scores
winescores <- pca.wine$scores
winescores
#Top 3 PCA Scores which represents the whole data
pca.wine$scores[,1:3]

winescores[1:5,1] #extract the PC scores from the first 5 observations
#Determine how many principal components to retain
#screeplot
screeplot(pca.wine, main = "screeplot")
screeplot(pca.wine, type = "lines")
# cbind used to bind the data in column wise
# Considering top 3 principal component scores and binding them with mydata
wine <- c(wine,pca.wine$scores[,1:3])
View(wine)
#Cluster Analysis (considering only pca scores as they represent the entire data)

clust.wine <- NbClust(wine[], distance = "euclidean", min.nc = 2, max.nc = 10, method = "complete", index = "all")

#plot bar chart for the clusters
install.packages("factoextra")
library(factoextra)
fviz_nbclust(clust.wine)+theme_minimal()
dev.off()
#Hierarchical clustering - All Variables
hclust.complete = eclust(wine, "hclust", k=7, method = "complete", graph = FALSE)
fviz_dend(hclust.complete, rect = TRUE, show_labels = FALSE)

#K-means clustering - All Variables
kmeans.wine = eclust(wine, "kmeans", k=5, nstart = 25, graph = FALSE)
fviz_cluster(kmeans.wine, geom = "point", frame.type = "norm")

#Cluster Analysis- with PCA components

wine.pca = wine[,2:14]
no_clust = NbClust(wine.pca, distance = "euclidean", min.nc = 2, max.nc = 10, method = "complete", index = "all")

#Plot barchart for the clusters
fviz_nbclust(no_clust)+theme_minimal()

# H clust using PCA components
hclust.complete = eclust(wine.pca, "hclust", k=7, method = "complete", graph = FALSE)
fviz_dend(hclust.complete, rect = TRUE, show_labels = FALSE)

#K-means clustering - All Variables
kmeans.wine = eclust(wine.pca, "kmeans", k=5, nstart = 25, graph = FALSE)
fviz_cluster(kmeans.wine, geom = "point", frame.type = "norm")

#Observation(s):
  #When PCA was applied on the entire set of varibles (13); 
  #PCA suggested that 90% of the information can be inferred from the first 7 varaibles. 
  #We then plotted dendrogram for both 13 varaibles and 7 varaibles data and found that the number of clustered required are 7 and the dendrogram seem identical.




