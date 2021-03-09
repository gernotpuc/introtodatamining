# The following code was produced as coursework for the class 'Introduction to Data Mining'
# at the University of Dundee.
#
# It includes various visualizations to show the influence on the random initial
# configuration of centroids on the outcome of k-means.
#
# The following code sources were used:
#
# Code example for k-means, provided by Mark Whitehorn in the course 
# 'Introduction to Data Mining' at the University of Dundee.
# 
# kmeans function:
# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/kmeans
#
# For creating scatterplots:
# https://stackoverflow.com/questions/7714677/scatterplot-with-too-many-points
# https://ggplot2.tidyverse.org/reference/geom_hex.html
#
# For creating silhouette plots:
# https://www.rdocumentation.org/packages/factoextra/versions/1.0.5/topics/fviz_silhouette

library(ggplot2)
library(hexbin)
library(cowplot)
library (cluster)
library (clustertend)
library (factoextra)
library(dplyr)

newiris <- iris
newiris$Species <- NULL


### 1st plot: Scatterplots for two runs of kmeans with different outcomes, 
### including visualization of random starting centroids.
# The first outcome of kmeans with k=3
set.seed(1)
start <- newiris[sample(1:nrow(newiris), 3), ]
kc <- kmeans(newiris,start, nstart = 1)
kc$size
table(iris$Species, kc$cluster)
p1 <- plot(newiris[c("Sepal.Length", "Sepal.Width")], col=kc$cluster, pch=20, cex = 1.3)
points(kc$centers[,c("Sepal.Length", "Sepal.Width")], col=1:3, pch=20, cex=5)
points(start[,c("Sepal.Length", "Sepal.Width")], col=4, pch=20, cex=5)
title(main = "kmeans random starting centroids and cluster centroids, 1st run")
# The second outcome of kmeans with k=3
set.seed(3)
start <- newiris[sample(1:nrow(newiris), 3), ]
kc <- kmeans(newiris,start, nstart = 1)
kc$size
table(iris$Species, kc$cluster)
p2 <- plot(newiris[c("Sepal.Length", "Sepal.Width")], col=kc$cluster, pch=20, cex = 1.3)
points(kc$centers[,c("Sepal.Length", "Sepal.Width")], col=1:3, pch=20, cex=5)
points(start[,c("Sepal.Length", "Sepal.Width")], col=4, pch=20, cex=5)
title(main = "kmeans random starting centroids and cluster centroids, 3rd run")


### 2nd plot: Hexbin plots of inital and final centroids 
### for 1000 runs of kmeans
runs=1000
initCentroids <- list()
finalCentroids <- list()
# Creating list of initial and final centroids for 1000 runs
for ( i in 1:runs ) {
  start <- newiris[sample(1:nrow(newiris), 3), ]
  kc <- kmeans(newiris,start, nstart = 1)
  kc$size
  table(iris$Species, kc$cluster)
  plot(newiris[c("Sepal.Length", "Sepal.Width")], cex = 0)
  initCentroids[[i]] <- start
  finalCentroids[[i]] <- kc$centers
}
# Data preparation for visualizations
initdf = do.call(rbind,initCentroids) 
finaldf = do.call(rbind,finalCentroids)
colnames(initdf) <- c("Sepal.Length","Sepal.Width", "Petal.Length", "Petal.Width", "run")
colnames(finaldf) <- c("Sepal.Length","Sepal.Width", "Petal.Length", "Petal.Width", "run")
finaldf2 <- as.data.frame(finaldf)
finaldf2$Sum <- rowSums(finaldf2)
m <- as.data.frame(table(finaldf2$Sum))
finaldf2 <- merge(x = finaldf2, y = m, by.x = "Sum" , by.y = "Var1", all.x=TRUE)    
finaldf2 <- distinct(finaldf2, Sum, .keep_all = TRUE)

# Creating the hexplot
hexplot <- ggplot() + 
  stat_binhex(data = initdf, aes(x=Sepal.Length, y=Sepal.Width), bins = 22, colour="light grey",na.rm=TRUE) + 
  geom_point(data=finaldf2, aes(x=Sepal.Length, y=Sepal.Width, size=Freq), colour = "red") +
  geom_point(aes(x=Sepal.Length, y=Sepal.Width), data=newiris, col = 'black', alpha = .1) +
  scale_fill_gradientn(colours=c("light yellow","orange"),name = "Initial centroids [n]",na.value=NA) +
  geom_count(colour = "red") +
  scale_size_area("Final centroids [n]") +
  ggtitle("Number of final and initial centroids after 1000 runs of kmeans")+
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
hexplot


### 3rd plot: Silhouette index plots for two runs of kmeans with different outcomes, 

# The first outcome of kmeans with k=3
set.seed(1)
start <- newiris[sample(1:nrow(newiris), 3), ]
kc <- kmeans(newiris,start, nstart = 1)
kc$size
sil1 <- silhouette(kc$cluster, dist(newiris))
fviz_silhouette(sil)

# The second outcome of kmeans with k=3
set.seed(3)
start <- newiris[sample(1:nrow(newiris), 3), ]
kc <- kmeans(newiris,start, nstart = 1)
kc$size
sil2 <- silhouette(kc$cluster, dist(newiris))
fviz_silhouette(sil2)

cowplot::plot_grid(
  fviz_silhouette(sil), fviz_silhouette(sil2),
  ncol=1,labels = 'AUTO', align = 'v', axis = 'lr'
)


### 4th plot: Scatterplot of the average silhouette widths 
### from 1000 runs of kmeans
silList <- list()
runs = 1000
# Creating lists of average silhouette widths  
for ( i in 1:runs ) {
  start <- newiris[sample(1:nrow(newiris), 3), ]
  kc <- kmeans(newiris,start, nstart = 1)
  kc$size
  table(iris$Species, kc$cluster)
  sil <- silhouette(kc$cluster, dist(newiris))
  sils <- cbind(sil, seq.int(nrow(sil)))
  silList[[i]] <- sils
}
# Data preparations and calculations
sildf = as.data.frame(do.call(rbind,silList)) 
colnames(sildf)[4] <- "ID"
silPoints <- aggregate(x = sildf$sil_width,              
          by = list(sildf$ID),              
          FUN = mean)
colnames(silPoints)[1] <- "ID"
iris$ID = seq.int(nrow(iris))
irisSil <- merge(x = iris, y = silPoints, by = "ID", all = TRUE)
colnames(irisSil)[7] <- "sil_width"
irisSil
aggregate(x = irisSil$sil_width,              
                       by = list(irisSil$Species),              
                       FUN = mean)

# Creating the scatterplot
ggplot(irisSil,aes(Sepal.Length,Sepal.Width))+geom_point(aes(colour = sil_width), size = 5, alpha = .8)+
  scale_colour_gradient2(low = "red", mid = "yellow", high = "light green", midpoint = 0.5)+
  ggtitle("Average silhouette width by point in the iris dataset after 1000 runs of kmeans")+
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
