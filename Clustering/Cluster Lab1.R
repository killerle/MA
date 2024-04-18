###############################################################################
###S2Lab1
###############################################################################

df <- faithful
head(df)

library(ggplot2)
ggplot(df,aes(eruptions,waiting)) +
       geom_point() +
       labs(     
        xlab= 'Eruptions',
        ylab= 'Waiting',
        title = 'Scatterplot')
#Two groups cluster could be done

df2 <- iris[,1:4] 
df2

plot(df2[,2])
plot(df2[,3])
plot(df2[,4])


###S2Lab1
setwd("~/Desktop/R/MA")
olive <- read.csv("olive.csv")
head(olive, n=4)

acids <- olive[,3:10]
head(acids)

acids_dis <- dist(acids, method="euclidean") 
acids_dis_mat <- as.matrix(acids_dis)
acids_dis_mat[1, 5]
#[1] 72.92462
acids_dis_mat[c(1:4, 331:333), c(1:4, 331:333)]

# Compare the Euclidean dissimilarity between observations 1 and 10.
acids_dis_mat[1, 10]
acids_dis_mat[10, 1]
#[1] 135.4253

# Compare the Manhattan dissimilarity between observations 1 and 10.
acids_Mh <- dist(acids, method="manhattan") 
acids_Mh_mat <- as.matrix(acids_Mh)
acids_Mh_mat[1,10]

#Consider which dissimilarity measure may appropriate to use?

????
  
#Compare the dissimilarity between the first five observations from the Sardinia 
#region, (specifically from Inland Sardinia) with the first five observations 
#from the North region, (specifically, Umbria). 
#Are oils from the same region more similar? 
#Use any dissimilarity measure you like.
Sa_acids <- olive[olive$Area == "Inland Sardinia",3:10] 
Sa_acids
Um_acids <- olive[olive$Area == "Umbria", 3:10]
Um_acids

dis_Sa_acids <- dist(Sa_acids)
dis_Sa_acids_mat <-as.matrix(dis_Sa_acids)
dis_Sa_acids_mat[1:5,1:5]

dis_Um_acids <- dist(Um_acids)
dis_Um_acids_mat <-as.matrix(dis_Um_acids)
dis_Um_acids_mat[1:5,1:5]
plot(dis_Sa_acids_mat[1:5,1:5])
plot(dis_Um_acids_mat[1:5,1:5])

##########################
###Hierarchical clustering
##########################
clust_Sa <- hclust(dis_Sa_acids, method = "average")
plot(clust_Sa)
clust_Um <- hclust(dis_Um_acids, method = "average")
plot(clust_Um)

clust_Eu_avrg <- hclust(acids_dis, method = "average")
plot(clust_Eu_avrg)

#Produce a dendrogram of the hierarchical clustering of the olive oil data 
#using single linkage and a Manhattan measure of dissimilarity. 
#Comment on the difference between this plot and that using average linkage and Euclidean distance. 
#Which factor has more influence on the dendogram, the linkage method or dissimilarity measure?
clust_Hm_sing <- hclust(acids_Mh, method = "single")
plot(clust_Hm_sing)


head(clust_Um$merge)
head(clust_Um$height)
#[,1] [,2]
#[1,] -450 -451
#[2,] -432 -434
#[3,] -437 -441
#[4,] -438 -444

head(clust_Eu_avrg$height)
#[1] 1.000000 3.741657 3.741657 4.898979 5.567764 5.720575

########################
###Choosing the Number of Clusters
########################
h_mu <- mean(clust_Eu_avrg$height)
h_sd <- sd(clust_Eu_avrg$height)

h <- h_mu+3*h_sd
h

abline(h=h, lty=2, col=2)
#got nine of clusters

acids_label1 <- cutree(clust_Eu_avrg, k=10) 
acids_label1
acids_label2 <- cutree(clust_Eu_avrg, h=306.5752)
which(acids_label1 == 1)
which(acids_label2 == 10)

palette(rainbow(10))
plot(acids[,1], acids[,2], col = acids_label2)
pairs(acids, col = acids_label1)

library(flexclust)
pacman::p_unload(all)
pacman::p_load(flexclust)

clust1 <- hclust(acids_dis, method = "average") 
clust2 <- hclust(acids_dis, method = "single")

clust1_label <- cutree(clust1, k=3) 
clust2_label <- cutree(clust2, k=3)

randIndex(clust1_label, clust2_label, correct = F)
#measure how agreement of different clusters

#Compute a 4 class clustering of the Olive Oil data, 
#using Euclidean dissimilarity and average linkage. 
#Compute the Rand index and adjusted Rand index of this compared with the 3 class 
#clustering using complete linkage. 
#Why use the 4 cluster solution?
clust1 <- hclust(acids_dis, method = "average") 
clust1_label <- cutree(clust1, k=4) 
clust1_label
which(clust1_label == 4)

clust2 <- hclust(acids_dis, method = "complete")
clust2_label <- cutree(clust2, k=3)
randIndex(clust1_label,clust2_label,correct = F)
randIndex(clust1_label,clust2_label,correct = T)

###############################################
###Standardization
###############################################

acids = olive[,3:10]
acid_sd <- apply(acids, 2 ,sd) 
acid_sd

standard_acids <- sweep(acids, 2, acid_sd, "/") 
head(standard_acids)

#Use the apply function to find the column means of acids.
acids_mean <- apply(acids,2,mean)
acids_mean

#Use the sweep function to create a centered version of acids whereby each column has mean 0.
centered_minsMean <- sweep(acids,2,acids_mean,"-")
centered_minsMean
centered <- sweep(centered_minsMean,2,acid_sd,"/")
centered
#check
standard_acids_mean <- apply(centered,2,mean)
standard_acids_mean

olive_scale_numeric <- scale(acids, center = TRUE, scale = TRUE) 
olive_scale = cbind.data.frame(olive[,1:2], olive_scale_numeric)

olive_scale_mean <- apply(olive_scale_numeric,2,mean)
olive_scale_mean
#???How scale is works? ps why these two method got different reuslts?

#Perform a hierarchical cluster analysis on the standardized version of acids. 
#Do the results differ from the hierarchical cluster analysis performed on the 
#unstandardized data?

dis_sd_acids <-dist(olive_scale_numeric)
clust_sd <- hclust(dis_sd_acids, method = "average")
plot(clust_sd)

h_sd_mu <- mean(clust_sd$height)
h_sd <- sd(clust_sd$height)
h_ <- h_sd_mu+3*h_sd
h_
abline(h=h_,clty=2,col=2)

sd_label <-cutree(clust_sd, h = h_)
sd_label

library(flexclust)
randIndex(sd_label,acids_label2,correct = F)
randIndex(sd_label,acids_label2,correct = T)


###############################################################################
###S2Lab2
###############################################################################
faithful <- faithful
str(lab2)
head(lab2)

dis_faithful <- dist(faithful)
clust_faithful <-hclust(dis_faithful, method = "average")
plot(clust_faithful)

###############################################################################
###Choosing k
###############################################################################
WSS <- rep(0,10)
WSS[2] <- sum(kmeans(faithful, centers = 2)$withinss)

#Using a for loop, edit the remaining entries of the vector WSS 
#so that they contain the appropriate total within group sum of squares value.
for (i in 1:10) {
  WSS[i] <- sum(kmeans(faithful, centers = i)$withinss)
  i = i + 1
}
WSS

#Plot the total within group sum of squares value as a function of k.
plot(WSS)


lable2 <- cutree(clust_faithful, k=2)
lable2
plot(faithful, col = lable2) 

k <- 2
cl2 <- kmeans(faithful,centers = 2) 
table(cl2$cluster)
cl2$centers
cl2$withinss

#What does the output from cl2$centers tell us about the difference between the two clusters? 
#Or to express the question in another way, what description can be given to each cluster?
diff <- lable2 - cl2$cluster

plot(faithful, col = cl2$cluster) 
points(cl2$centers, col=1:k, pch=8, cex=5)

g1 <- faithful[which(cl2$cluster==1),]
g2 <- faithful[which(cl2$cluster==2),]
plot(g1)
plot(g2)
plot(faithful)

ng1 <- cl2$size[1]
total1 <- sum(as.matrix(dist(rbind(g1, cl2$centers[1,])))[ng1+1,]) 
ave1 <- total1/ng1
ave1


ng2 <- cl2$size[2]
total2 <- sum(as.matrix(dist(rbind(g2, cl2$centers[2,])))[ng2+1,]) 
ave2 <- total2/ng2
ave2

library("cluster")
dist_mat <- dist(faithful)^2
silhouette_cl2 <- silhouette(cl2$cluster, dist_mat) 
summary(silhouette_cl2)
plot(silhouette_cl2)

hcl <- cutree(hclust(dist(faithful)), 2) 
icl <- kmeans(faithful, centers = 2)
tab <- table(hcl, icl$cluster)

#Find the Rand index and adjusted Rand index of these clustering solutions. 
library(flexclust)
#icl2 <- kmeans(faithful, centers = 3)
#randIndex(icl$cluster,icl2$cluster,correct = F)
hcl <- cutree(hclust(dist(faithful)), 2) 
icl <- kmeans(faithful, centers = 2)

randIndex(hcl,icl$cluster,correct = F)
randIndex(hcl,icl$cluster,correct = T)
#• Use the Silhouette index method to compare the clustering solutions. 
#Which cluster solution appears to be best with respect to this metric?

hcl_silhouette <-silhouette(hcl, dist_mat)
summary(hcl_silhouette)
plot(hcl_silhouette)
icl_silhouette <-silhouette(icl$cluster, dist_mat)
summary(icl_silhouette)
plot(icl_silhouette)

#• Perform clustering analysis on the faithful data after standardizing it. 
#Do your results differ?
faithful_scale <- scale(faithful, center = TRUE, scale = TRUE)
#faithful_scale = cbind.data.frame(faithful[,1:2], faithful_scale_numeric)
plot(faithful_scale)
icl_sd <- kmeans(faithful_scale, centers =2)
dis_mat_sd <- dist(faithful_scale)^2
icl_sd_silhouette <- silhouette(icl_sd$cluster,dis_mat_sd)
summary(icl_sd_silhouette)
plot(icl_sd_silhouette)

randIndex(icl$cluster,icl_sd$cluster,correct = F)

#• Perform a k-means clustering analysis on the Olive Oil data.
plot(olive[,6])

#choosing K level
faithful
WSS <- rep(0,10)

for (i in 1:10) {
  WSS[i] <- sum(kmeans(olive[,3:10], centers = i)$withinss)
  i = i + 1
}
WSS

WSS <- rep(0,10)

plot(WSS)

icl_new <- kmeans(olive[,3:10], centers = 2)
icl_new$cluster
dis_mat <- dist(olive[,3:10])^2
icl_new_silhouette <- silhouette(icl_new$cluster,dis_mat)
summary(icl_new_silhouette)
plot(icl_new_silhouette)