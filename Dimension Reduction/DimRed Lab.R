dim(iris)
names(iris)
head(iris, n=4)

fisher1 <- prcomp(iris[, 1:4])
print(fisher1)
print(fisher1$rotation)

#Check that the output from fisher1 agrees with what you find by 
#applying the eigen function to the covariance matrix of the Iris data. 

iris <- iris[,1:4]
cov <- cov(iris)
cov
eigen <- eigen(cov)$vectors
eigen

####Interpretung the output of a PCA
summary(fisher1)
round(fisher1$rotation, 2)
round(fisher1$sdev,2) 

plot(fisher1, main = "Fisher's Iris Data")
plot(fisher1, main = "Fisher's Iris Data", type = "l")

fisher_var_explain <- (fisher1$sdev^2) / (sum(fisher1$sdev^2)) 
plot(fisher_var_explain, type = "b", main = "Fisher's Iris Data", 
     xlab = "No. of components", ylab = "Proportion of variance explained", xaxt = "n") 
axis(1, at = 1:4)


#What is the appropriate number of PCs for summarizing the Iris data?

#four PCs is enough cuz these PCs could account for total variables by sum up the prop to 1.   

#What is an appropriate description for each of these PCs?
???

###############################################################################
###Projecting the data onto the principal components
###############################################################################
newiris <- predict(fisher1)
head(newiris, n = 5)

str(newiris)
str(iris)

plot(newiris)

newiris
#Find the PC values for observation 10 using either predict or newdata.
newiris[10,]
obs_10 <- predict(fisher1)[10,]
obs_10
#Check this result by calculating (in R!) PC values directly using the fisher1$rotation loadings matrix. 
#Note that prcomp centers the data by default before performing the PCA. 
#This means that you will have to subtract the mean of the data from the observation 
#for each variable before multiplying it by the PC coefficients in order to get the same result.

iris_mean <-apply(iris,2,mean)
iris_mean
minMean <-sweep(iris,2,iris_mean,"-") 
minMean
iris

fisher1$rotation
sum_fisher1 <- summary(fisher1)
prop <- sum_fisher1$importance[2,]
prop

t <- t(fisher1$rotation)
t
newiris
new_PC1 <- minMean[1:round(prop[1]*150,0),] * t[1,]
new_PC1
new_PC2 <- minMean[round(prop[2]*150,0),] * t[2,]
new_PC3 <- minMean[round(prop[3]*150,0),] * t[3,]
new_PC4 <- minMean[round(prop[4]*150,0),] * t[4,]

new_PCs <-rbind(new_PC1,new_PC2,new_PC3,new_PC4)
new_PCs

#??? how to appy this term?
remove(iris)
iris
########
#Visulization
#########
plot(iris[, 1], iris[, 2], col=iris[, 5])
legend(6.5, 4.5, legend = levels(iris[, 5]), col = c(1, 2, 3), pch = 1)
pairs(iris[,1:4],col=iris[,5])
pairs(newiris,col=iris[,5])
plot(newiris[,1], newiris[,2], type="n", xlab="PC1", ylab="PC2")
text(newiris[,1], newiris[,2], labels=substr(iris[,5],1,2), col=as.integer(iris[,5]))

#Re-run the Iris data PCA with scaled variables. Do your previous conclusions still hold?
iris_std <- eigen(cor(iris[, 1:4]))

fisher2 <- prcomp(iris[, 1:4], scale=TRUE)
newiris_std <- predict(fisher2)
newiris_std

pairs(newiris_std,col=iris[,5])

#Comparison
diag(cov(iris[,1:4]))
eigen(cov(iris[,1:4]))

######develop Olive
#Investigate the olive data.

#Perform a PCA on both the original Olive Oil data and the standardized Olive Oil data. 
#What are the original variances of the variables?
#Do you notice any important differences between the analyses? 
#What are these? Which analysis is the more reliable?

olive <- read.csv("olive.csv")
oli <- olive[,3:10]
fisher3 <- prcomp(oli)
summary(fisher3)
plot(fisher3)
newolive <- predict(fisher3)

newolive
label <- as.numeric(as.factor(olive[,1]))
pairs(newolive,col=label)

plot(newolive[,1], newolive[,2], xlab="PC1", ylab="PC2", col=label)

