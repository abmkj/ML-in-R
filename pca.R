rm(list=ls())
setwd( "/Desktop/" )

# Loading and inspecting data ---------------------------------------------

dat <- read.csv("track_records.csv")
head(dat)

#Basic processing
rownames(dat) <- dat$country
dat <- dat[,-8]
head(dat)


# Performing PCA ----------------------------------------------------------

pca.out <- prcomp(dat, scale = TRUE)
names(pca.out)

#Means and SDs of variables on original scale
pca.out$center
pca.out$scale

#PC Loadings
pca.out$rotation
pca.out$rotation[,1,drop=FALSE] 
sqrt( sum( pca.out$rotation[,1,drop=FALSE]^2 ) )

sum(pca.out$rotation[,1]^2)

#PC Score Vectors (columns)
pca.out$x 
pca.out$x[1:5,1,drop=FALSE]

var(pca.out$x[,1]) 
var(pca.out$x[,2]) 

#Biplot
biplot(pca.out, col = c(4,2), scale = 0)

#Proportion of variance explained by PCs
pca.var <- pca.out$sdev^2
pve <- pca.var/sum(pca.var)
pve
sum(pca.var)

#Scree plot
plot(pve, xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", 
     ylim = c(0,1), type = 'b', col = "blue")

#Cumulative variance plot
plot(cumsum(pve), xlab = "Principal Component", 
     ylab = "Cumulative Proportion of Variance Explained", 
     ylim = c(0,1), type = 'b', col = "blue")




