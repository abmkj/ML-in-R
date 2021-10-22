setwd( "/Users/Home/Files" )

# Loading and inspecting data ---------------------------------------------

#Working with test data as an example
dat &lt;- read.csv("track_records.csv")
head(dat)

#Basic processing
rownames(dat) &lt;- dat$country
dat &lt;- dat[,-8]
head(dat)


# Performing PCA ----------------------------------------------------------

pca.out &lt;- prcomp(dat, scale = TRUE)
names(pca.out)

#Means and SDs of variables on original scale
pca.out$center
pca.out$scale

#PC Loadings
pca.out$rotation
pca.out$rotation[,1,drop=FALSE] #These are the variable loadings on the first PC
sqrt( sum( pca.out$rotation[,1,drop=FALSE]^2 ) ) #These are all unit vectors

sum(pca.out$rotation[,1]^2)

#PC Score Vectors (columns)
pca.out$x #The first column tells you where each country lies along the first principal component
pca.out$x[1:5,1,drop=FALSE]

var(pca.out$x[,1]) #Variance of score vector for first PC across countries
var(pca.out$x[,2]) #Variance of score vector for second PC across countries


# Biplot and Scree Plot ---------------------------------------------------

#Biplot
biplot(pca.out, col = c(4,2), scale = 0)

#Proportion of variance explained by PCs
pca.var &lt;- pca.out$sdev^2
pve &lt;- pca.var/sum(pca.var)
pve

#Total variance to explain
sum(pca.var)

#Scree plot
plot(pve, xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", 
     ylim = c(0,1), type = 'b', col = "blue")

#Cumulative variance plot
plot(cumsum(pve), xlab = "Principal Component", 
     ylab = "Cumulative Proportion of Variance Explained", 
     ylim = c(0,1), type = 'b', col = "blue")


#Standardizaton important in this case
head(dat)
pca.out.ns &lt;- prcomp(dat, scale = FALSE)
pca.out.ns$rotation
biplot(pca.out.ns, col = c(4,2), scale = 0)

</pre></body></html>Ztext/plainUUTF-8_?https://raw.githubusercontent.com/a-mkj/PCA-ICA-in-R/main/pca.RP