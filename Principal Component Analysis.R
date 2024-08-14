# script for Principal Component Analysis (PCA)
  # author: Molly Grant 
  # date: 12 August 2024 
  # R version 4.4.1 (2024-06-14 ucrt)

########################
# overview ###
########################
# This R script provides simple code for PCA ###
# The code for PCA uses the tidyverse package ###
# The simple code is then followed with an example using a dataset within R called decathlon2 ###
########################

########################
# set-up ###
########################
packages <- c("dplyr", "ggplot2", "tidyverse", "reshape2")

# Load all packages in the list
lapply(packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
})

########################
# PCA simple code ###
########################

#view first six rows of data
head(data)

#boxplot
boxplot(data, main = "Boxplot of Variables") #check if variables are on the same or different scales (informs decision to scale or not)

#pairwise relationships
pairs(data, main = "Pairwise Relationships") #Visualise if variables have linear relationships which is a requirement for PCA.

#histograms and qqplots
morph <- scale(data) #put on same scale if needed
par(mfrow = c(5,4), mar = c(4,4,3,0), mgp = c(2,1,0))
for(i in 1:10){
  hist(morph[,i], main = names(morph)[i], xlab='')
  qqnorm(morph[,i], main = names(morph)[i], xlab='')
  qqline(morph[,i])
} #note: normality is not an assumption in PCA.

#correlation heatmaps
data.cor<-cor(data, use='complete.obs')
PcorMelt <- reshape2::melt(data.cor) 
names(PcorMelt)<-c("Measurement1", "Measurement2", "Correlation")
ggplot(data=PcorMelt, aes(x=Measurement1, y=Measurement2, fill=Correlation)) + geom_tile() + ggtitle("Correlation Heatmap")
corrplot(data.cor, method = "number", type = "upper") 

#### calculate principal components ####
results <- prcomp(data, scale = TRUE) #if variables are on different scales, set scale = TRUE to standardise.
results

results$rotation <- -1*results$rotation #eigenvectors in R point in the negative direction by default, so we’ll multiply by -1 to reverse the signs.

#display principal components 
Loadings <- results$rotation #note: this is not rotating the variables. can rotate if want to make loadings more interpretable.
Loadings #note these are unrotated

#reverse the signs of the PC scores
results$x <- -1*results$x

#display the first six scores
head(results$x)

biplot(results, scale = 0)

# Eigenvalues
std_devs <- results$sdev
eigenvalues <- std_devs^2
eigenvalues

#calculate total variance explained by each principal component
var_explained <- results$sdev^2 / sum(results$sdev^2)
var_explained

#create scree plot
qplot(c(1:10), var_explained) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

screeplot(results, main='Screeplot', xlab='component') #different scree plot

summary(results)

#Methods for choosing the number of PCs:
#- Scree Plot: This is useful for determining the number of principal components to retain by plotting the eigenvalues (use before the "elbow")
#- Cumulative Variance: Check the cumulative explained variance to decide how many principal components to retain for analysis (aim for the number that captures desired variance; e.g., 60% or 80%)
#- Eigenvalues: if scaled, >1 eigenvalues ("better" than one of the original variables

# Extract loadings for the top X PCs
Loadings_X <- results$rotation[, 1:X]
print("Loadings for the top X PCs:")
print(Loadings_X)

# Reverse the signs of the PC scores (optional, if needed for consistency)
results$x <- -1 * results$x

# Extract scores for the top 4 PCs
PC_scores <- results$x[, 1:X]
print("Scores for the top X PCs (first 6 rows):")
print(head(PC_scores))

########################
# PCA example ###
########################
#For this example we’ll use the decathlon dataset built into R (note: data not designed specifically for PCA)

#load data
data(decathlon2)
decathlon2 <- decathlon2[,1:10]
#view first six rows of data
head(decathlon2)

#boxplot
boxplot(decathlon2[,1:10], main = "Boxplot of Decathlon Variables") #all on different scales

#pairwise relationships
pairs(decathlon2[,1:10], main = "Pairwise Relationships") #These variables do not have linear relationships which is a requirement for PCA, but we will proceed as this is test data. Can also check the residual vs. fitted plots if want.

#histograms and qqplots
morph <- scale(decathlon2[,1:10])
par(mfrow = c(5,4), mar = c(4,4,3,0), mgp = c(2,1,0))
for(i in 1:10){
  hist(morph[,i], main = names(morph)[i], xlab='')
  qqnorm(morph[,i], main = names(morph)[i], xlab='')
  qqline(morph[,i])
} #normality is not an assumption in PCA but good to visualise.

#correlation heatmaps
decathlon.cor<-cor(decathlon2[,1:10], use='complete.obs')
PcorMelt <- reshape2::melt(decathlon.cor) 
names(PcorMelt)<-c("Measurement1", "Measurement2", "Correlation")
ggplot(data=PcorMelt, aes(x=Measurement1, y=Measurement2, fill=Correlation)) + geom_tile() + ggtitle("Correlation Heatmap")
corrplot(decathlon.cor, method = "number", type = "upper") 

#### calculate principal components ####
results <- prcomp(decathlon2[,1:10], scale = TRUE) #as the variables are on different scales, we set scale = TRUE to standardise the variables to have mean of 0 and sd of 1 
results

results$rotation <- -1*results$rotation #eigenvectors in R point in the negative direction by default, so we’ll multiply by -1 to reverse the signs.

#display principal components 
Loadings <- results$rotation #have to adde a step if want to rotate so the loadings are more interpretable (some are pushed higher, some lower)
Loadings #note these are unrotated

#reverse the signs of the PC scores
results$x <- -1*results$x

#display the first six scores
head(results$x)

biplot(results, scale = 0)

# Eigenvalues
std_devs <- results$sdev
eigenvalues <- std_devs^2
eigenvalues

#calculate total variance explained by each principal component
var_explained <- results$sdev^2 / sum(results$sdev^2)
var_explained

#create scree plot
qplot(c(1:10), var_explained) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

screeplot(results, main='Screeplot', xlab='component') #different scree plot

summary(results)

#Methods for choosing the number of PCs:
#- Scree Plot: This is useful for determining the number of principal components to retain by plotting the eigenvalues (use before the "elbow")
#- Cumulative Variance: Check the cumulative explained variance to decide how many principal components to retain for analysis (aim for the number that captures desired variance; e.g., 60% or 80%)
#- Eigenvalues: if scaled, >1 eigenvalues ("better" than one of the original variables

# Extract loadings for the top 4 PCs
Loadings_4 <- results$rotation[, 1:4]
print("Loadings for the top 4 PCs:")
print(Loadings_4)

# Reverse the signs of the PC scores (optional, if needed for consistency)
results$x <- -1 * results$x

# Extract scores for the top 4 PCs
PC_scores <- results$x[, 1:4]
print("Scores for the top 4 PCs (first 6 rows):")
print(head(PC_scores))
