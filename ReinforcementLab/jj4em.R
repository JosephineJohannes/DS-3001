---
title: "Reinforcement-Lab"
author: "Josephine Johannes"
date: "11/3/2021"
output: html_document
---

# due Friday 
library(tidyverse)
library(NbClust)
# read in data and create dataframe (df1)
df <- read_csv("data-summary.csv")
df1 <- select(df,main_colors,opp_colors,on_play,num_turns,won)

# sparse matrix: matrix filled with all 0s 
# feature engineering (cora,corc)
df2 <- select(df,"deck_Adeline, Resplendent Cathar":"deck_Wrenn and Seven")
mat = data.matrix(df2)
vec1 <- vector()
vec3 <- vector()
for(i in 1:nrow(mat) ){
  x<-cor( mat[1,] , mat[i,]) # made some correlations
  vec1 <- c(vec1,x)
  z<-cor( mat[47,] , mat[i,])
  vec3 <- c(vec3,z)
}

# add new features to dataframe
df1 <- df1 %>% mutate(cora = vec1)
df1 <- df1 %>% mutate(corc = vec3)

# make scatter plot comparing new features
ggplot(df1,aes(x=cora,y=corc))+geom_point(aes(colour = main_colors))


## The Actual Lab
# something that I learned in the lab was the difference in using k-means versus knn other than 
# knowing that one is a supervised and unsupervised ML algorithm
actual_df <- read_csv("data-frame.csv")
str(actual_df)
ggplot(actual_df, aes(x=cora, y=corc))+geom_point(aes(colour = num_turns))


# first create the cluster data 
set.seed(1000)
actual_df$num_turns <- scale(actual_df$num_turns, center = TRUE, scale = FALSE)
actual_df <- actual_df[complete.cases(actual_df),]

# creating the factors for clustering
clust_data <- actual_df[ ,c("num_turns","cora","corc")]

# finding the number of clusters 
(nbclust_obj = NbClust(data = clust_data, method= "kmeans"))

# subset the first row from Best.nc and convert to a data frame 
freq_k = nbclust_obj$Best.nc[1,]
freq_k = data.frame(freq_k)

#Plot the recommended number of clusters as a histogram 
ggplot(freq_k, aes(x = freq_k)) + geom_bar()+ scale_x_continuous(breaks = seq(0, 15, by = 1)) + scale_y_continuous(breaks = seq(0, 12, by = 1)) + labs(x = "Number of Clusters", y = "Number ", title = "Cluster Analysis")

kmeans_obj = kmeans(clust_data, centers = 2, algorithm = "Lloyd")
head(kmeans_obj)

clusters = as.factor(kmeans_obj$cluster)
ggplot(actual_df, aes(x = num_turns, 
                            y = cora,
                            shape = clusters)) + 
  geom_point(size = 6) +
  ggtitle("Number of turns vs. Cora") +
  xlab("Number Turns") +
  ylab("Cora") +
  scale_shape_manual(name = "Cluster", 
                     labels = c("Cluster 1", "Cluster 2"),
                     values = c("1", "2")) + 
  scale_color_manual(name="won", labels=c("TRUE", "FALSE"), values = c("red", "blue")) +
  theme_light()
# a description in the comment of the purpose of the code 
#a figure you generated  
#Something you looked up after class that you were curious about 
# After class, I wondered if there was another method of clustering like K-means and found that K- Medians existed 
# Instead of using the mean, you use the median to compute the group centroids