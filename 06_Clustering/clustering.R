#============================================================
##### CLUSTERING: FINDING PATTERNS IN YOUR DATA ####
#============================================================

#### Slide 21: Step 1: load packages and data ####

# Load libraries.
#library(e1071)
library(tidyverse)
library(plotly)
library(htmltools)
library(devtools)
library(caret)
library(NbClust)



#==================================================================================

#### Slide 22: Step 1: load packages and data ####

house_votes_Dem = read_csv("data/house_votes_Dem.csv")

# What does the data look like?
View(house_votes_Dem)
str(house_votes_Dem)
table(house_votes_Dem$party.labels)

house_votes_Rep = read_csv("data/house_votes_Rep.csv")

table(house_votes_Rep$party.labels)
View(house_votes_Rep)
#==================================================================================

#### Slide 23: Step 2: run k-means ####

# Define the columns to be clustered by sub-setting the data.
# Placing the vector of columns after the comma inside the 
# brackets tells R that you are selecting columns.
clust_data_Dem = house_votes_Dem[, c("aye", "nay", "other")]
View(clust_data_Dem)
# have three continuous variables and segment them off for use 
# passing in a concatenated list with column names as indicators 
# use the names for more intuitive 

# Run an algorithm with 2 centers.
# kmeans uses a different starting data point each time it runs.
# Make the results reproducible with the set.seed() function.
set.seed(1) #randomly chooses a place to start (but set it to compare apples to apples)
kmeans_obj_Dem = kmeans(clust_data_Dem, centers = 2, 
                        algorithm = "Lloyd")   #<- there are several ways of implementing k-means, see the help menu for a full list
# not normalizing the data 
# counting total number of votes and don't need to normalize because they're on the same range 
# already on the same scale and will be better to scale if we don't normalize
# Lloyd means Euclidean distance and we want two centers because of 2 parties and pass in cluaster data 
# want to maximize the ratio between between_ss/total_ss
# total sum square is the variance within and is accounted for by the distance between 
#between the centroids and within is the average distance between a point and the centroid 
# average difference of 

# What did the kmeans function produce, 
# what does the new variable kmeans_obj contain?
kmeans_obj_Dem
# is that the right number of centers? 
# can do the elbow method or nbclust (runs different algorithmic approaches to model) 

# View the results of each output of the kmeans function.
head(kmeans_obj_Dem)


#==================================================================================

#### Slide 28: Step 3: visualize plot ####

# Tell R to read the cluster labels as factors so that ggplot2 
# (the graphing package) can read them as category labels instead of 
# continuous variables (numeric variables).
party_clusters_Dem = as.factor(kmeans_obj_Dem$cluster)
#clustering object // clustering algorithm 
#object oriented programming land and all the different components associed with K-means
# we care the most about clusters 
# just a list of how we classify each row in the dataset and turning tnto a factor 

# What does the kmeans_obj look like?
View(party_clusters_Dem)
# why aren't they just clustered 2s? Keep in mind, that we created a 3 dimensional space 

#==================================================================================

#### Slide 29: Step 3: visualize plot ####

# aesthetic value: pass in the variables that you want to use 
# the shape is the clusters and passed in independently 
# everything else after is just how we want it to appear 
View(house_votes_Dem)
View(party_clusters_Dem)

ggplot(house_votes_Dem, aes(x = aye, 
                            y = nay,
                            shape = party_clusters_Dem)) + 
  geom_point(size = 6) +
  ggtitle("Aye vs. Nay votes for Democrat-introduced bills") +
  xlab("Number of Aye Votes") +
  ylab("Number of Nay Votes") +
  scale_shape_manual(name = "Cluster", 
                     labels = c("Cluster 1", "Cluster 2"),
                     values = c("1", "2")) +
  theme_light()
# scale_shape_manual: changing the representation of the label
#==================================================================================

#### Slide 32: Step 5: validate results ####

# don't understand if there are outliers and we are just writing a new variable and add it at the top of
# ggplot 
# represent 4 variables on one scale 
# we say waht we want the colors to be and how to level them in the legend 
# seems like voting patterns seem to be more 2 dimensional and aligned with republicans 
# understand the models and test how good they are 
# generate clusters and show how the clusters who up with 2 varying variable graphs.
ggplot(house_votes_Dem, aes(x = aye, 
                            y = nay,
                            color = party.labels,  #<- tell R how to color 
                            #   the data points
                            shape = party_clusters_Dem)) + 
  geom_point(size = 6) +
  ggtitle("Aye vs. Nay votes for Democrat-introduced bills") +
  xlab("Number of Aye Votes") +
  ylab("Number of Nay Votes") +
  scale_shape_manual(name = "Cluster", 
                     labels = c("Cluster 1", "Cluster 2"),
                     values = c("1", "2")) +
  scale_color_manual(name = "Party",         #<- tell R which colors to use and
                     #   which labels to include in the legend
                     labels = c("Republican", "Democratic"),
                     values = c("red", "blue")) +
  theme_light()


# Save your graph. For Windows, use setwd("C:/file path")

ggsave("US House Votes for Dem Bills.png", 
       width = 10, 
       height = 5.62, 
       units = "in")

#optioinal for the lab 
#innerjoin data set with original data set (interjoin: democrats are blue and repulcans are red) 
#==================================================================================

#### Slide 35: Clustering vs. visualizing ####

# We can visualize votes in 3D with the following code.
# View our data.
View(house_votes_Dem)

# Assign colors by party in a new data frame.
party_color3D_Dem = data.frame(party.labels = c("Democrat", "Republican"),
                               color = c("blue", "red"))

View(party_color3D_Dem)


# Join the new data frame to our house_votes_Dem data set.
house_votes_color_Dem = inner_join(house_votes_Dem, party_color3D_Dem)
# inner join: finds the things that are repeats 

house_votes_color_Dem$clusters <- (party_clusters_Dem)
# add clusters to it 

str(house_votes_color_Dem)

house_votes_color_Dem$Last.Name <- gsub("[^[:alnum:]]", "", house_votes_color_Dem$Last.Name)
# regular expression: way to accomodate for various 
#alnum = alphanumeric and only in this string 
# want to get rid of the characters and clean it up
# Use plotly to do a 3d imaging 

fig <- plot_ly(house_votes_color_Dem, 
               type = "scatter3d",
               mode="markers",
               symbol = ~clusters,
               x = ~aye, 
               y = ~nay, 
               z = ~other,
               color = ~color,
               colors = c('#0C4B8E','#BF382A'), 
               text = ~paste('Representative:',Last.Name,
                             "Party:",party.labels))
# x, y, z used to graph and cluster the points 
# markers: triangles 
# colors = hexadecimal code 
# text: hover on point which shows the reps name and party 
# use ~ for plot_ly function specifically 

fig
dev.off()
# bounding box which sets the boundaries to default 
#==================================================================================

#### Slide 41: How good is the clustering? ####
# pointing out individual variance measures 

# Inter-cluster variance,
# "betweenss" is the sum of the distances between points 
# from different clusters.
num_Dem = kmeans_obj_Dem$betweenss
#telling us that the spread is mainly against 2 axis of yay and nay 
# not so much votes on "other" 

# Total variance, "totss" is the sum of the distances
# between all the points in the data set.
denom_Dem = kmeans_obj_Dem$totss

# Variance accounted for by clusters.
(var_exp_Dem = num_Dem / denom_Dem)


#==================================================================================

#### Slide 43: Elbow method: measure variance ####

# Run an algorithm with 3 centers.
set.seed(1) 
kmeans_obj_Dem = kmeans(clust_data_Dem, centers = 3, algorithm = "Lloyd")

# Inter-cluster variance.
num_Dem3 = kmeans_obj_Dem$betweenss

# Total variance.
denom_Dem3 = kmeans_obj_Dem$totss

# Variance accounted for by clusters.
(var_exp_Dem3 = num_Dem3 / denom_Dem3)
# the variance increased and might make more sense that 3 centers would fit this more 
=======

<<<<<<< HEAD
#Might be a helpful look to compare to just a normal variance calculation:
# s2=∑ni=1(xi− x¯)2/(n−1) = variance equation for var()

total.var <- var(clust_data_Dem$aye)+var(clust_data_Dem$nay)+var(clust_data_Dem$other)

total.var.km <- (kmeans_obj_Dem$betweenss+kmeans_obj_Dem$tot.withinss)/(427-1)

# Numbers are the same. 
total.var
total.var.km


#==================================================================================

#### Slide 45: Automating a step we want to repeat ####

# The function explained_variance wraps our code for calculating 
# the variance explained by clustering.
explained_variance = function(data_in, k){
  
  # Running the kmeans algorithm.
  set.seed(1)
  kmeans_obj = kmeans(data_in, centers = k, algorithm = "Lloyd", iter.max = 30)
  
  # Variance accounted for by clusters:
  # var_exp = intercluster variance / total variance
  var_exp = kmeans_obj$betweenss / kmeans_obj$totss
  var_exp  
}

#==================================================================================

#### Slide 46: automating a step we want to repeat ####

# Recall the variable we are using for the data that we're clustering.
View(clust_data_Dem)

# The sapply() function plugs in several values into our explained_variance function.
#sapply() takes a vector, lapply() takes a dataframe
explained_var_Dem = sapply(1:10, explained_variance, data_in = clust_data_Dem)

View(explained_var_Dem)


# Data for ggplot2.
elbow_data_Dem = data.frame(k = 1:10, explained_var_Dem)
View(elbow_data_Dem)
## tradeoff between complexity and accuracy 
# how complex do I want to make this model in terms of diminishing returns 
# what is the cutoff? you have to use your intuition 
# good thing to do unsupervised learning up front and use intuition 
# going beyond 3 in terms of relations
# how complex do I want to make this? 
# nominal spike -- jumps are fairly small past 3 // basically overfitting 
# trying to identify outliers but it's not really much better 
#==================================================================================

#### Slide 47: Elbow method: plotting the graph ####

# Plotting data.
ggplot(elbow_data_Dem, 
       aes(x = k,  
           y = explained_var_Dem)) + 
  geom_point(size = 4) +           #<- sets the size of the data points
  geom_line(size = 1) +            #<- sets the thickness of the line
  xlab('k') + 
  ylab('Inter-cluster Variance / Total Variance') + 
  theme_light()

#==================================================================================

#### Slide 50: NbClust: k by majority vote ####

# Install packages.
#install.packages("NbClust") if needed
library(NbClust)

# Run NbClust.
(nbclust_obj_Dem = NbClust(data = clust_data_Dem, method = "kmeans"))

# View the output of NbClust.
nbclust_obj_Dem

# View the output that shows the number of clusters each method recommends.
View(nbclust_obj_Dem$Best.nc)

#==================================================================================

#### Slide 54: NbClust: k by majority vote ####

# Subset the 1st row from Best.nc and convert it 
# to a data frame so ggplot2 can plot it.
freq_k_Dem = nbclust_obj_Dem$Best.nc[1,]
freq_k_Dem = data.frame(freq_k_Dem)
View(freq_k_Dem)

# Check the maximum number of clusters suggested.
max(freq_k_Dem)

#essentially resets the plot viewer back to default
dev.off()

# Plot as a histogram.
ggplot(freq_k_Dem,
       aes(x = freq_k_Dem)) +
  geom_bar() +
  scale_x_continuous(breaks = seq(0, 15, by = 1)) +
  scale_y_continuous(breaks = seq(0, 12, by = 1)) +
  labs(x = "Number of Clusters",
       y = "Number of Votes",
       title = "Cluster Analysis")

#===============================================================================

# Now we are going to build a simple decision tree using the clusters as a feature
# can be used in PCA 
# added clusters as a feature and add party affiliation 
# see if the clusters are preditctive of party affiliation 
# reminder this is our model, using 3 clusters 
set.seed(1)
kmeans_obj_Dem = kmeans(clust_data_Dem, centers = 3, algorithm = "Lloyd")

# this is the output of the model. 
kmeans_obj_Dem$cluster

house_votes_Dem$clusters <- kmeans_obj_Dem$cluster
View(house_votes_Dem)

# Do some data preparation
# drop the name variable, won't be helpful
tree_data <- house_votes_Dem[,-1]
str(tree_data)
# change 1 and 5 to factors
tree_data[,c(1,5)] <- lapply(tree_data[,c(1,5)], as.factor)
# do we need to normalize? 


# Split 
train_index <- createDataPartition(tree_data$party.labels,
                                           p = .7,
                                           list = FALSE,
                                           times = 1)
train <- tree_data[train_index,]
tune_and_test <- tree_data[-train_index, ]

#The we need to use the function again to create the tuning set 

tune_and_test_index <- createDataPartition(tune_and_test$party.labels,
                                           p = .5,
                                           list = FALSE,
                                           times = 1)

tune <- tune_and_test[tune_and_test_index, ]
test <- tune_and_test[-tune_and_test_index, ]

dim(tune)
dim(test)

# Create our features and target for training of the model. 
# create featureset 
features <- as.data.frame(train[,-1])
target <- train$party.labels # add party label to target 


set.seed(1980)
party_dt <- train(x=features,
                    y=target,
                    method="rpart") # rule based decision tree (less complex than c5.0)
# simple approach to building a decision tree 
# This is more or less a easy target but the clusters are very predictive. 
party_dt
varImp(party_dt) # variable importance// very predictive of party outcomes 

# Let's predict and see how we did. 
dt_predict_1 = predict(party_dt,tune,type= "raw")

confusionMatrix(as.factor(dt_predict_1), 
                as.factor(tune$party.labels), 
                dnn=c("Prediction", "Actual"), 
                mode = "sens_spec")

#======================================================================== 
# let's experiment and see how we would do without the clusters. 

tree_data_nc <- tree_data[,-5]
str(tree_data_nc)
# understanding balance between performance and complexity 

train_index <- createDataPartition(tree_data$party.labels,
                                   p = .7,
                                   list = FALSE,
                                   times = 1)
train <- tree_data_nc[train_index,]
tune_and_test <- tree_data_nc[-train_index, ]

#Then we need to use the function again to create the tuning set 

tune_and_test_index <- createDataPartition(tune_and_test$party.labels,
                                           p = .5,
                                           list = FALSE,
                                           times = 1)

tune <- tune_and_test[tune_and_test_index, ]
test <- tune_and_test[-tune_and_test_index, ]

#Generate the features and target once again
features <- as.data.frame(train[,-1])
target <- train$party.labels

set.seed(1980)
party_dt <- train(x=features,
                  y=target,
                  method="rpart")

# This is more or less a easy target but the clusters are very predictive. 
party_dt
varImp(party_dt)

dt_predict_1 = predict(party_dt,tune,type= "raw")

confusionMatrix(as.factor(dt_predict_1), 
                as.factor(tune$party.labels), 
                dnn=c("Prediction", "Actual"), 
                mode = "sens_spec")

dt_predict_t = predict(party_dt,test,type= "raw")

confusionMatrix(as.factor(dt_predict_t), 
                as.factor(test$party.labels), 
                dnn=c("Prediction", "Actual"), 
                mode = "sens_spec")

#didn't really make a huge difference, what could we have done differently? 
#==================================================================================

#Now you try with the Republican Data and a NBA Stat Example 

#==================================================================================

