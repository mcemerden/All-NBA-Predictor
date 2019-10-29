library(dplyr)
library(cluster)
library(plotly)

#Import LWRW csv file containing player data
Players <- read.csv("~/Desktop/LWRW.csv", stringsAsFactors=FALSE)

#Remove rows containing NAs
Players <- data.frame(na.omit(Players))

#Keep only numeric variables
Players_numeric <- Players %>% select_if(is.numeric)

#As a precursor to cluster analysis, normalize the numeric data
Players_numeric_scale <- data.frame(scale(Players_numeric))
rownames(Players_numeric_scale) <- Players$Player


## Principal Component Analysis

pc <- princomp(Players_numeric_scale)

plot(pc) #principal components bar chart
plot(pc,type="lines") #principal components scree plot
summary(pc) #summary principal components analysis
pc$loadings #weights of each principal component

#Only keep the first 5 PCs
comp <- data.frame(pc$scores[,1:5])

#Create matrix of player PC 1, 2, 3, 4 and 5
M1 <- cbind(comp$Comp.1, comp$Comp.2, comp$Comp.3, comp$Comp.4, comp$Comp.5)

#Convert rownames to player names
rownames(M1) <- Players$Player

#Create a distance matrix for each pair of players
M2 <- dist(M1)

#Have colnames be the same set as rownames
colnames(M2) <- rownames(M2)

#Convert into a matrix
M2 <- as.matrix(M2)

#Convert into a data frame
M2 <- data.frame(M2)

#Switch 0s to 1000 as we don't want a player to be compared to himself.
M2[M2 == 0] <- 1000

#Change .. from player names to a single . 
colnames(M2) <- gsub("\\..", ".", colnames(M2))
rownames(M2) <- gsub("\\..", ".", rownames(M2))

#Use this information to identify most similar pair. Used Lionel Messi for this example
rownames(M2[M2$L.Messi == min(M2$L.Messi),])

#Store #20 most similar players to Lionel Messi. 
#Can be used for any player by changing the name
MessiComp <- M2[order(M2$L.Messi),] %>% select(L.Messi) %>% head(20)


## K-Means Clustering

#Determine k using within cluster sum of squares 
wss <- (nrow(Players_numeric_scale)-1)*sum(apply(Players_numeric_scale,2,var))

for (i in 2:15) wss[i] <- sum(kmeans(Players_numeric_scale,
                                     centers=i)$withinss)

#Plot of WSS
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within cluster sum of squares")

#Store cluster analysis as an object called Clusters.
Clusters <- kmeans(Players_numeric_scale, 5, nstart = 25, iter.max = 1000)


#Store the cluster each player belongs to as well as the values for PC1 and PC2
Players_numeric_scale <- Players_numeric_scale %>% mutate(Cluster = Clusters$cluster)
Players_numeric_scale$Comp1 <- comp$Comp.1
Players_numeric_scale$Comp2 <- comp$Comp.2


#Setup cluster plot for players with PC1 on the X axis and PC2 on the Y axis

p1 <- plot_ly(data = Players_numeric_scale, x = ~Comp1, y = ~Comp2, color = ~as.factor(Cluster)) %>% 
  add_trace(
    type = 'scatter',
    mode = 'markers',
    text = rownames(Players_numeric_scale), # when you hover on a point it will show it's rowname
    hoverinfo = 'text',
    showlegend = F
  )

#Display p1
p1

#Display the average of each variable by cluster
Stat_summary <- Players_numeric_scale %>% group_by(Cluster) %>%
  summarise_all(list(mean))


