library(dplyr)
library(tidyverse)
library(lattice)
library(cluster)
library(ggplot2)

d = read.csv("medications.csv")
d1 = read.csv("questionnaire.csv")
d1 <- select(d1, SEQN, HIQ270) #Choosing columns

d$DAYSCOUNT <- d$RXDDAYS * d$RXDCOUNT #An aggregated variable measuring how much in total they take for a given drug

d[is.na(d)] <- 0
d2 <- filter(d, RXDDAYS!=99999) #Removed those who did not know how long they had been using the medication

plot(d2$RXDDAYS)

d3 <- merge(d1, d2, by.x = "SEQN", by.y = "SEQN")

d4 <- unique(transform(d3, DAYSCOUNT=ave(DAYSCOUNT, SEQN, FUN=sum)))
d5 <- d4[!duplicated(d4$SEQN),] # summed DAYSCOUNT and now IDs are unique

tapply(d5$DAYSCOUNT, d5$HIQ270, summary)

boxplot(DAYSCOUNT~HIQ270,data=d5)

boxplot(RXDDAYS~HIQ270,data=d5)
boxplot(RXDCOUNT~HIQ270,data=d5)

########################################################################################################

data1 <- read.csv("demographic.csv")
data2 <- read.csv("questionnaire.csv")
data3 <- merge(data1, data2, by.x = "SEQN", by.y = "SEQN")
data4 <- data3[,c("RIAGENDR", "RIDAGEYR", "OCD150", "DMDEDUC2", "DMDMARTL", "DMDCITZN", "INDHHIN2")]

plot(data4)

#Specify a MAXIMUM number of clusters for kmeans
MAX_K = 15
wss = numeric(MAX_K)

for(k in 1: MAX_K){
  wss[k] = kmeans(na.omit(data4), k)$tot.withinss
}


#view the WSS plot
plot(1:MAX_K, wss, pch = 20, type = "o",xlab="Number of Clusters (K)",ylab="Within Sum of Squares (WSS)", main = "Total WSS against K")
# optimum number of clusters is 3

#We shall now perform a silhouette Analysis to find the optimum K. Note that there is no silhouette width for K=1 as there are no
#neighbouring clusters
avgwidths = numeric(MAX_K)

for(k in 2: MAX_K){
  pam = pam(data4,k)
  avgwidth = pam$silinfo$avg.width
  avgwidths[k] = avgwidth
}

#View the average silhouette width plot:
plot(1:MAX_K, avgwidths, pch = 20, type = "o", main = "Average Silhouette Width against K")

pam = pam(data4,3)
silplot = silhouette(pam)
plot(silplot)
pam$silinfo$avg.width

# We see that K= 3 seems to be a reasonable choice, thus we shall now summarise the data with the new clusters.
kmeans_data_3 = kmeans(na.omit(data4), 3)

kmeans_data_3

#We add a new column to the data which is the new cluster it belongs to.
data_new_3 =   na.omit(data4) %>% mutate(cluster = factor(kmeans_data_3$cluster))
data_new_3

#Test if the clusters are significant. 
# The clusterEval function checks if all clusters are statistically different from one another at a certain significance level,
# using anova. This checks if the clusters are truely statistically different for each category
# 
clusterEval = function(data_new_3, sig){
  eval = list();
  overall = TRUE
  for(i in 1:5){
    eval[[colnames(data_new_3)[i]]] = anova(lm(data = data_new_3, data_new_3[,i] ~ cluster))
    pval = eval[[colnames(data_new_3)[i]]]$'Pr(>F)'[1]
    cat(colnames(data_new_3)[i],": \n")
    cat("P-value: ",pval,"\n")
    cat("Significant at ", sig, ": ", pval < sig, "\n")
    cat("\n")
    
    if(pval > sig){
      overall = FALSE
    }
  }
  cat("Overall Significance: ", overall)
}
clusterEval(data_new_3, 0.01) #check at 1% level of significance

#Seems like gender is difference isn't significant. I will remove this variable and cluster again.

###########################################################################
###2nd TRY
data5 <- data4[,c("RIDAGEYR", "OCD150", "DMDEDUC2", "DMDMARTL", "DMDCITZN", "INDHHIN2")]

plot(data5)

#Specify a MAXIMUM number of clusters for kmeans
MAX_K = 15
wss = numeric(MAX_K)

for(k in 1: MAX_K){
  wss[k] = kmeans(na.omit(data5), k)$tot.withinss
}

#view the WSS plot
plot(1:MAX_K, wss, pch = 20, type = "o",xlab="Number of Clusters (K)",ylab="Within Sum of Squares (WSS)", main = "Total WSS against K")
# optimum number of clusters is 3

#We shall now perform a silhouette Analysis to find the optimum K. Note that there is no silhouette width for K=1 as there are no
#neighbouring clusters
avgwidths = numeric(MAX_K)

for(k in 2: MAX_K){
  pam = pam(data5,k)
  avgwidth = pam$silinfo$avg.width
  avgwidths[k] = avgwidth
}

#View the average silhouette width plot:
plot(1:MAX_K, avgwidths, pch = 20, type = "o", main = "Average Silhouette Width against K")

pam = pam(data5,3)
silplot = silhouette(pam)
plot(silplot)
pam$silinfo$avg.width

# We see that K= 3 seems to be a reasonable choice, thus we shall now summarise the data with the new clusters.
kmeans_data5_3 = kmeans(na.omit(data5), 3)

kmeans_data5_3

#We add a new column to the data which is the new cluster it belongs to.
data5_new_3 =   na.omit(data5) %>% mutate(cluster = factor(kmeans_data5_3$cluster))
data5_new_3

#Test if the clusters are significant. 
# The clusterEval function checks if all clusters are statistically different from one another at a certain significance level,
# using anova. This checks if the clusters are truely statistically different for each category
# 
clusterEval = function(data5_new_3, sig){
  eval = list();
  overall = TRUE
  for(i in 1:5){
    eval[[colnames(data5_new_3)[i]]] = anova(lm(data = data5_new_3, data5_new_3[,i] ~ cluster))
    pval = eval[[colnames(data5_new_3)[i]]]$'Pr(>F)'[1]
    cat(colnames(data5_new_3)[i],": \n")
    cat("P-value: ",pval,"\n")
    cat("Significant at ", sig, ": ", pval < sig, "\n")
    cat("\n")
    
    if(pval > sig){
      overall = FALSE
    }
  }
  cat("Overall Significance: ", overall)
}
clusterEval(data5_new_3, 0.01) #check at 1% level of significance

#Significant clusters. Yay!

####################################################################################
## Now seeing the clusters characteristics
tapply(data5_new_3$RIDAGEYR, data5_new_3$cluster, summary)
boxplot(RIDAGEYR~cluster,data=data5_new_3)
ggplot(data5_new_3, aes(x = OCD150)) + geom_histogram(aes(color = cluster), fill = "white")
ggplot(data5_new_3, aes(x = DMDEDUC2)) + geom_histogram(aes(color = cluster), fill = "white") 
CrossTable(data5_new_3$DMDMARTL, data5_new_3$cluster)
CrossTable(data5_new_3$DMDCITZN, data5_new_3$cluster)
ggplot(data5_new_3, aes(x = INDHHIN2)) + geom_histogram(aes(color = cluster), fill = "white") 

#Cluster 1: Elderly (60s), working less, lower education, higher prop. of US citizens, slightly lower income
#Cluster 2: Middle-aged (50s), working slightly
#Cluster 3: Adults (30s), working more, higher education, lower married rate, more foreigners
