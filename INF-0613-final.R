########################################
# Trabalho Final - INF-0613
# Nome(s): Carlos Fernandes e Liselene de Abreu Borges
########################################
rm(list=ls())
setwd("/Carlos/ComplexData/INF-0613/Trabalho Final")
#setwd("~/Projects/ComplexData/trabalho/INF-0613")

headlines <- read.csv("dataset/headlines.csv")
features <- read.csv("dataset/features.csv")

# Scaled
features.reduced <- prcomp(features, scale.=TRUE)
features.importance<-summary(features.reduced)$importance

#1654 componentes possuem 85% da variancia
a<-features.importance[3,features.importance[3,]>0.85]
pc85<-a[1]
#1804 componentes possuem 90% da variancia
a<-features.importance[3,features.importance[3,]>0.90]
pc90<-a[1]

#define o cojunto de dados reduzido e calcula a matrix de distancias, para 1654 componentes (85%)
dataset<-features.reduced$x[,1:1654]
d<- dist(dataset)

#kmeans: 20 clusters tem melhor betweengss/totss
K <- c(5,10,15,20)
x <- 1
for (k in K) {
  set.seed(1234)
  c <- kmeans(dataset,k, nstart = 20)
  print(k)
  print(c$betweenss/c$totss)
  x <- x + 1
  #s<-silhouette(km$cluster,d)
  #summary(s)$avg.width
}

# Not Scaled
features.reduced.not_scaled <- prcomp(features)
features.importance.not_scaled<-summary(features.reduced.not_scaled)$importance

#1390 componentes possuem 85% da variancia
a<-features.importance.not_scaled[3,features.importance.not_scaled[3,]>0.85]
pc85.not_scaled<-a[1]
#1598 componentes possuem 90% da variancia
a<-features.importance.not_scaled[3,features.importance.not_scaled[3,]>0.90]
pc90.not_scaled<-a[1]

#define o cojunto de dados reduzido e calcula a matrix de distancias, para 1390 componentes (85%)