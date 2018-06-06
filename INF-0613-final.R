########################################
# Trabalho Final - INF-0613
# Nome(s): Carlos Fernandes e Liselene de Abreu Borges
########################################

rm(list=ls())
#setwd("/Carlos/ComplexData/INF-0613/Trabalho Final")
setwd("~/Projects/ComplexData/trabalho/INF-0613")

library(MASS)
library(cluster)
library(ngram)
library(stringr)

K <- c(5,10,15,20)

calculateSilhouete <- function(clusters_, distance) {
  x <- 1
  silhouetes <- list()
  for (k in K) {
    print(k)
    set.seed(1234)
    silhouetes[[x]] <- silhouette(clusters_[[x]]$cluster, distance)
    #print(silhouetes[[x]])
    x <- x + 1
  }
  return(silhouetes)
}


#kmeans
calculateKmeans <- function(dataset) {
  x <- 1
  means <- list()
  for (i in K) {
    print(i)
    set.seed(1234)
    means[[x]] <- kmeans(dataset,i, nstart = 20)
    #print(means.scaled[[x]]$totss)
    x <- x + 1
    gc()
  }
  return (means)
}

#kmedian
library(flexclust)
calculateKmedian <- function(dataset) {
  x <- 1
  means <- list()
  for (k in K) {
    print(k)
    set.seed(1234)
    means[[x]]=kcca(dataset,k)
    means[[x]]
    x <- x + 1
    gc()
  }
  return (means)
}

#fuzzy
library(e1071)
calculateFuzzy <- function(dataset) {
  x <- 1
  clusters <- list()
  for (k in K) {
    print(k)
    set.seed(1234)
    clusters[[x]]=cmeans(dataset, k, m=2)
    x <- x + 1
  }
  return (clusters)
}

getBigram <- function(dataset){
  bg <- c();
  for (line in 1:K[3]){ #para K=15 é o 3 
    b <- concatenate(headlines[(dataset[[3]]$cluster==line),2])
    ng<-ngram(b,n=2)
    bg<-c(bg,get.phrasetable(ng)[1:3,1]) #pega os 3 bigramas mais frequentes e concatenar para todos os clusters
  }
  return(bg)
}

getBigram2016 <- function(dataset){
  bg <- c();
  for (line in 1:K[1]){ #para K=15
    index2016<-str_detect(headlines$publish_date[(dataset[[1]]$cluster==line)],"2016")
    b <- concatenate(headlines$headline_text[index2016])
    ng<-ngram(b,n=2)
    print(get.phrasetable(ng)[1:3,1])
  }
  return(bg)
}

headlines <- read.csv("dataset/headlines.csv")
features <- read.csv("dataset/features.csv")

#########################################################################################3
# Calculate the PCA and get most relevant components
#Scaled
features.reduced <- prcomp(features, scale.=TRUE)
features.importance<-summary(features.reduced)$importance
#save(features.reduced, file="features.reduced.RData")
#save(features.importance, file="features.importance.RData")
load("features.reduced.RData")
load("features.importance.RData")

# Not Scaled
features.reduced.not_scaled <- prcomp(features)
features.importance.not_scaled<-summary(features.reduced.not_scaled)$importance
#save(features.reduced.not_scaled, file="features.reduced.not_scaled.RData")
load("features.reduced.not_scaled.RData")
#########################################################################################3

## Q1 Com quantas componentes principais conseguimos preservar 85% da variância
# dos dados? E 90%?
# Calculate based on scaled dataset
a<-features.importance[3,features.importance[3,]>0.85] # 1654 componentes possuem 85% da variancia
pc85<-a[1]
a<-features.importance[3,features.importance[3,]>0.90] # 1804 componentes possuem 90% da variancia
pc90<-a[1]

#define o cojunto de dados reduzido e calcula a matrix de distancias, para 1654 componentes (85%)
dataset<-features.reduced$x[,1:1654]
#d<- dist(dataset)
#save(d, file="distDataset.RData")
load("distDataset.RData")

# Calculate based on not scaled dataset
a<-features.importance.not_scaled[3,features.importance.not_scaled[3,]>0.85] #1390 componentes possuem 85% da variancia
pc85.not_scaled<-a[1]
a<-features.importance.not_scaled[3,features.importance.not_scaled[3,]>0.90] #1598 componentes possuem 90% da variancia
pc90.not_scaled<-a[1]

#define o conjunto de dados reduzido e calcula a matrix de distancias, para 1390 componentes (85%)
dataset.scale_false<-features.reduced.not_scaled$x[,1:1390]
d.scale_false<- dist(dataset.scale_false)
#save(d.scale_false, file="d.scale.false.RData")
load("d.scale.false.RData")

## Q2 - Efetue o agrupamento dos dados com o k-means e determine o número de clusters adequado.

## Q2(a) Faça isso comparando tanto o coeficiente de silhueta quanto o valor do erro quadrático do resultado obtido variando o k = {5, 10, 15, 20}.
#kmeans scaled
clusters.scaled <- calculateKmeans(dataset)
#save(clusters.scaled,file="clusters.scaled.RData")
load("clusters.scaled.RData")
#silhouette scaled
silhouete.scaled <- calculateSilhouete(clusters.scaled, d)
s<-c(0,0,0,0)
for(i in 1:4){
  s[i]<-summary(silhouete.scaled[[i]])$avg.width
}
png('silhueta.png')
plot(K,s,type='b',ylab="Coeficiente de silhueta",xlab="Número de clusters")
dev.off()

#kmeans not scaled
clusters.scaled.false <- calculateKmeans(dataset.scale_false)
#save(clusters.scaled.false,file="clusters.scaled.false.RData")
load("clusters.scaled.false.RData")
#silhouette not scaled
silhouete.scaled.false <- calculateSilhouete(clusters.scaled.false, d.scale_false)
s<-c(0,0,0,0)
for(i in 1:4){
  s[i]<-summary(silhouete.scaled.false[[i]])$avg.width
}
png('silhueta.scaled.false.png')
plot(K,s,type='b',ylab="Coeficiente de silhueta",xlab="Número de clusters")
dev.off()

## Q2(b) Como o uso de normalização (parâmetro scale do prcomp) antes de efetuar o PCA afeta os resultados?


## Q2(c) Explore duas variações do k-means. Por exemplo, k-medians, k-medoids, fuzzy c-means.
##DUVIDA: fazer com median e medoid pro no escalado??
# kmedian
clusters.scaled.kmedian <- calculateKmedian(dataset)
# fuzzy
clusters.scaled.fuzzy <- calculateFuzzy(dataset)

# DUVIDA: tem que fazer o kmedian e kmedoid somente para os dados 
# escalados e normalizados ou também nos não escalado e não normalizado ???
# kmedian
clusters.scaled.false.kmedian <- calculateKmedian(dataset.scale_false)
# fuzzy
clusters.scaled.false.fuzzy <- calculateFuzzy(dataset.scale_false)


## Q3 Análise de bigramas
#scale true
bg.scaled<-getBigram(clusters.scaled);

#scale false
bg.scale.falsed<-getBigram(clusters.scaled.false);

## Q3  Analise os clusters calculando os bigramas1 (subsequência contínua de duas palavras) mais frequentes de cada
# cluster
# Q3(a) Quais são os 3 bigramas mais frequentes de cada um?
# Q3(b) O que eles dizem sobre o tema das notícias dos seus clusters?

## Q4 Utilizando os dados com a dimensionalidade reduzida, efetue a mesma análise do item 3 apenas para notícias
# de 2016.
# Q4(a) O número de clusters utilizado é o mais adequado?
# Q4(b) Existem temas recorrentes que surgem tanto na análise com os dados completos quanto na análise deste
# ano isoladamente?
#scale true
bg2016.scale <- getBigram2016(clusters.scaled)

#scale false
bg2016.scale.false <- getBigram2016(clusters.scaled.false)
