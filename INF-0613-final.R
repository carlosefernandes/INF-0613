########################################
# Trabalho Final - INF-0613
# Nome(s): Carlos Fernandes e Liselene de Abreu Borges
########################################

rm(list=ls())
#setwd("/Carlos/ComplexData/INF-0613/Trabalho Final")
#setwd("~/Projects/ComplexData/trabalho/INF-0613")

library(MASS)
library(cluster)
library(ngram)
library(stringr)
require(ggplot2)

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

#bigrams
getBigram <- function(dataset,hl){
  bg <- list()
  for (line in 1:K[3]){ #para K=15 é o 3 
    b <- concatenate(hl[(dataset[[3]]$cluster==line),2])
    ng<-ngram(b,n=2)
    bg[[line]]<-get.phrasetable(ng)[1:3,1] #pega os 3 bigramas mais frequentes e concatenar para todos os clusters
  }
  return(bg)
}

getBigram2016 <- function(dataset,hl){
  indexA<-(str_detect(hl$publish_date,"2016"))
  bg <- list()
  for (line in 1:K[3]){ #para K=15
    indexC<-(dataset[[3]]$cluster==line)
    b <- concatenate(hl$headline_text[(indexA & indexC)])
    ng<-ngram(b,n=2)
    bg[[line]]<-get.phrasetable(ng)[1:3,1]
  }
  return(bg)
}

graficos2 <- function(data1, data2, type1, type2, ylabel,t){
  df1 <- data.frame(x=K,y=data1,type=type2)
  df2 <- data.frame(x=K,y=data2,type=type1)
  df <- rbind(df1,df2)
  ggplot(data=df, aes(x = K, y = value, colour = legenda))+
    geom_point(aes(x,y,colour=type))+
    geom_line(aes(x,y,colour=type))+
    ylab(ylabel)+
    ggtitle(t)
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
s1<-c(0,0,0,0)
for(i in 1:4){
  s1[i]<-summary(silhouete.scaled[[i]])$avg.width
}

#kmeans not scaled
clusters.scaled.false <- calculateKmeans(dataset.scale_false)
#save(clusters.scaled.false,file="clusters.scaled.false.RData")
load("clusters.scaled.false.RData")
#silhouette not scaled
silhouete.scaled.false <- calculateSilhouete(clusters.scaled.false, d.scale_false)
s2<-c(0,0,0,0)
for(i in 1:4){
  s2[i]<-summary(silhouete.scaled.false[[i]])$avg.width
}

#err.norm<-c(46202326,46039547,45893505,45774086)
#err.norm.max<-max(err.norm)
#err.norm<-err.norm/err.norm.max
#err.raw<-c(20365.38,20125.02,19920.57,19823.53)
#err.raw.max<-max(err.raw)
#err.raw<-err.raw/err.raw.max
#grafico2(K,s1,s2,"sil.norm","sil.raw","Coeficiente de silhueta");

grafico2(K,s1,s2,"sil.norm","sil.raw","Coeficiente de silhueta");

## Q2(b) Como o uso de normalização (parâmetro scale do prcomp) antes de efetuar o PCA afeta os resultados?

## Q2(c) Explore duas variações do k-means. Por exemplo, k-medians, k-medoids, fuzzy c-means.
# scaled 
# kmedian
clusters.scaled.kmedian <- calculateKmedian(dataset)
# fuzzy
clusters.scaled.fuzzy <- calculateFuzzy(dataset)

# scaled false
# kmedian
clusters.scaled.false.kmedian <- calculateKmedian(dataset.scale_false)
# fuzzy
clusters.scaled.false.fuzzy <- calculateFuzzy(dataset.scale_false)

#graficos
#kn.max<-max(kmedian.morm)
#kmedian.morm<-kmedian.morm/kn.max
#kr.max<-max(kmedian.raw)
#kmedian.raw<-kmedian.raw/fr.max
#graficos2(kmedian.morm,kmedian.raw,paste("morm*",kn.max),
#          paste("row*",kr.max),"Sum of within cluster distances","kmedian")

#fn.max<-max(fuzzy.norm)
#fuzzy.norm<-fuzzy.norm/fn.max
#fr.max<-max(fuzzy.raw)
#fuzzy.raw<-fuzzy.raw/fr.max
#graficos2(fuzzy.norm,fuzzy.raw,paste("morm*",fn.max),
#          paste("row*",fr.max),"withinerror","fuzzy")


## Q3  Analise os clusters calculando os bigramas1 (subsequência contínua de duas palavras) mais frequentes de cada
# cluster
# Q3(a) Quais são os 3 bigramas mais frequentes de cada um?
# Q3(b) O que eles dizem sobre o tema das notícias dos seus clusters?
#scale true
bg.scaled<-getBigram(clusters.scaled,headlines);
#scale false
bg.scale.falsed<-getBigram(clusters.scaled.false,headlines);

## Q4 Utilizando os dados com a dimensionalidade reduzida, efetue a mesma análise do item 3 apenas para notícias
# de 2016.
# Q4(a) O número de clusters utilizado é o mais adequado?
# Q4(b) Existem temas recorrentes que surgem tanto na análise com os dados completos quanto na análise deste
# ano isoladamente?
#scale true
bg2016.scale <- getBigram2016(clusters.scaled,headlines)

#scale false
bg2016.scale.false <- getBigram2016(clusters.scaled.false,headlines)
