########################################
# Trabalho Final - INF-0613
# Nome(s): Carlos Fernandes e Liselene de Abreu Borges
########################################

rm(list=ls())
setwd("/Carlos/ComplexData/INF-0613/Trabalho Final")
#setwd("~/Projects/ComplexData/trabalho/INF-0613")
#setwd("~/")

library(MASS)
library(cluster)

calculateSilhouete <- function(clusters_, distance) {
  K <- c(5,10,15,20)
  x <- 1
  silhouetes <- list()
  for (k in K) {
    print(k)
    set.seed(1234)
    silhouetes[[x]] <- silhouette(clusters_[[x]]$cluster, distance)
    #print(means.scaled[[x]]$totss)
    x <- x + 1
  }
}


#kmeans
calculateKmeans <- function(dataset) {
  K <- c(5,10,15,20)
  x <- 1
  means <- list()
  for (k in K) {
    print(k)
    set.seed(1234)
    means[[x]] <- kmeans(dataset,k, nstart = 20)
    #print(means.scaled[[x]]$totss)
    x <- x + 1
  }
  return (means)
}

#kmedian
library(flexclust)
calculateKmedian <- function(dataset) {
  K <- c(5,10,15,20)
  x <- 1
  means <- list()
  for (k in K) {
    print(k)
    set.seed(1234)
    means[[x]]=kcca(dataset,k)
    x <- x + 1
  }
  return (means)
}

#kmedoid
library(cluster)
calculateKmedoid <- function(dataset) {
  K <- c(5,10,15,20)
  x <- 1
  means <- list()
  for (k in K) {
    print(k)
    set.seed(1234)
    means[[x]]=pam(dataset,k)
    x <- x + 1
  }
  return (means)
}

headlines <- read.csv("dataset/headlines.csv")
features <- read.csv("dataset/features.csv")

#########################################################################################3
# Calculate the PCA and get most relevant components
#Scaled
features.reduced <- prcomp(features, scale.=TRUE)
features.importance<-summary(features.reduced)$importance

# Not Scaled
features.reduced.not_scaled <- prcomp(features)
features.importance.not_scaled<-summary(features.reduced.not_scaled)$importance
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
d<- dist(dataset)

# Calculate based on not scaled dataset
a<-features.importance.not_scaled[3,features.importance.not_scaled[3,]>0.85] #1390 componentes possuem 85% da variancia
pc85.not_scaled<-a[1]
a<-features.importance.not_scaled[3,features.importance.not_scaled[3,]>0.90] #1598 componentes possuem 90% da variancia
pc90.not_scaled<-a[1]

#define o conjunto de dados reduzido e calcula a matrix de distancias, para 1390 componentes (85%)
dataset.scale_false<-features.reduced.not_scaled$x[,1:1390]
d.scale_false<- dist(dataset.scale_false)

## Q2 - Efetue o agrupamento dos dados com o k-means e determine o número de clusters adequado.

## Q2(a) Faça isso comparando tanto o coeficiente de silhueta quanto o valor do erro quadrático do resultado obtido variando o k = {5, 10, 15, 20}.
#kmeans scaled
clusters.scaled <- calculateKmeans(dataset)
#silhouette scaled
silhouete.scaled <- calculateSilhouete(clusters.scaled, d)

#kmeans not scaled
clusters.scaled.false <- calculateKmeans(dataset.scale_false)
#silhouette not scaled
silhouete.scaled.false <- calculateSilhouete(clusters.scaled.false, d.scale_false)

## Q2(b) Como o uso de normalização (parâmetro scale do prcomp) antes de efetuar o PCA afeta os resultados?


## Q2(c) Explore duas variações do k-means. Por exemplo, k-medians, k-medoids, fuzzy c-means.
##DUVIDA: fazer com median e medoid pro no escalado??
# kmedian
clusters.scaled.kmedian <- calculateKmedian(dataset)
# kmedoid
clusters.scaled.kmedoid <- calculateKmedoid(dataset)

# DUVIDA: tem que fazer o kmedian e kmedoid somente para os dados 
# escalados e normalizados ou também nos não escalado e não normalizado ???
# kmedian
clusters.scaled.false.kmedian <- calculateKmedian(dataset.scale_false)
# kmedoid
clusters.scaled.false.kmedoid <- calculateKmedoid(dataset.scale_false)

## Q3 Análise de bigramas
package("NLP")
#s <- "The quick brown fox jumps over the lazy dog"
# DUVIDA: qual vai ser a entrada s, a matriz??
w <- strsplit(s, " ", fixed = TRUE)[[1L]] # Split into words:
ngrams(w, 2L) # Word bi-grams
vapply(ngrams(w, 2L), paste, "", collapse = " ")# Word bi-grams pasted together

## Q3  Analise os clusters calculando os bigramas1 (subsequência contínua de duas palavras) mais frequentes de cada
# cluster
# Q3(a) Quais são os 3 bigramas mais frequentes de cada um?
# Q3(b) O que eles dizem sobre o tema das notícias dos seus clusters?

## Q4 Utilizando os dados com a dimensionalidade reduzida, efetue a mesma análise do item 3 apenas para notícias
# de 2016.
# Q4(a) O número de clusters utilizado é o mais adequado?
# Q4(b) Existem temas recorrentes que surgem tanto na análise com os dados completos quanto na análise deste
# ano isoladamente?
  
