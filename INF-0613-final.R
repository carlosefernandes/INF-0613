rm(list=ls())
setwd("/Carlos/ComplexData/INF-0613/Trabalho Final/final")
headlines <- read.csv("dataset/headlines.csv")
features <- read.csv("dataset/features.csv")

features.reduced <- prcomp(features, scale.=TRUE)
summary(features.reduced)





