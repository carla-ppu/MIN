######Paquetes#########
#Reglas de asociación
if (!require(arules)) {
  install.packages("arules",
                   dependencies = TRUE)}
if (!require(arulesViz)){
  install.packages("arulesViz",dependencies = TRUE)}
if (!require(ggplot2)) {
  install.packages("ggplot2",
                   dependencies = TRUE)}
if (!require(RColorBrewer)){
  install.packages("RColorBrewer",dependencies = TRUE)}
if (!require(tidyverse)) {
  install.packages("tidyverse",
                   dependencies = TRUE)}

######Librerias###########
#Clustering y gráficas
library("FactoMineR")
library("factoextra")
library("cluster")
library("dplyr")
library("fpc")
library("ggplot2")
library("RColorBrewer")
library("tidyverse")
#Reglas de asociación
library("arules")
library("arulesViz")