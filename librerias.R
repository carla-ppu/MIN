######Paquetes#########
#Reglas de asociaci�n
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
#Clustering y gr�ficas
library("FactoMineR")
library("factoextra")
library("cluster")
library("dplyr")
library("fpc")
library("ggplot2")
library("RColorBrewer")
library("tidyverse")
#Reglas de asociaci�n
library("arules")
library("arulesViz")