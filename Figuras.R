#Figura 5: Modelo de una capa generado
library(latex2exp)
library(plotrix)
CAPA200F5 <- read.csv("datos/CAPA200F5.csv")

graficarcapaC(200,CAPA200F5,TeX("Capa de radio 200 $\\mu m$"))
