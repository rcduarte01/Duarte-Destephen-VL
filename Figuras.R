#Figura 5: Modelo de una capa generado
CAPA100F5 <- read.csv("datos/CAPA100F5.csv")
CAPA200F5 <- read.csv("datos/CAPA200F5.csv")
CAPA300F5 <- read.csv("datos/CAPA300F5.csv")
CAPA400F5 <- read.csv("datos/CAPA400F5.csv")
CAPA500F5 <- read.csv("datos/CAPA500F5.csv")
CAPA600F5 <- read.csv("datos/CAPA600F5.csv")
CAPA700F5 <- read.csv("datos/CAPA700F5.csv")
CAPA800F5 <- read.csv("datos/CAPA800F5.csv")

graficarcapaC(200,CAPA200F5,TeX("Capa de radio 200 $\\mu m$"))

#Figura 8: Tamaños de muestra de radios 100, 200, 300 y 400 μm


par(mfrow=c(2,2))
graficarcapaC(100,CAPA100F5,TeX("Capa de radio 100 $\\mu m$"))
graficarcapaC(200,CAPA200F5,TeX("Capa de radio 200 $\\mu m$"))
graficarcapaC(300,CAPA300F5,TeX("Capa de radio 300 $\\mu m$"))
graficarcapaC(400,CAPA400F5,TeX("Capa de radio 400 $\\mu m$"))
par(mfrow=c(1,1))

# Figura 9: Tamaños de muestra de radios 500, 600, 700 y 800 μm
par(mfrow=c(2,2))
graficarcapaC(500,CAPA500F5,TeX("Capa de radio 500 $\\mu m$"))
graficarcapaC(600,CAPA600F5,TeX("Capa de radio 600 $\\mu m$"))
graficarcapaC(700,CAPA700F5,TeX("Capa de radio 700 $\\mu m$"))
graficarcapaC(800,CAPA800F5,TeX("Capa de radio 800 $\\mu m$"))
par(mfrow=c(1,1))








