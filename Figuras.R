
#Figura 5: Modelo de una capa generado

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


# Tabla 2: Eficiencia en función del tamaño de muestra
set.seed(2021)
Eficiencia(radio=100, capa=CAPA100F5)

Eficiencia(radio=200, capa=CAPA200F5)

Eficiencia(radio=300, capa=CAPA300F5)

Eficiencia(radio=400, capa=CAPA400F5)

Eficiencia(radio=500, capa=CAPA500F5)

Eficiencia(radio=600, capa=CAPA600F5)

Eficiencia(radio=700, capa=CAPA700F5)

Eficiencia(radio=800, capa=CAPA800F5)


#Figura 11:
Figura11()

# Figura 12: Estimaciones dinámicas de la eficiencia del filtro para diferentes tamaños
#de partícula.

# Tabla 3: Varianzas máximas para la eficiencia por tamaño 
# de partículas modelo de una capa

# OBS: Esta función con los datos de los experimentos del artículo
# tarda aproximadamente 20 minutos en correr.

eficienciaUnacapa()

# Figura 13: Partículas capturadas en la superficie del filtro

capturas1capa()

#Figura 14: Estimaciones dinámicas de la eficiencia del filtro 
#para diferentes tamaños de partículas.

#Tabla 4: Varianzas máximas para la eficiencia por tamaño de partículas
#modelo multicapa.

# OBS: Esta función con los datos de los experimentos del artículo
# tarda aproximadamente 80 minutos en correr.

eficienciaMulticapa()






