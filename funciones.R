# Cargando capas generadas
{
CAPA100F5 <- read.csv("datos/CAPA100F5.csv")
CAPA200F5 <- read.csv("datos/CAPA200F5.csv")
CAPA300F5 <- read.csv("datos/CAPA300F5.csv")
CAPA400F5 <- read.csv("datos/CAPA400F5.csv")
CAPA500F5 <- read.csv("datos/CAPA500F5.csv")
CAPA600F5 <- read.csv("datos/CAPA600F5.csv")
CAPA700F5 <- read.csv("datos/CAPA700F5.csv")
CAPA800F5 <- read.csv("datos/CAPA800F5.csv")
CAPA800F5_2 <- read.csv("datos/CAPA800F5_2.csv")
}

coulterporometerdata<- read.csv("datos/coulterporometerdata.csv")

#Cargando distribución de particulas contaminantes
{
  porosTT<-read.csv("datos/poros.dat",header = FALSE)
  distribucionPa<-porosTT[30:50,]
  
  F5<-porosTT[1:29,]
  F5$V2<-F5$V2/100
  F5$V2<-1-F5$V2
  F5$V1<-F5$V1/2
  F5<-F5[order(F5$V1), ]
  
  
  distribucionPa$V2<-distribucionPa$V2/100
  distribucionPa$V2[21] <- 1
}

# Función para graficar las capas 
graficarcapaC<-function(radio,capa, titulo){
  
  plot(NULL, xlim=c(0,2*radio), 
       ylim=c(0,2*radio), main=titulo,asp=1,
       xlab="",ylab="",axes=F,
       yaxt="n",xaxt="n",cex.main = 3)
  
  mtext(TeX("$\\mu m$"), side=1, line=4, cex=3)
  
  axis(2,cex.axis=2)
  axis(1,cex.axis=2)
  draw.circle(x=radio,y=radio,radius=radio,col="black") # capa grande
  
  for(i in 1:(dim(capa)[1])){
    if(capa$estado[i]==1){
      draw.circle(capa$x[i],capa$y[i],radius=capa$r[i],border="black",col="white")
    }
  }
}

# Función para graficar las particulas capturadas
graficarParticulas<- function(color, particulas){
  for(i in 1: dim(particulas)[1]){
    draw.circle(x=particulas[i,3],y=particulas[i,4],
                r=particulas[i,1]/2,
                col=color)
  }
}

# Función que genera la muestra de partículas contaminantes
GenerarMuestra<-function(distribucion,n){
  u<-runif(n,min = 0,max = 1)
  muestra<-rep(0,n)
  #print(u)
  for(i in 1:n){
    for(j in 1:(dim(distribucion)[1]-1)){
      if(u[i] > distribucion[,2][j] & u[i]<=distribucion[,2][j+1] ){
        muestra[i]<-distribucion[,1][j]
        break
      }
    }
  }
  return(muestra)
}

# Función para lanzar las partículas
lanzarparticulasCC2<-function(capa,radio,muestra,numCAP){
  #numCAP número de capa sobre la cual es lanzada la part
  #graficarcapaC(radio,capa)
  # rojo capturadas
  # azul libres
  conteo<-0 # contador de particulas
  # capa<-capa[order(capa$r,decreasing = T),]
  capt<-muestra
  # 1 la particula o poro esta libre
  # 0 la particula esta capturada, el poro esta bloqueado
  
  muestra<-muestra[order(muestra[,2],decreasing = T),]# colocar las part libres primero
  NoCap<-sum(muestra[,2]) # numero de particulas no capturadas
  
  for(j in 1:NoCap){
    # generar posicion aleatoria sobre capa y definir region de busqueda
    {
      
      repeat{ 
        x<- runif(1,0,2*radio) 
        y<- runif(1,0,2*radio) 
        if(!((x-radio)^2 + (y-radio)^2 > (radio)^2)){ break } # Negation is crucial here!
      }

      rrr1<-x + 2*max(capa$r)
      rrr2<-x - 2*max(capa$r)
      rrr3<-y + 2*max(capa$r)
      rrr4<-y - 2*max(capa$r)
      # subconjunto de busqueda
      
      sub<-subset(capa, capa$x <= rrr1 & capa$x >= rrr2 & capa$y <= rrr3 & capa$y >= rrr4)
      kk<-dim(sub)[1] # cantidad de poros en la región de busqueda
    }
    if(kk==0){ # no hay ningun poro cerca, la particula es capturada
      #points(x=x,y=y,col ="red",pch=19,cex=0.20)
      muestra[j,2]<-0
      muestra[j,3]<-x
      muestra[j,4]<-y
      muestra[j,5]<-numCAP
    }else{ # buscar si la particula cae en un poro o no
      cc<-0 # conteo de poros en que la particula no esta
      for(i in 1:kk){ 
        dC<-sqrt( (sub$x[i]-x)^2 + (sub$y[i]-y)^2 ) # distancia entre el centro del poro y el punto
        if(dC < sub$r[i]){ # la particula esta adentro del poro
          if(sub$estado[i]==0){# el poro está obstruido, la particula queda capturada
            #points(x=x,y=y,col ="red",pch=19,cex=0.20)
            muestra[j,2]<-0
            muestra[j,3]<-x
            muestra[j,4]<-y
            muestra[j,5]<-numCAP
            break
          }else{ # el poro esta libre
            
            if(muestra[j,1] >= 2*sub$r[i]){ # la particula es mas grande que el poro, particula capturada
              #points(x=x,y=y,col ="red",pch=19,cex=0.20)
              muestra[j,2]<-0
              muestra[j,3]<-x
              muestra[j,4]<-y
              muestra[j,5]<-numCAP
              iii<-as.numeric(rownames(sub[i,]))
              capa$estado[iii]<-0
              conteo<-conteo+1
              #cat(conteo,"indice que cambia", iii,"\n")
              break
            }else{ # la particula es libre
              #points(x=x,y=y,col ="blue",pch=19,cex=0.20)
              break
            } 
            
          }# fin de else del if(sub$estado[i]==0)
          
          
        }else{ # la particula esta afuera del poro aumentar el conteo de poros revisados
          cc<-cc+1
        }
      }# fin del for que recorre los poros del subset
      
      if(cc==kk){ # esta afuera de todos los poros de la busqueda, queda capturado
        #points(x=x,y=y,col ="red",pch=19,cex=0.20)
        muestra[j,2]<-0 
        muestra[j,3]<-x
        muestra[j,4]<-y
        muestra[j,5]<-numCAP
      }
    }# fin de else de if(kk==0)
    
    
  }# fin del for que recorre particulas no capturadas
  
  porosidad<-sum(pi*capa$r^2*capa$estado)/(pi*radio^2) # cambio de la porosidad
  eff<-(sum(muestra[,2])/NoCap)*100 # porcentaje de particulas no capturadas
  retorno<-list(muestra,capa,100-eff,NoCap,porosidad)
  return(retorno)
  
} 

#Eficiencia en función del tamaño de muestra
Eficiencia <- function(numero_simulaciones=50, radio, capa){
  area <- pi*radio^2*1e-12
  NUM   <- ceiling( 1.7519e9*0.10*5*area)
  vectorEF <- c()
  for(i in 1: numero_simulaciones ){
    tamano<-GenerarMuestra(distribucionPa,NUM)
    estado<-rep(1,NUM)
    capaCapt<- rep(0,NUM)
    coorx<-rep(0,NUM) # coordenada en x de la captura
    coory<-rep(0,NUM) # coordenada en y de la captura
    particulas<-cbind(tamano,estado,coorx,coory,capaCapt)
    
    resultado <- lanzarparticulasCC2(capa, radio, particulas, 0)
    
    vectorEF[i] <- resultado[[3]]/100
  }
  S2<- var(vectorEF)
  z <- qnorm(0.025,lower.tail = F)
  ell <- mean(vectorEF)
  
  LI <- round(ell - z*sqrt(S2/numero_simulaciones),4)
  LS <- round(ell + z*sqrt(S2/numero_simulaciones),4)
  cat("Radio de la capa: ", radio,"\nNúmero de partículas lanzadas: ",NUM)
  cat("\nEficiencia: ", round(ell,4),"\nVarianza: ",round(S2,5),
      "\nIntervalo de confianza: ",LI,LS )
  
  invisible(c(ell, LI, LS))
  
}

# Función que genera la figura 11 
Figura11 <- function(){
  radiosT <- c(TeX("Modelo radio 100 ($\\mu m$)"),
               TeX("Modelo radio 200 ($\\mu m$)"),
               TeX("Modelo radio 300 ($\\mu m$)"),
               TeX("Modelo radio 400 ($\\mu m$)"),
               TeX("Modelo radio 500 ($\\mu m$)"),
               TeX("Modelo radio 600 ($\\mu m$)"),
               TeX("Modelo radio 700 ($\\mu m$)"),
               TeX("Modelo radio 800 ($\\mu m$)")
  )
  colores <- rainbow(10)
  {
    plot(density(coulterporometerdata$x,bw=0.29), col=colores[1],
         main = "",axes=F,lwd=3,
         xlab = "",
         ylab = "",
         yaxt="n",xaxt="n")
    mtext(TeX("Radio de poros ($\\mu m$)"), side=1, line=4, cex=3)
    
    axis(2,cex.axis=2)
    axis(1,cex.axis=2)
    
    
    lines(density(CAPA100F5$r), ylim=c(0,0.24),
          col=colores[2], lwd=3)
    
    
    lines(density(CAPA200F5$r),
          col=colores[3],lwd=3)
    
    lines(density(CAPA300F5$r),
          col=colores[4],lwd=3)
    
    lines(density(CAPA400F5$r),
          col=colores[5],lwd=3)
    
    lines(density(CAPA500F5$r),
          col=colores[6],lwd=3)
    
    lines(density(CAPA600F5$r),
          col=colores[7],lwd=3)
    
    lines(density(CAPA700F5$r,bw=0.29),
          col=colores[8],lwd=3)
    
    
    lines(density(CAPA800F5$r),
          col=colores[9],lwd=3)
    
    legend("topleft",bty = "n",cex=1,
           legend = c("Experimental Coulter Porometer",
                      radiosT[1:4]),
           lty = 1, col = colores[1:5], lwd = 3)
    
    legend("topright",bty = "n",cex=1,
           legend = radiosT[5:8],
           lty = 1, col = colores[6:9], lwd = 3)
  }
  
}

# funciónes  que calcula la eficiencia dinámica
grafico_dinamico <- function(simulacion,tampar, tiempo_global){
  eff<-c()
  for(i in 1:tiempo_global){
    
    num <-sum(simulacion[[2]][[i]][,1] > tampar & 
                simulacion[[2]][[i]][,2]==0)
    
    den <- sum(simulacion[[1]][[i]][,1]>tampar)
    
    eff[i]<-num/den
  }
  return(eff) 
}

lanzamiento_dinamico <- function(capa,radio,tiempo_global, deltaT){
  capaX <- capa
  capturas_por_min   <- list()
  muestras_iniciales <- list()
  # tiempo global en minutos
  # calcular el número de particulas por minuto
  area <- pi*radio^2*1e-12
  NUM_particulas <-ceiling(1.7519e9*0.10*deltaT*area)
  
  for(i in 1: tiempo_global){
    # generar tamaño de muestra de particulas por minuto
    ##################################################################
    tamano<-GenerarMuestra(distribucionPa,NUM_particulas)
    estado<-rep(1,NUM_particulas)
    capaCapt<- rep(0,NUM_particulas)
    coorx<-rep(0,NUM_particulas) # coordenada en x de la captura
    coory<-rep(0,NUM_particulas) # coordenada en y de la captura
    muestras_iniciales[[i]]<-cbind(tamano,estado,coorx,coory,capaCapt)
    ##################################################################
    sim<-lanzarparticulasCC2(capaX,radio,muestras_iniciales[[i]],0)
    capaX <- sim[[2]]
    # particulas capturadas
    capturas_por_min[[i]]<-sim[[1]]
  }
  return(list(muestras_iniciales,capturas_por_min))
}

lanzamiento_dinamico_multicapa <- function(capa1,capa2,capa3,radio, 
                                           tiempo_global, deltaT){
  capa1X <- capa1
  capa2X <- capa2
  capa3X <- capa3
  
  capturas_por_min   <- list() # capturas por deltaT
  muestras_iniciales <- list() # muestra de particulas iniciales
  
  # tiempo global en deltaT minutos
  # calcular el número de particulas por deltaT minutos
  
  area <- pi*radio^2*1e-12
  NUM_particulas <-ceiling(1.7519e9*0.10*deltaT*area)
  
  for(i in 1: tiempo_global){
    # generar tamaño de muestra de particulas por minuto
    ######################################################
    tamano<-GenerarMuestra(distribucionPa,NUM_particulas)
    estado<-rep(1,NUM_particulas)
    capaCapt<- rep(0,NUM_particulas)
    coorx<-rep(0,NUM_particulas) # coordenada en x de la captura
    coory<-rep(0,NUM_particulas) # coordenada en y de la captura
    muestras_iniciales[[i]]<-cbind(tamano,estado,coorx,coory,capaCapt)
    ######################################################
    
    sim<-lanzarparticulasCC2(capa1X,radio,muestras_iniciales[[i]],1)
    capa1X <- sim[[2]] # guardo la capa con memoria de poros capturados
    
    sim<-lanzarparticulasCC2(capa2X,radio,sim[[1]],2)
    capa2X <- sim[[2]] # guardo la capa con memoria de poros capturados
    
    sim<-lanzarparticulasCC2(capa3X,radio,sim[[1]],3)
    capa3X <- sim[[2]] # guardo la capa con memoria de poros capturados
    
    capturas_por_min[[i]]<-sim[[1]]
    
  }
  return(list(muestras_iniciales,capturas_por_min))
}


eficienciaUnacapa <- function(num_simulaciones=50, deltaT=5, tiempo_global=24){
  set.seed(2021)
  p1_una <- matrix(NA,nrow = num_simulaciones,
                   ncol = tiempo_global)
  p5_una <- matrix(NA,nrow = num_simulaciones,
                   ncol = tiempo_global)
  p10_una <- matrix(NA,nrow = num_simulaciones,
                    ncol = tiempo_global)
  p15_una <- matrix(NA,nrow = num_simulaciones,
                    ncol = tiempo_global)
  for( i in 1:num_simulaciones){
    simulacion_una <- lanzamiento_dinamico(CAPA800F5,800,tiempo_global,deltaT)
    p1_una[i,]  <- grafico_dinamico(simulacion_una, 1, tiempo_global)
    p5_una[i,]  <- grafico_dinamico(simulacion_una, 5, tiempo_global)
    p10_una[i,] <- grafico_dinamico(simulacion_una, 10, tiempo_global)
    p15_una[i,] <- grafico_dinamico(simulacion_una, 15, tiempo_global)
  }
  
  plot(5*c(1:24),colMeans(p15_una), type = "b", pch=1, 
       lwd=3,main="", axes=F,
       xlab = "",
       ylab = "",
       ylim=c(0.15,1.15) )
  
  lines(5*c(1:24), colMeans(p10_una), type = "b", pch=2, lwd=3)
  lines(5*c(1:24), colMeans(p5_una), type = "b", pch=3,lwd=3)
  lines(5*c(1:24), colMeans(p1_una), type = "b", pch=4,lwd=3)
  
  axis(2,cex.axis=2)
  axis(1,cex.axis=2)
  
  
  diametros <- c(TeX("D_p  > 1 ($\\mu m$)"),
                 TeX("D_p  > 5 ($\\mu m$)"),
                 TeX("D_p  > 10 ($\\mu m$)"),
                 TeX("D_p  > 15 ($\\mu m$)")) 
  
  legend(x = "topleft",legend = diametros[1:2], cex=1.5,lty =0,
         lwd=3, xpd = TRUE,pch = c(4,3),bty = "n")
  
  legend(x = "topright", legend = diametros[3:4], cex=1.5, lty =0,
         lwd=3, xpd = TRUE, pch = c(2,1), bty = "n")
  
  mtext("Tiempo (Min)", side=1, line=2.5, cex=2)
  mtext("Eficiencia %", side=2, line=2.5, cex=2)
  
  t1 <- round(max(colVars(p1_una)),5)
  t2 <- round(max(colVars(p5_una)),5)
  t3 <- round(max(colVars(p10_una)),5)
  t4 <- round(max(colVars(p15_una)),5)
  
  cat("Varianzas máximas para la eficiencia por tamaño de partículas modelo de una capa",
      "\nDp>1μm: ",t1, "\nDp>5μm: ",t2,"\nDp>10μm: ", t3,"\nDp>15μm: ",t4)
}

eficienciaMulticapa <- function(num_simulaciones=50, deltaT=5, tiempo_global=24){
  set.seed(2021)
  p1_multi <- matrix(NA,nrow = num_simulaciones,
                     ncol = tiempo_global)
  
  p5_multi <- matrix(NA,nrow = num_simulaciones,
                     ncol = tiempo_global)
  
  p10_multi <- matrix(NA,nrow = num_simulaciones,
                      ncol = tiempo_global)
  
  p15_multi <- matrix(NA,nrow = num_simulaciones,
                      ncol = tiempo_global)
  
  for( i in 1:num_simulaciones){
    simulacion_multi <- lanzamiento_dinamico_multicapa(CAPA800F5,CAPA800F5_2,CAPA800F5,
                                                       800,tiempo_global,deltaT)
    
    p1_multi[i,]  <- grafico_dinamico(simulacion_multi, 1, tiempo_global)
    p5_multi[i,]  <- grafico_dinamico(simulacion_multi, 5, tiempo_global)
    p10_multi[i,] <- grafico_dinamico(simulacion_multi, 10, tiempo_global)
    p15_multi[i,] <- grafico_dinamico(simulacion_multi, 15, tiempo_global)
    
  }
  
  plot(5*c(1:24),colMeans(p15_multi), type = "b", 
       pch=1, lwd=3,main="", axes=F, xlab = "", 
       ylab = "", ylim=c(0.15,1.15) )
  
  lines(5*c(1:24), colMeans(p10_multi), type = "b", pch=2, lwd=3)
  lines(5*c(1:24), colMeans(p5_multi), type = "b", pch=3,lwd=3)
  lines(5*c(1:24), colMeans(p1_multi), type = "b", pch=4,lwd=3)
  
  axis(2,cex.axis=2)
  axis(1,cex.axis=2)
  
  
  diametros <- c(TeX("D_p  > 1 ($\\mu m$)"),
                 TeX("D_p  > 5 ($\\mu m$)"),
                 TeX("D_p  > 10 ($\\mu m$)"),
                 TeX("D_p  > 15 ($\\mu m$)"))
  
  legend(x = "bottom", legend = diametros[1:2], 
         cex=1.5,lty =0,lwd=3, xpd = TRUE,
         pch = c(4,3), bty = "n")
  
  legend(x = "bottomright", legend = diametros[3:4], 
         cex=1.5, lty =0, lwd=3, xpd = TRUE,
         pch = c(2,1), bty = "n")
  
  mtext("Tiempo (Min)", side=1, line=2.5, cex=2)
  mtext("Eficiencia %", side=2, line=2.5, cex=2)
  
  
  t1 <- round(max(colVars(p1_multi)),5)
  t2 <- round(max(colVars(p5_multi)),5)
  t3 <- round(max(colVars(p10_multi)),5)
  t4 <- round(max(colVars(p15_multi)),5)
  
  cat("Varianzas máximas para la eficiencia por tamaño de partículas modelo de una capa",
      "\nDp>1μm: ",t1, "\nDp>5μm: ",t2,"\nDp>10μm: ", t3,"\nDp>15μm: ",t4)
}


capturas1capa <- function(){
  simulacion_multi <- lanzamiento_dinamico_multicapa(CAPA800F5,CAPA800F5_2,CAPA800F5,
                                                     800,tiempo_global=24,deltaT=5)
  
  graficarcapaC(800,CAPA800F5,"")
  ccc <- rep(c("red", "yellow", "blue"), c(8,8,8))
  
  for(i in 1:24){
    temp <- subset(simulacion_multi[[2]][[i]], 
                   simulacion_multi[[2]][[i]][,5]==1)
    graficarParticulas(ccc[i], temp)
    invisible(readline(prompt = "Presioná Enter para mostrar los proximos 5 minutos:"))
  }
  
  
}







  
  
  
  